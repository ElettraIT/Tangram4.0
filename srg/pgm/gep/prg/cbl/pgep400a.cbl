       Identification Division.
       Program-Id.                                 pgep400a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    glo                 *
      *                                   Fase:    gep400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 02/12/93    *
      *                       Ultima revisione:    NdK del 21/12/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Analisi di portafoglio globale              *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Situazione di portafoglio globale           *
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
      *            * Data scadenza                                     *
      *            *---------------------------------------------------*
               10  srt-dts-sdb            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero scadenza                                   *
      *            *---------------------------------------------------*
               10  srt-num-sdb            pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Tipo scadenza                                     *
      *            *---------------------------------------------------*
               10  srt-tip-sdb            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo debitore                                     *
      *            *---------------------------------------------------*
               10  srt-tip-dbt            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice debitore                                   *
      *            *---------------------------------------------------*
               10  srt-cod-dbt            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza debitore                        *
      *            *---------------------------------------------------*
               10  srt-dpz-dbt            pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Importo scadenza                                  *
      *            *---------------------------------------------------*
               10  srt-imp-sdb            pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Segnale Si/No dilazione concordata                *
      *            *---------------------------------------------------*
               10  srt-snx-dlc            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale Si/No contrassegno                        *
      *            *---------------------------------------------------*
               10  srt-snx-cts            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sigla ultima operazione eseguita sulla scadenza   *
      *            *---------------------------------------------------*
               10  srt-suo-sdb            pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Date ultimo sollecito                             *
      *            *---------------------------------------------------*
               10  srt-dso-l01            pic  9(07)                  .
               10  srt-dso-l02            pic  9(07)                  .
               10  srt-dso-l03            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero scadenza di origine                        *
      *            *---------------------------------------------------*
               10  srt-nsc-org            pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Flags di elaborazione                             *
      *            *---------------------------------------------------*
               10  srt-flg-ela.
                   15  srt-flg-blo.
                       20  srt-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  srt-flg-nbl.
                       20  srt-flg-nbx occurs 03
                                          pic  x(01)                  .

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
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

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
      *        * - 06 : Solo Dilazioni Concordate                      *
      *        * - 07 : Solo Incassi Elettronici                       *
      *        * - 08 : Solo Ri.Ba.                                    *
      *        * - 09 : Solo C.d.O.                                    *
      *        * - 10 : Solo M.Av.                                     *
      *        * - 11 : Solo R.I.D.                                    *
      *        * - 12 : Solo Bonifici Bancari                          *
      *        * - 13 : Solo C/C Postali                               *
      *        * - 14 : Solo Ricevute Bancarie                         *
      *        * - 15 : Solo Tratte                                    *
      *        * - 16 : Solo Paghero'                                  *
      *        *-------------------------------------------------------*
           05  rr-tip-scd                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Selezione su status scadenze                          *
      *        *                                                       *
      *        * - 01 : Sia a scadere che scadute                      *
      *        * - 02 : Solo quelle a scadere                          *
      *        * - 03 : Solo quelle scadute                            *
      *        *-------------------------------------------------------*
           05  rr-sts-scd                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Selezione su status effetti                           *
      *        *                                                       *
      *        * - 01 : Tutti                                          *
      *        * - 02 : Solo quelli in portafoglio                     *
      *        * - 03 : Solo quelli in circolazione                    *
      *        * - 04 : Solo quelli insoluti                           *
      *        *-------------------------------------------------------*
           05  rr-sts-eff                 pic  9(02)                  .
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

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipi scadenza da includere                 *
      *        *-------------------------------------------------------*
           05  w-exp-tip-scd.
               10  w-exp-tip-scd-num      pic  9(02)       value 16   .
               10  w-exp-tip-scd-lun      pic  9(02)       value 25   .
               10  w-exp-tip-scd-tbl.
                   15  filler             pic  x(25) value
                            "Tutte                    "               .
                   15  filler             pic  x(25) value
                            "RD+BB+CCP                "               .
                   15  filler             pic  x(25) value
                            "IE+RIBA+CDO+MAV+RID      "               .
                   15  filler             pic  x(25) value
                            "RB+TR+PC                 "               .
                   15  filler             pic  x(25) value
                            "Solo RD                  "               .
                   15  filler             pic  x(25) value
                            "Solo DC                  "               .
                   15  filler             pic  x(25) value
                            "Solo IE                  "               .
                   15  filler             pic  x(25) value
                            "Solo RIBA                "               .
                   15  filler             pic  x(25) value
                            "Solo CDO                 "               .
                   15  filler             pic  x(25) value
                            "Solo MAV                 "               .
                   15  filler             pic  x(25) value
                            "Solo RID                 "               .
                   15  filler             pic  x(25) value
                            "Solo BB                  "               .
                   15  filler             pic  x(25) value
                            "Solo CCP                 "               .
                   15  filler             pic  x(25) value
                            "Solo RB                  "               .
                   15  filler             pic  x(25) value
                            "Solo TR                  "               .
                   15  filler             pic  x(25) value
                            "Solo PC                  "               .
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su status scadenze               *
      *        *-------------------------------------------------------*
           05  w-exp-sts-scd.
               10  w-exp-sts-scd-num      pic  9(02)       value 03   .
               10  w-exp-sts-scd-lun      pic  9(02)       value 25   .
               10  w-exp-sts-scd-tbl.
                   15  filler             pic  x(25) value
                            "Sia a scadere che scadute"               .
                   15  filler             pic  x(25) value
                            "Solo quelle a scadere    "               .
                   15  filler             pic  x(25) value
                            "Solo quelle scadute      "               .
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su status effetti                *
      *        *-------------------------------------------------------*
           05  w-exp-sts-eff.
               10  w-exp-sts-eff-num      pic  9(02)       value 04   .
               10  w-exp-sts-eff-lun      pic  9(02)       value 25   .
               10  w-exp-sts-eff-tbl.
                   15  filler             pic  x(25) value
                            "Tutti                         "          .
                   15  filler             pic  x(25) value
                            "solo in Portafoglio      "          .
                   15  filler             pic  x(25) value
                            "solo in Circolazione     "          .
                   15  filler             pic  x(25) value
                            "solo Insoluti            "          .
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
      *                * Codice cliente                                *
      *                *-----------------------------------------------*
                   15  w-mpn-cod-002      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Dipendenza cliente                            *
      *                *-----------------------------------------------*
                   15  w-mpn-dpz-002      pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale cliente                       *
      *                *-----------------------------------------------*
                   15  w-mpn-rag-002      pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(15)                  .

      *    *===========================================================*
      *    * Work-area per ciclo di Report-Program                     *
      *    *-----------------------------------------------------------*
       01  w-prn.
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                               *
      *        *-------------------------------------------------------*
           05  w-prn-flg-uno              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per routine qry-trt-fun-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-qry-trt-fun.
           05  w-qry-trt-fun-x80          pic  x(80)                  .
           05  w-qry-trt-fun-pnt          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per esecuzione analisi di portafoglio           *
      *    *-----------------------------------------------------------*
       01  w-ana-gep.
      *        *-------------------------------------------------------*
      *        * Totalizzazioni per tipo scadenza                      *
      *        *-------------------------------------------------------*
           05  w-ana-gep-tts.
      *            *---------------------------------------------------*
      *            * 12 elementi, per ogni tipo scadenza               *
      *            *  - 01 : Rimesse Dirette                           *
      *            *  - 02 : Dilazioni concordate                      *
      *            *  - 03 : Incassi Elettronici                       *
      *            *  - 04 : Ri.Ba.                                    *
      *            *  - 05 : C.d.O.                                    *
      *            *  - 06 : M.Av.                                     *
      *            *  - 07 : R.I.D.                                    *
      *            *  - 08 : Bonifici Bancari                          *
      *            *  - 09 : C/C Postali                               *
      *            *  - 10 : Ricevute Bancarie                         *
      *            *  - 11 : Tratte                                    *
      *            *  - 12 : Paghero'                                  *
      *            *---------------------------------------------------*
               10  w-ana-gep-tts-ele  occurs 12.
      *                *-----------------------------------------------*
      *                * Totale importo                                *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-tot  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale numero scadenze                        *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-num  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Totale 'A scadere'                            *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-asc  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Scaduto'                              *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-sca  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Insoluto'                             *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-isp  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Portafoglio'                          *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-por  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Circolazione'                         *
      *                *-----------------------------------------------*
                   15  w-ana-gep-tts-cir  pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Indici per scansione su elementi                  *
      *            *---------------------------------------------------*
               10  w-ana-gep-tts-i01      pic  9(02)                  .
               10  w-ana-gep-tts-i02      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per calcolo                                *
      *            *---------------------------------------------------*
               10  w-ana-gep-tts-s11      pic s9(11)                  .

      *    *===========================================================*
      *    * Work area per visualizzazione                             *
      *    *-----------------------------------------------------------*
       01  w-vis.
      *        *-------------------------------------------------------*
      *        * Per visualizzazione importi in valuta base            *
      *        *-------------------------------------------------------*
           05  w-vis-imp-vlb.
      *            *---------------------------------------------------*
      *            * Valore da visualizzare                            *
      *            *---------------------------------------------------*
               10  w-vis-imp-vlb-val      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Linea di visualizzazione                          *
      *            *---------------------------------------------------*
               10  w-vis-imp-vlb-lin      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione di visualizzazione                      *
      *            *---------------------------------------------------*
               10  w-vis-imp-vlb-pos      pic  9(03)                  .

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
      *        * Personalizzazioni per gestione portafoglio 'gep'      *
      *        *-------------------------------------------------------*
           05  w-prs-gep.
      *            *---------------------------------------------------*
      *            * Si/No aggiornamenti contabili a fronte delle mo-  *
      *            * vimentazioni di portafoglio, valido per ogni ti-  *
      *            * po di movimento di portafoglio                    *
      *            *                                                   *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *                                                   *
      *            * Default : Si                                      *
      *            *                                                   *
      *            * N.B. : Se questa personalizzazione indica che non *
      *            *        si devono eseguire gli aggiornamenti con-  *
      *            *        tabili, tutti i parametri successivi rela- *
      *            *        tivi alle modalita' per gli aggiornamenti  *
      *            *        contabili stessi diventano non significa-  *
      *            *        tivi.                                      *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-prs-gep-snx-cge      pic  x(01)                  .

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
           move      "APGRSP    "         to   w-spg-alf-gat          .
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
           move      "APGRSP    "         to   w-spg-alf-gat          .
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
      *              * Si/No funzionamento automatico : Si             *
      *              *-------------------------------------------------*
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
      *                  * Selezione su status effetti                 *
      *                  *---------------------------------------------*
           perform   acc-sts-eff-000      thru acc-sts-eff-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo evidenziazione            *
      *                  *---------------------------------------------*
           perform   acc-tip-evd-000      thru acc-tip-evd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
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
      *              * Prompt per selezione status effetti             *
      *              *-------------------------------------------------*
           perform   pmt-sts-eff-000      thru pmt-sts-eff-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione selezione status effetti        *
      *              *-------------------------------------------------*
           perform   vis-sts-eff-000      thru vis-sts-eff-999        .
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
      *    * Prompt per selezione status effetti                       *
      *    *-----------------------------------------------------------*
       pmt-sts-eff-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su stato effetti :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sts-eff-999.
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
           move      ">"                  to   v-edm                  .
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
           move      spaces               to   v-msk                  .
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
      *    * Accettazione campo selezione : Selezione su status effet- *
      *    * ti                                                        *
      *    *-----------------------------------------------------------*
       acc-sts-eff-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sts-eff-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-eff-lun    to   v-car                  .
           move      w-exp-sts-eff-num    to   v-ldt                  .
           move      "TPCI#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-eff-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sts-eff           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sts-eff-999.
       acc-sts-eff-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sts-eff             .
       acc-sts-eff-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        rr-sts-eff           not  = zero
                     go to acc-sts-eff-600.
           if        v-key                =    "UP  "
                     go to acc-sts-eff-600
           else      go to acc-sts-eff-100.
       acc-sts-eff-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sts-eff-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sts-eff-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sts-eff-100.
       acc-sts-eff-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Selezione su status effetti       *
      *    *-----------------------------------------------------------*
       vis-sts-eff-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-eff-lun    to   v-car                  .
           move      w-exp-sts-eff-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-eff-tbl    to   v-txt                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sts-eff           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sts-eff-999.
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
      *              * Selezione status effetti                        *
      *              *-------------------------------------------------*
           if        rr-sts-eff           =    zero
                     move  01             to   rr-sts-eff             .
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
      *                  * Selezione status effetti                    *
      *                  *---------------------------------------------*
           move      zero                 to   rr-sts-eff             .
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
           move      01                   to   rr-sts-scd             .
      *                  *---------------------------------------------*
      *                  * Selezione status effetti                    *
      *                  *---------------------------------------------*
           move      01                   to   rr-sts-eff             .
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
                     input  procedure     is   int-srt-inp-000
                                          thru int-srt-inp-999
                     output procedure     is   qry-rou-pri-000
                                          thru qry-rou-pri-999        .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       int-srt-inp-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione totali per tipo scadenza       *
      *              *-------------------------------------------------*
           perform   tot-tts-ini-000      thru tot-tts-ini-999        .
      *              *-------------------------------------------------*
      *              * Start su [sdb] per data chiusura scadenza       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "DATCHS    "         to   f-key                  .
           move      rr-dat-rfa           to   rf-sdb-dat-chs         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to int-srt-inp-999.
       int-srt-inp-100.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Next su [sdb] per data scadenza                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to int-srt-inp-999.
      *              *-------------------------------------------------*
      *              * Selezione su tipo scadenza                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo scadenza    *
      *                  *---------------------------------------------*
           go to     int-srt-inp-101
                     int-srt-inp-102
                     int-srt-inp-103
                     int-srt-inp-104
                     int-srt-inp-105
                     int-srt-inp-106
                     int-srt-inp-107
                     int-srt-inp-108
                     int-srt-inp-109
                     int-srt-inp-110
                     int-srt-inp-111
                     int-srt-inp-112
                     int-srt-inp-113
                     int-srt-inp-114
                     int-srt-inp-115
                     int-srt-inp-116
                     depending            on   rr-tip-scd             .
       int-srt-inp-101.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Tutte                       *
      *                  *---------------------------------------------*
           go to     int-srt-inp-200.
       int-srt-inp-102.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Rim.Dir.,Bonif.,C/C Post.   *
      *                  *---------------------------------------------*
           if       (rf-sdb-tip-sdb       not  = 01 or
                     rf-sdb-snx-dlc       not  = "N" ) and
                     rf-sdb-tip-sdb       not  = 07    and
                     rf-sdb-tip-sdb       not  = 08
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-103.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Scadenze elettroniche       *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 02 and
                     rf-sdb-tip-sdb       not  = 03 and
                     rf-sdb-tip-sdb       not  = 04 and
                     rf-sdb-tip-sdb       not  = 05 and
                     rf-sdb-tip-sdb       not  = 06
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-104.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Ric.Banc.,Tratte,Paghero'   *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10 and
                     rf-sdb-tip-sdb       not  = 11
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-105.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Rimesse Dirette        *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 01  or
                     rf-sdb-snx-dlc       not  = "N"
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-106.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Dilazioni Concordate   *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 01  or
                     rf-sdb-snx-dlc       not  = "S"
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-107.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Incassi Elettronici    *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 02
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-108.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Ri.Ba.                 *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 03
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-109.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo C.d.O.                 *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 04
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-110.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo M.Av.                  *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 05
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-111.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo R.I.D.                 *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 06
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-112.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Bonifici Bancari       *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 07
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-113.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo C/C Postali            *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 08
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-114.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Ricevute Bancarie      *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 09
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-115.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Tratte                 *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 10
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-116.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Paghero'               *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 11
                     go to int-srt-inp-100.
           go to     int-srt-inp-200.
       int-srt-inp-200.
      *              *-------------------------------------------------*
      *              * Determinazione status scadenza                  *
      *              *-------------------------------------------------*
           move      rr-dat-rfa           to   w-det-srd-sdb-drd      .
           perform   det-srd-sdb-000      thru det-srd-sdb-999        .
      *              *-------------------------------------------------*
      *              * Test se scadenza ancora aperta                  *
      *              *-------------------------------------------------*
           if        w-det-srd-sdb-sts    not  = "A"
                     go to int-srt-inp-100.
      *              *-------------------------------------------------*
      *              * Test se sigla ultima operazione eseguita e' a   *
      *              * spazi                                           *
      *              *-------------------------------------------------*
           if        w-det-srd-sdb-suo    =    spaces
                     go to int-srt-inp-100.
       int-srt-inp-300.
      *              *-------------------------------------------------*
      *              * Selezione su status scadenza                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo scadenza                       *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 01 and
                     rf-sdb-tip-sdb       not  = 07 and
                     rf-sdb-tip-sdb       not  = 08
                     go to int-srt-inp-350.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della selezione su   *
      *                  * status scadenze                             *
      *                  *---------------------------------------------*
           go to     int-srt-inp-301
                     int-srt-inp-302
                     int-srt-inp-303
                     depending            on   rr-sts-scd             .
       int-srt-inp-301.
      *                  *---------------------------------------------*
      *                  * Scadenze sia a scadere che scadute          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessuna selezione                       *
      *                      *-----------------------------------------*
           go to     int-srt-inp-400.
       int-srt-inp-302.
      *                  *---------------------------------------------*
      *                  * Solo quelle a scadere                       *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       not  >  rr-dat-rfa
                     go to int-srt-inp-100.
           go to     int-srt-inp-400.
       int-srt-inp-303.
      *                  *---------------------------------------------*
      *                  * Solo quelle scadute                         *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       >    rr-dat-rfa
                     go to int-srt-inp-100.
           go to     int-srt-inp-400.
       int-srt-inp-350.
      *              *-------------------------------------------------*
      *              * Selezione su status effetti                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della selezione su   *
      *                  * status effetti                              *
      *                  *---------------------------------------------*
           go to     int-srt-inp-351
                     int-srt-inp-352
                     int-srt-inp-353
                     int-srt-inp-354
                     depending            on   rr-sts-eff             .
       int-srt-inp-351.
      *                  *---------------------------------------------*
      *                  * Tutti gli effetti                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessuna selezione                       *
      *                      *-----------------------------------------*
           go to     int-srt-inp-400.
       int-srt-inp-352.
      *                  *---------------------------------------------*
      *                  * Solo effetti in portafoglio                 *
      *                  *---------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     go to int-srt-inp-100.
           go to     int-srt-inp-400.
       int-srt-inp-353.
      *                  *---------------------------------------------*
      *                  * Solo effetti in circolazione                *
      *                  *---------------------------------------------*
           if        w-det-srd-sdb-suo    not  = "PRE" and
                     w-det-srd-sdb-suo    not  = "ACT" and
                     w-det-srd-sdb-suo    not  = "ACD"
                     go to int-srt-inp-100.
           go to     int-srt-inp-400.
       int-srt-inp-354.
      *                  *---------------------------------------------*
      *                  * Solo effetti insoluti                       *
      *                  *---------------------------------------------*
           if        w-det-srd-sdb-suo    not  = "ISP"
                     go to int-srt-inp-100.
           go to     int-srt-inp-400.
       int-srt-inp-400.
      *              *-------------------------------------------------*
      *              * Totalizzazioni per tipo scadenza                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 03
                     go to int-srt-inp-500.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   tot-tts-agg-000      thru tot-tts-agg-999        .
       int-srt-inp-500.
      *              *-------------------------------------------------*
      *              * Composizione record di Sort                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiave del Sort                             *
      *                  *---------------------------------------------*
           move      rf-sdb-dts-sdb       to   srt-dts-sdb            .
           move      rf-sdb-num-sdb       to   srt-num-sdb            .
      *                  *---------------------------------------------*
      *                  * Dati del Sort                               *
      *                  *---------------------------------------------*
           move      rf-sdb-tip-sdb       to   srt-tip-sdb            .
           move      rf-sdb-tip-dbt       to   srt-tip-dbt            .
           move      rf-sdb-cod-dbt       to   srt-cod-dbt            .
           move      rf-sdb-dpz-dbt       to   srt-dpz-dbt            .
           move      rf-sdb-imp-sdb       to   srt-imp-sdb            .
           move      rf-sdb-snx-dlc       to   srt-snx-dlc            .
           move      rf-sdb-snx-cts       to   srt-snx-cts            .
           move      w-det-srd-sdb-suo    to   srt-suo-sdb            .
           move      rf-sdb-dso-l01       to   srt-dso-l01            .
           move      rf-sdb-dso-l02       to   srt-dso-l02            .
           move      rf-sdb-dso-l03       to   srt-dso-l03            .
           move      rf-sdb-nsc-org       to   srt-nsc-org            .
           move      rf-sdb-flg-ela       to   srt-flg-ela            .
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale record [sdb]     *
      *              *-------------------------------------------------*
           go to     int-srt-inp-100.
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
      *                  * Lettura sequenziale archivio sortato [srt]  *
      *                  *---------------------------------------------*
           return    srt    at end
                            move  "#"     to   w-cnt-qry-flg-sub      .
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
      *                  * Lettura archivio [dcc]                      *
      *                  *---------------------------------------------*
           if        srt-tip-dbt          not  = 01
                     go to qry-liv-det-250.
           if        srt-dpz-dbt          =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      srt-cod-dbt          to   w-let-arc-dcc-cod      .
           move      srt-dpz-dbt          to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
       qry-liv-det-250.
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
           move      "B"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      srt-num-sdb          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo scadenza                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      13                   to   v-pos                  .
      *
           if        srt-tip-sdb          =    01    and
                     srt-snx-dlc          =    "S"
                     move  "DC  "         to   v-alf
           else if   srt-tip-sdb          =    01    and
                     srt-snx-cts          =    "S"
                     move  "CT  "         to   v-alf
           else if   srt-tip-sdb          =    01    and
                     srt-snx-dlc          not  = "S" and
                     srt-snx-cts          not  = "S"
                     move  "RD  "         to   v-alf
           else if   srt-tip-sdb          =    02
                     move  "IE  "         to   v-alf
           else if   srt-tip-sdb          =    03
                     move  "RIBA"         to   v-alf
           else if   srt-tip-sdb          =    04
                     move  "CDO "         to   v-alf
           else if   srt-tip-sdb          =    05
                     move  "MAV "         to   v-alf
           else if   srt-tip-sdb          =    06
                     move  "RID "         to   v-alf
           else if   srt-tip-sdb          =    07
                     move  "BB  "         to   v-alf
           else if   srt-tip-sdb          =    08
                     move  "CCP "         to   v-alf
           else if   srt-tip-sdb          =    09
                     move  "RB  "         to   v-alf
           else if   srt-tip-sdb          =    10
                     move  "TR  "         to   v-alf
           else if   srt-tip-sdb          =    11
                     move  "PC  "         to   v-alf
           else      move  spaces         to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Mark-Point                              *
      *                      *-----------------------------------------*
           move      "+"                  to   v-edm                  .
           move      002                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      srt-num-sdb          to   w-mpn-num-002          .
      *
           if        srt-tip-dbt          =    01
                     move  srt-cod-dbt    to   w-mpn-cod-002
                     move  srt-dpz-dbt    to   w-mpn-dpz-002
                     move  w-let-arc-dcc-rag
                                          to   w-mpn-rag-002
           else      move  zero           to   w-mpn-cod-002
                     move  spaces         to   w-mpn-dpz-002
                     move  spaces         to   w-mpn-rag-002          .
           move      w-mpn                to   v-cnt                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-300.
      *                  *---------------------------------------------*
      *                  * Literal per avviso ricevuto                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        srt-flg-nbx (1)      not  = "S"
                     go to qry-liv-det-320.
      *                      *-----------------------------------------*
      *                      * Stampa segnale                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "a"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-320.
      *                  *---------------------------------------------*
      *                  * Test se la scadenza di origine proviene da  *
      *                  * un insoluto                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        srt-nsc-org          =    zero
                     go to qry-liv-det-400.
      *                      *-----------------------------------------*
      *                      * Lettura [sdb] per la scadenza di ori-   *
      *                      * gine                                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      srt-nsc-org          to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                          *-------------------------------------*
      *                          * Test su esito della lettura         *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Literal per insoluto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se la scadenza di orgine e' un     *
      *                      * insoluto                                *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-isp       =    zero
                     go to qry-liv-det-400.
      *                      *-----------------------------------------*
      *                      * Stampa segnale                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "i"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se data normale                         *
      *                      *-----------------------------------------*
           if        srt-dts-sdb          =    zero
                     go to qry-liv-det-420.
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      srt-dts-sdb          to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-500.
       qry-liv-det-420.
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
       qry-liv-det-500.
      *                  *---------------------------------------------*
      *                  * Segnale di presenza solleciti               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        srt-dso-l01          =    zero and
                     srt-dso-l02          =    zero and
                     srt-dso-l03          =    zero
                     go to qry-liv-det-520.
      *                      *-----------------------------------------*
      *                      * Stampa segnale                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      18                   to   v-pos                  .
           move      "s"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-520.
      *                  *---------------------------------------------*
      *                  * Importo scadenza                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione v-pos                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Rimesse Dirette, Bonifici o C/C  *
      *                          * postali                             *
      *                          *-------------------------------------*
           if        srt-tip-sdb          not  = 01 and
                     srt-tip-sdb          not  = 07 and
                     srt-tip-sdb          not  = 08
                     go to qry-liv-det-550.
      *                              *---------------------------------*
      *                              * Test su data scadenza           *
      *                              *---------------------------------*
           if        srt-dts-sdb          not  >  rr-dat-rfa
                     move  41             to   v-pos
           else      move  28             to   v-pos                  .
      *                              *---------------------------------*
      *                              * A stampa                        *
      *                              *---------------------------------*
           go to     qry-liv-det-580.
       qry-liv-det-550.
      *                          *-------------------------------------*
      *                          * Se altri tipi scadenza              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su status scadenza         *
      *                              *---------------------------------*
           if        srt-suo-sdb          =    "PRE" or
                     srt-suo-sdb          =    "ACT" or
                     srt-suo-sdb          =    "ACD"
                     move  68             to   v-pos
           else if   srt-suo-sdb          =    "ISP"
                     move  41             to   v-pos
           else      move  54             to   v-pos                  .
      *                              *---------------------------------*
      *                              * A stampa                        *
      *                              *---------------------------------*
           go to     qry-liv-det-580.
       qry-liv-det-580.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        srt-imp-sdb          >    999999999 or
                     srt-imp-sdb          <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        srt-imp-sdb          >    999999999 or
                     srt-imp-sdb          <   -999999999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
           move      v-lnr                to   v-lin                  .
      *
           if        c-dec                <    1
                     add  1               to   v-pos                  .
      *
           move      srt-imp-sdb          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      -              "  Portafoglio    Circolazione "
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
       vis-tot-tts-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..12               *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-i01      .
       vis-tot-tts-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore 01..12                     *
      *              *-------------------------------------------------*
           add       1                    to   w-ana-gep-tts-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a totali                  *
      *              *-------------------------------------------------*
           if        w-ana-gep-tts-i01    >    12
                     go to vis-tot-tts-800.
       vis-tot-tts-400.
      *              *-------------------------------------------------*
      *              * Stampa elemento con indice 'w-ana-gep-tts-i01'  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-500.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo scadenza    *
      *                  *---------------------------------------------*
           go to     vis-tot-tts-510
                     vis-tot-tts-520
                     vis-tot-tts-530
                     vis-tot-tts-540
                     vis-tot-tts-550
                     vis-tot-tts-560
                     vis-tot-tts-570
                     vis-tot-tts-580
                     vis-tot-tts-590
                     vis-tot-tts-600
                     vis-tot-tts-610
                     vis-tot-tts-620
                     depending            on   w-ana-gep-tts-i01      .
           go to     vis-tot-tts-700.
       vis-tot-tts-510.
      *                  *---------------------------------------------*
      *                  * Indice 01 : Rimesse Dirette                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      01                   to   w-mpn-tsc-001          .
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
           move      "RD              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A scadere                               *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-asc (01)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      18                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scaduto                                 *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-sca (01)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-520.
      *                  *---------------------------------------------*
      *                  * Indice 02 : Dilazioni concordate            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      01                   to   w-mpn-tsc-001          .
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
           move      "DC              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A scadere                               *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-asc (02)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      18                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scaduto                                 *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-sca (02)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-530.
      *                  *---------------------------------------------*
      *                  * Indice 03 : Incassi Elettronici             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      03                   to   w-mpn-tsc-001          .
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
           move      "IE              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (03)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (03)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (03)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-540.
      *                  *---------------------------------------------*
      *                  * Indice 04 : Ri.Ba.                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      04                   to   w-mpn-tsc-001          .
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
           move      "RIBA            :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (04)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (04)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (04)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-550.
      *                  *---------------------------------------------*
      *                  * Indice 05 : C.d.O.                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      05                   to   w-mpn-tsc-001          .
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
           move      "CDO             :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (05)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (05)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (05)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-560.
      *                  *---------------------------------------------*
      *                  * Indice 06 : M.Av.                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      06                   to   w-mpn-tsc-001          .
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
           move      "MAV             :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (06)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (06)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (06)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-570.
      *                  *---------------------------------------------*
      *                  * Indice 07 : R.I.D.                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      07                   to   w-mpn-tsc-001          .
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
           move      "RID             :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (07)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (07)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (07)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-580.
      *                  *---------------------------------------------*
      *                  * Indice 08 : Bonifici Bancari                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      08                   to   w-mpn-tsc-001          .
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
           move      "BB              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A scadere                               *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-asc (08)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      18                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scaduto                                 *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-sca (08)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-590.
      *                  *---------------------------------------------*
      *                  * Indice 09 : C/C Postali                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      09                   to   w-mpn-tsc-001          .
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
           move      "CCP             :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A scadere                               *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-asc (09)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      18                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scaduto                                 *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-sca (09)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-600.
      *                  *---------------------------------------------*
      *                  * Indice 10 : Ricevute Bancarie               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      10                   to   w-mpn-tsc-001          .
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
           move      "RB              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (10)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (10)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (10)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-610.
      *                  *---------------------------------------------*
      *                  * Indice 11 : Tratte                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      11                   to   w-mpn-tsc-001          .
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
           move      "TR              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (11)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (11)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (11)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
       vis-tot-tts-620.
      *                  *---------------------------------------------*
      *                  * Indice 12 : Paghero'                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      12                   to   w-mpn-tsc-001          .
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
           move      "PC              :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Insoluto                                *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-isp (12)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Portafoglio                             *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-por (12)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Circolazione                            *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-cir (12)
                                          to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-tts-700.
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
      -              "--------------  --------------"
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
           move      zero                 to   w-ana-gep-tts-s11      .
           move      zero                 to   w-ana-gep-tts-i01      .
       vis-tot-tts-832.
           add       1                    to   w-ana-gep-tts-i01      .
           if        w-ana-gep-tts-i01    >    12
                     go to vis-tot-tts-833.
           add       w-ana-gep-tts-asc
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           go to     vis-tot-tts-832.
       vis-tot-tts-833.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-s11    to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      18                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-tts-840.
      *                  *---------------------------------------------*
      *                  * Totale 'Scaduto'                            *
      *                  *---------------------------------------------*
       vis-tot-tts-841.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-gep-tts-s11      .
           move      zero                 to   w-ana-gep-tts-i01      .
       vis-tot-tts-842.
           add       1                    to   w-ana-gep-tts-i01      .
           if        w-ana-gep-tts-i01    >    12
                     go to vis-tot-tts-843.
           add       w-ana-gep-tts-sca
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           add       w-ana-gep-tts-isp
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           go to     vis-tot-tts-842.
       vis-tot-tts-843.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-s11    to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      34                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-tts-850.
      *                  *---------------------------------------------*
      *                  * Totale 'Portafoglio'                        *
      *                  *---------------------------------------------*
       vis-tot-tts-851.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-gep-tts-s11      .
           move      zero                 to   w-ana-gep-tts-i01      .
       vis-tot-tts-852.
           add       1                    to   w-ana-gep-tts-i01      .
           if        w-ana-gep-tts-i01    >    12
                     go to vis-tot-tts-853.
           add       w-ana-gep-tts-por
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           go to     vis-tot-tts-852.
       vis-tot-tts-853.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-s11    to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      50                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-tts-860.
      *                  *---------------------------------------------*
      *                  * Totale 'Circolazione'                       *
      *                  *---------------------------------------------*
       vis-tot-tts-861.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-gep-tts-s11      .
           move      zero                 to   w-ana-gep-tts-i01      .
       vis-tot-tts-862.
           add       1                    to   w-ana-gep-tts-i01      .
           if        w-ana-gep-tts-i01    >    12
                     go to vis-tot-tts-863.
           add       w-ana-gep-tts-cir
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           go to     vis-tot-tts-862.
       vis-tot-tts-863.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      w-ana-gep-tts-s11    to   w-vis-imp-vlb-val      .
           move      v-lnr                to   w-vis-imp-vlb-lin      .
           move      66                   to   w-vis-imp-vlb-pos      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-tts-875.
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
      *                  * Literal per 'Esposizione'                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "Esposizione totale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-895.
      *                  *---------------------------------------------*
      *                  * Totale 'Esposizione'                        *
      *                  *---------------------------------------------*
       vis-tot-tts-896.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-gep-tts-s11      .
           move      zero                 to   w-ana-gep-tts-i01      .
       vis-tot-tts-897.
           add       1                    to   w-ana-gep-tts-i01      .
           if        w-ana-gep-tts-i01    >    12
                     go to vis-tot-tts-898.
           add       w-ana-gep-tts-asc
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           add       w-ana-gep-tts-sca
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           add       w-ana-gep-tts-isp
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           add       w-ana-gep-tts-por
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
           add       w-ana-gep-tts-cir
                    (w-ana-gep-tts-i01)   to   w-ana-gep-tts-s11      .
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
           move      46                   to   v-pos                  .
           move      w-ana-gep-tts-s11    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *                  * Se codice cliente a zero, a visualizzazione *
      *                  * a spaces                                    *
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
      *                  * Cliente                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione stringa                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-qry-trt-fun-x80      .
           move      1                    to   w-qry-trt-fun-pnt      .
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           string    "Cliente : "
                                delimited by   size
                                          into w-qry-trt-fun-x80
                                  with pointer w-qry-trt-fun-pnt      .
      *                          *-------------------------------------*
      *                          * Codice cliente                      *
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
      *                          * Dipendenza cliente                  *
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
      *                          * Ragione sociale cliente             *
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
      *                      * Linea cliente a spazi                   *
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
           move      w-mpn-num-002        to   s-num                  .
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
           move      "  Numero    Tipo     Data     A scadere     Scadut
      -              "o    Portafoglio  Circolazione"
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
           move      "----------- ----   --------  -----------  --------
      -              "---  -----------  ------------"
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
      *              * Inizializzazione indice 01..12                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-i01      .
       tot-tts-ini-100.
      *              *-------------------------------------------------*
      *              * Incremento indice 01..12                        *
      *              *-------------------------------------------------*
           add       1                    to   w-ana-gep-tts-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il max : uscita                        *
      *              *-------------------------------------------------*
           if        w-ana-gep-tts-i01    >    12
                     go to tot-tts-ini-999.
      *              *-------------------------------------------------*
      *              * Azzeramento totale importo                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-tot
                                              (w-ana-gep-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale numero scadenze              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-num
                                              (w-ana-gep-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'A scadere'                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-asc
                                              (w-ana-gep-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Scaduto'                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-sca
                                              (w-ana-gep-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Insoluto'                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-isp
                                              (w-ana-gep-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Portafoglio'                *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-por
                                              (w-ana-gep-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Circolazione'               *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-gep-tts-cir
                                              (w-ana-gep-tts-i01)     .
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
           move      rf-sdb-tip-sdb       to   w-ana-gep-tts-i01      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo scadenza, e se  *
      *              * non riconosciuto : nessun aggiornamento         *
      *              *-------------------------------------------------*
           go to     tot-tts-agg-100
                     tot-tts-agg-150
                     tot-tts-agg-200
                     tot-tts-agg-250
                     tot-tts-agg-300
                     tot-tts-agg-350
                     tot-tts-agg-400
                     tot-tts-agg-450
                     tot-tts-agg-500
                     tot-tts-agg-550
                     tot-tts-agg-600
                     depending            on   w-ana-gep-tts-i01      .
       tot-tts-agg-100.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 01 : Rimesse Dirette           *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Deviazione a seconda che si tratti di di-  *
      *                   * lazione concordata o no                    *
      *                   *--------------------------------------------*
           if        rf-sdb-snx-dlc       not  = "S"
                     go to tot-tts-agg-110
           else      go to tot-tts-agg-120.
       tot-tts-agg-110.
      *                   *--------------------------------------------*
      *                   * Se non dilazione concordata                *
      *                   *--------------------------------------------*
      *                       *----------------------------------------*
      *                       * Aggiornamento totale importo           *
      *                       *----------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (01) .
      *                       *----------------------------------------*
      *                       * Aggiornamento totale numero scadenze   *
      *                       *----------------------------------------*
           add       1                    to   w-ana-gep-tts-num (01) .
      *                       *----------------------------------------*
      *                       * Aggiornamento 'A scadere'/'Scaduto'    *
      *                       *----------------------------------------*
           if        rf-sdb-dts-sdb       >    rr-dat-rfa
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-asc (01)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-sca (01) .
      *                       *----------------------------------------*
      *                       * Uscita                                 *
      *                       *----------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-120.
      *                   *--------------------------------------------*
      *                   * Se dilazione concordata                    *
      *                   *--------------------------------------------*
      *                       *----------------------------------------*
      *                       * Aggiornamento totale importo           *
      *                       *----------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (02) .
      *                       *----------------------------------------*
      *                       * Aggiornamento totale numero scadenze   *
      *                       *----------------------------------------*
           add       1                    to   w-ana-gep-tts-num (02) .
      *                       *----------------------------------------*
      *                       * Aggiornamento 'A scadere'/'Scaduto'    *
      *                       *----------------------------------------*
           if        rf-sdb-dts-sdb       >    rr-dat-rfa
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-asc (02)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-sca (02) .
      *                       *----------------------------------------*
      *                       * Uscita                                 *
      *                       *----------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-150.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 02 : Incassi Elettronici       *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (03) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (03) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (03)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (03)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (03) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-200.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 03 : Ri.Ba.                    *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (04) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (04) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (04)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (04)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (04) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-250.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 04 : C.d.O.                    *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (05) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (05) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (05)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (05)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (05) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-300.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 05 : M.Av.                     *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (06) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (06) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (06)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (06)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (06) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-350.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 06 : R.I.D.                    *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (07) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (07) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (07)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (07)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (07) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-400.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 07 : Bonifici Bancari          *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (08) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (08) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'A scadere'/'Scaduto'        *
      *                   *--------------------------------------------*
           if        rf-sdb-dts-sdb       >    rr-dat-rfa
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-asc (08)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-sca (08) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-450.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 08 : C/C Postali               *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (09) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (09) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'A scadere'/'Scaduto'        *
      *                   *--------------------------------------------*
           if        rf-sdb-dts-sdb       >    rr-dat-rfa
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-asc (09)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-sca (09) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-500.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 09 : Ricevute Bancarie         *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (10) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (10) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (10)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (10)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (10) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-550.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 10 : Tratte                    *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (11) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (11) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (11)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (11)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (11) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-600.
      *              *-------------------------------------------------*
      *              * Se tipo scadenza 11 : Paghero'                  *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale importo               *
      *                   *--------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-ana-gep-tts-tot (12) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento totale numero scadenze       *
      *                   *--------------------------------------------*
           add       1                    to   w-ana-gep-tts-num (12) .
      *                   *--------------------------------------------*
      *                   * Aggiornamento 'Insoluto/Portafoglio'/'Cir- *
      *                   * colazione'                                 *
      *                   *--------------------------------------------*
           if        w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-cir (12)
           else if   w-det-srd-sdb-suo    =    "ISP"
                     add   rf-sdb-imp-sdb to   w-ana-gep-tts-isp (12)
           else      add   rf-sdb-imp-sdb to   w-ana-gep-tts-por (12) .
      *                   *--------------------------------------------*
      *                   * Uscita                                     *
      *                   *--------------------------------------------*
           go to     tot-tts-agg-999.
       tot-tts-agg-999.
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
      *    * Subroutine per visualizzazione importo in valuta base     *
      *    *-----------------------------------------------------------*
       vis-imp-vlb-000.
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      w-vis-imp-vlb-lin    to   v-lin                  .
           move      w-vis-imp-vlb-pos    to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      w-vis-imp-vlb-val    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
        vis-imp-vlb-999.
           exit.

