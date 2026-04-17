       Identification Division.
       Program-Id.                                 pgep301a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    mov                 *
      *                                   Fase:    gep301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/04/94    *
      *                       Ultima revisione:    NdK del 11/02/11    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti per gestione   *
      *                    portafoglio                                 *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Interrogazione su scadenze cliente          *
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
      *    * Descrizione tipo interrogazione per l'overlay             *
      *    *-----------------------------------------------------------*
       01  w-des-tit-pgm-ovy              pic  x(40)       value
                     "   INTERROGAZIONE SU SCADENZE CLIENTE   "       .

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
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

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
      *        * Per codice cliente                                    *
      *        *-------------------------------------------------------*
           05  w-ipc-cod-cli.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al codice cli- *
      *            * ente                                              *
      *            *---------------------------------------------------*
               10  w-ipc-cod-cli-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al co-  *
      *            * dice cliente                                      *
      *            *---------------------------------------------------*
               10  w-ipc-cod-cli-val      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per dipendenza cliente                                *
      *        *-------------------------------------------------------*
           05  w-ipc-dpz-cli.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al codice di-  *
      *            * pendenza cliente                                  *
      *            *---------------------------------------------------*
               10  w-ipc-dpz-cli-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al co-  *
      *            * dice dipendenza cliente                           *
      *            *---------------------------------------------------*
               10  w-ipc-dpz-cli-val      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Per tipo scadenza                                     *
      *        *-------------------------------------------------------*
           05  w-ipc-tip-sdb.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * scadenza                                          *
      *            *---------------------------------------------------*
               10  w-ipc-tip-sdb-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al ti-  *
      *            * po di scadenza                                    *
      *            *---------------------------------------------------*
               10  w-ipc-tip-sdb-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per tipo acquisizione scadenza                        *
      *        *-------------------------------------------------------*
           05  w-ipc-tac-sdb.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * acquisizione scadenza                             *
      *            *---------------------------------------------------*
               10  w-ipc-tac-sdb-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al ti-  *
      *            * po di acquisizione scadenza                       *
      *            *---------------------------------------------------*
               10  w-ipc-tac-sdb-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per status scadenza                                   *
      *        *-------------------------------------------------------*
           05  w-ipc-sts-sdb.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa allo status    *
      *            * scadenza                                          *
      *            *---------------------------------------------------*
               10  w-ipc-sts-sdb-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa allo    *
      *            * status scadenza                                   *
      *            *---------------------------------------------------*
               10  w-ipc-sts-sdb-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per numeri scadenza da escludere dalla selezione in   *
      *        * quanto gia' appartenenti all'operazione di riscossio- *
      *        * ne in corso di composizione                           *
      *        *-------------------------------------------------------*
           05  w-ipc-nsd-eds.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa ai numeri sca- *
      *            * denza da escludere dalla selezione                *
      *            *---------------------------------------------------*
               10  w-ipc-nsd-eds-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa ai nu-  *
      *            * meri scadenza da escludere dalla selezione, cor-  *
      *            * rispondente al numero di scadenze da escludere    *
      *            *---------------------------------------------------*
               10  w-ipc-nsd-eds-val      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numeri scadenza per le scadenze da escludere      *
      *            *---------------------------------------------------*
               10  w-ipc-nsd-eds-nsc
                             occurs  999  pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di lavoro per il caricamento della variabile *
      *            *---------------------------------------------------*
               10  w-ipc-nsd-eds-i01      pic  9(03)                  .
               10  w-ipc-nsd-eds-i02      pic  9(03)                  .
               10  w-ipc-nsd-eds-i03      pic  9(02)                  .
               10  w-ipc-nsd-eds-wnv.
                   15  w-ipc-nsd-eds-wn0  pic  x(07)                  .
                   15  w-ipc-nsd-eds-wn9  pic  9(03)                  .
               10  w-ipc-nsd-eds-wrl.
                   15  w-ipc-nsd-eds-wrn
                              occurs  07  pic  9(11)                  .
                   15  filler             pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-cli                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente, ragione sociale                       *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente, indirizzo                             *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-via             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente, localita'                             *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente                         *
      *        *-------------------------------------------------------*
           05  rr-dpz-cli                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente, ragione sociale        *
      *        *-------------------------------------------------------*
           05  rr-dpz-cli-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente, indirizzo              *
      *        *-------------------------------------------------------*
           05  rr-dpz-cli-via             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente, localita'              *
      *        *-------------------------------------------------------*
           05  rr-dpz-cli-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipi di scadenza da ricercare                         *
      *        * - 00 : Tutti                                          *
      *        * - 01 : Rimesse dirette                                *
      *        * - 02 : Incassi elettronici                            *
      *        * - 03 : Ri.Ba.                                         *
      *        * - 04 : C.d.O.                                         *
      *        * - 05 : M.Av.                                          *
      *        * - 06 : R.I.D.                                         *
      *        * - 07 : Bonifici bancari                               *
      *        * - 08 : C/C postali                                    *
      *        * - 09 : Ricevute bancarie                              *
      *        * - 10 : Tratte                                         *
      *        * - 11 : Paghero' cambiari                              *
      *        * - 61 : Paghero' cambiari avuti in cessione            *
      *        *-------------------------------------------------------*
           05  rr-tip-sdb                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data scadenza minima da ricercare                     *
      *        *-------------------------------------------------------*
           05  rr-dts-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data scadenza massima da ricercare                    *
      *        *-------------------------------------------------------*
           05  rr-dts-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Importo da ricercare                                  *
      *        *-------------------------------------------------------*
           05  rr-imp-sdb                 pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Status scadenze da ricercare                          *
      *        * - 01 : Solo quelle ancora aperte                      *
      *        * - 02 : Tutte                                          *
      *        * - 03 : Solo quelle solo emesse                        *
      *        * - 04 : Solo quelle stornate                           *
      *        * - 05 : Solo quelle riscosse                           *
      *        * - 06 : Solo quelle pagate                             *
      *        * - 07 : Solo quelle compensate                         *
      *        * - 08 : Solo quelle presentate                         *
      *        * - 09 : Solo quelle insolute                           *
      *        * - 10 : Solo quelle richiamate                         *
      *        * - 11 : Solo quelle accreditate                        *
      *        * - 12 : Solo quelle esitate                            *
      *        *-------------------------------------------------------*
           05  rr-sts-sdb                 pic  9(02)                  .

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
      *        * Work per determinazione status scadenza               *
      *        *-------------------------------------------------------*
           05  w-det-sts-sdb.
      *            *---------------------------------------------------*
      *            * Status determinato                                *
      *            * - Spaces : Status non determinabile               *
      *            * - A      : Scadenza ancora aperta                 *
      *            * - C      : Scadenza ormai chiusa                  *
      *            *---------------------------------------------------*
               10  w-det-sts-sdb-sts      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sigla ultima operazione eseguita sulla scadenza   *
      *            * - Spaces : Nessuna                                *
      *            * - EMI    : Emissione                              *
      *            * - SSC    : Storno                                 *
      *            * - RIS    : Riscossione                            *
      *            * - PAG    : Pagamento                              *
      *            * - CMP    : Compensazione                          *
      *            * - IID    : Inclusione in distinta                 *
      *            * - RSP    : Richiamo della scadenza presentata     *
      *            * - ACS    : Accredito scadenza al dopo incasso     *
      *            * - NBE    : Notizia di buon esito sulla scadenza   *
      *            * - PBE    : Presunto buon esito sulla scadenza     *
      *            * - ISP    : Insoluto sulla scadenza presentata     *
      *            *---------------------------------------------------*
               10  w-det-sts-sdb-suo      pic  x(03)                  .
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

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cli              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente                         *
      *        *-------------------------------------------------------*
           05  w-sav-dpz-cli              pic  x(04)                  .

      *    *===========================================================*
      *    * Work-area per ripristino Start                            *
      *    *-----------------------------------------------------------*
       01  w-rip-str.
      *        *-------------------------------------------------------*
      *        * Work-area per ripristino Start su [sdb]               *
      *        *-------------------------------------------------------*
           05  w-rip-str-sdb.
      *            *---------------------------------------------------*
      *            * Flag di End-of-file                               *
      *            *---------------------------------------------------*
               10  w-rip-str-sdb-eof      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Parametri di Start                                *
      *            *---------------------------------------------------*
               10  w-rip-str-sdb-tip      pic  9(02)                  .
               10  w-rip-str-sdb-cod      pic  9(07)                  .
               10  w-rip-str-sdb-dts      pic  9(07)                  .
               10  w-rip-str-sdb-num      pic  9(11)                  .

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
      *        * Work per : Tipi scadenza da ricercare                 *
      *        *-------------------------------------------------------*
           05  w-exp-tsc-drc.
               10  w-exp-tsc-drc-num      pic  9(02)       value 13   .
               10  w-exp-tsc-drc-lun      pic  9(02)       value 33   .
               10  w-exp-tsc-drc-tbl.
                   15  filler             pic  x(33) value
                            "Tutti                            "       .
                   15  filler             pic  x(33) value
                            "RD                               "       .
                   15  filler             pic  x(33) value
                            "IE                               "       .
                   15  filler             pic  x(33) value
                            "RIBA                             "       .
                   15  filler             pic  x(33) value
                            "CDO                              "       .
                   15  filler             pic  x(33) value
                            "MAV                              "       .
                   15  filler             pic  x(33) value
                            "RID                              "       .
                   15  filler             pic  x(33) value
                            "BB                               "       .
                   15  filler             pic  x(33) value
                            "CCP                              "       .
                   15  filler             pic  x(33) value
                            "RB                               "       .
                   15  filler             pic  x(33) value
                            "TR                               "       .
                   15  filler             pic  x(33) value
                            "PC                               "       .
                   15  filler             pic  x(33) value
                            "PC ceduti da terzi               "       .
      *        *-------------------------------------------------------*
      *        * Work per : Status scadenze                            *
      *        *-------------------------------------------------------*
           05  w-exp-sts-sdb.
               10  w-exp-sts-sdb-num      pic  9(02)       value 12   .
               10  w-exp-sts-sdb-lun      pic  9(02)       value 25   .
               10  w-exp-sts-sdb-tbl.
                   15  filler             pic  x(25) value
                            "Solo quelle ancora aperte"               .
                   15  filler             pic  x(25) value
                            "Tutte                    "               .
                   15  filler             pic  x(25) value
                            "Solo quelle solo emesse  "               .
                   15  filler             pic  x(25) value
                            "Solo quelle stornate     "               .
                   15  filler             pic  x(25) value
                            "Solo quelle riscosse     "               .
                   15  filler             pic  x(25) value
                            "Solo quelle pagate       "               .
                   15  filler             pic  x(25) value
                            "Solo quelle compensate   "               .
                   15  filler             pic  x(25) value
                            "Solo quelle presentate   "               .
                   15  filler             pic  x(25) value
                            "Solo quelle insolute     "               .
                   15  filler             pic  x(25) value
                            "Solo quelle richiamate   "               .
                   15  filler             pic  x(25) value
                            "Solo quelle accreditate  "               .
                   15  filler             pic  x(25) value
                            "Solo quelle esitate      "               .

      *    *===========================================================*
      *    * Work per routine stp-int-pag-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-stp-int-pag.
           05  w-stp-int-pag-x80          pic  x(80)                  .
           05  w-stp-int-pag-pnt          pic  9(03)                  .

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
      *        * Numero scadenza                                       *
      *        *-------------------------------------------------------*
           05  w-mpn-num-sdb              pic  9(11)                  .

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
      *              * il codice cliente                               *
      *              *-------------------------------------------------*
           perform   ipc-cod-cli-000      thru ipc-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il codice dipendenza cliente                    *
      *              *-------------------------------------------------*
           perform   ipc-dpz-cli-000      thru ipc-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo scadenza                                *
      *              *-------------------------------------------------*
           perform   ipc-tip-sdb-000      thru ipc-tip-sdb-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo acquisizione scadenza                   *
      *              *-------------------------------------------------*
           perform   ipc-tac-sdb-000      thru ipc-tac-sdb-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * lo status scadenza                              *
      *              *-------------------------------------------------*
           perform   ipc-sts-sdb-000      thru ipc-sts-sdb-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * l'esclusione di certi numeri scadenza dalla se- *
      *              * lezione, in quanto gia' appartenenti ad una o-  *
      *              * perazione di riscossione in corso di composi-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           perform   ipc-nsd-eds-000      thru ipc-nsd-eds-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "INTSDB    "         to   w-spg-alf-gat          .
           perform   mem-spg-att-000      thru mem-spg-att-999        .
      *              *-------------------------------------------------*
      *              * Se memorizzazione non avvenuta : uscita con er- *
      *              * rore                                            *
      *              *-------------------------------------------------*
           if        w-spg-snx-gat        not  = spaces
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No proposta tutte dipendenze come        *
      *                  * default di selezione per i clienti          *
      *                  *---------------------------------------------*
           perform   prs-snx-tdd-dcc-000  thru prs-snx-tdd-dcc-999    .
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
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Eliminazione sottoprogramma in attivita'        *
      *              *-------------------------------------------------*
           move      "INTSDB    "         to   w-spg-alf-gat          .
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
      *    * Lettura della variabile eventuale di i.p.c. per codice    *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
       ipc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'cod-cli' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
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
                     go to ipc-cod-cli-200
           else      go to ipc-cod-cli-400.
       ipc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-cod-cli-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-cod-cli-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cod-cli-999.
       ipc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-cod-cli-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-cod-cli-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cod-cli-999.
       ipc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per codice    *
      *    * dipendenza cliente                                        *
      *    *-----------------------------------------------------------*
       ipc-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dpz-cli' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dpz-cli"            to   s-var                  .
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
                     go to ipc-dpz-cli-200
           else      go to ipc-dpz-cli-400.
       ipc-dpz-cli-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dpz-cli-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-dpz-cli-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dpz-cli-999.
       ipc-dpz-cli-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dpz-cli-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-dpz-cli-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dpz-cli-999.
       ipc-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di scadenza                                               *
      *    *-----------------------------------------------------------*
       ipc-tip-sdb-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tip-sdb' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tip-sdb"            to   s-var                  .
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
                     go to ipc-tip-sdb-200
           else      go to ipc-tip-sdb-400.
       ipc-tip-sdb-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tip-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-tip-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tip-sdb-999.
       ipc-tip-sdb-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tip-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-tip-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tip-sdb-999.
       ipc-tip-sdb-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di acquisizione scadenza                                  *
      *    *-----------------------------------------------------------*
       ipc-tac-sdb-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tac-sdb' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tac-sdb"            to   s-var                  .
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
                     go to ipc-tac-sdb-200
           else      go to ipc-tac-sdb-400.
       ipc-tac-sdb-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tac-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-tac-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tac-sdb-999.
       ipc-tac-sdb-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tac-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-tac-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tac-sdb-999.
       ipc-tac-sdb-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per status    *
      *    * scadenze                                                  *
      *    *-----------------------------------------------------------*
       ipc-sts-sdb-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'sts-sdb' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "sts-sdb"            to   s-var                  .
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
                     go to ipc-sts-sdb-200
           else      go to ipc-sts-sdb-400.
       ipc-sts-sdb-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-sts-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-sts-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-sts-sdb-999.
       ipc-sts-sdb-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-sts-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-sts-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-sts-sdb-999.
       ipc-sts-sdb-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per i numeri  *
      *    * scadenza da escludere dalla selezione in quanto gia' ap-  *
      *    * partenenti alla riscossione in corso di formazione        *
      *    *-----------------------------------------------------------*
       ipc-nsd-eds-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'nsd-eds' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "nsd-eds"            to   s-var                  .
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
                     go to ipc-nsd-eds-100
           else      go to ipc-nsd-eds-900.
       ipc-nsd-eds-100.
      *              *-------------------------------------------------*
      *              * Se variabile 'nsd-eds' esistente                *
      *              *-------------------------------------------------*
       ipc-nsd-eds-150.
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-nsd-eds-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-nsd-eds-val      .
       ipc-nsd-eds-200.
      *                  *---------------------------------------------*
      *                  * Raccolta dei numeri di scadenza da esclude- *
      *                  * re dalla selezione                          *
      *                  *---------------------------------------------*
       ipc-nsd-eds-225.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice 001..143, per   *
      *                      * records variabili di i.p.c. con numeri  *
      *                      * scadenza da escludere, contenenti cia-  *
      *                      * scuno max 7 numeri scadenza             *
      *                      *-----------------------------------------*
           move      zero                 to   w-ipc-nsd-eds-i01      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice 001..999, per   *
      *                      * bufferizzazione numeri scadenza da e-   *
      *                      * scludere                                *
      *                      *-----------------------------------------*
           move      zero                 to   w-ipc-nsd-eds-i02      .
       ipc-nsd-eds-250.
      *                      *-----------------------------------------*
      *                      * Incremento indice 001..143              *
      *                      *-----------------------------------------*
           add       1                    to   w-ipc-nsd-eds-i01      .
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo : fine caricamento  *
      *                      *-----------------------------------------*
           if        w-ipc-nsd-eds-i01    >    143
                     go to ipc-nsd-eds-500.
       ipc-nsd-eds-275.
      *                      *-----------------------------------------*
      *                      * Composizione nome della variabile da    *
      *                      * leggere per il gruppo successivo di 7   *
      *                      * numeri scadenza da escludere            *
      *                      *-----------------------------------------*
           move      "nsd-eds"            to   w-ipc-nsd-eds-wn0      .
           move      w-ipc-nsd-eds-i01    to   w-ipc-nsd-eds-wn9      .
       ipc-nsd-eds-300.
      *                      *-----------------------------------------*
      *                      * Estrazione della variabile 'nsd-edsnnn' *
      *                      * dal livello di profondita' precedente   *
      *                      * o dallo stesso livello di profondita'   *
      *                      * applicativa a seconda se il sottopro-   *
      *                      * gramma e' stato richiamato dal main op- *
      *                      * pure da un sottoprogramma dello stesso  *
      *                      * livello                                 *
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      w-ipc-nsd-eds-wnv    to   s-var                  .
           if        w-ipc-tdc-mos-snx    =    "S" and
                     w-ipc-tdc-mos-val    =    "M"
                     move  "-"            to   s-dop
           else      move  "="            to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       ipc-nsd-eds-325.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * operazione                              *
      *                      *-----------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-nsd-eds-350
           else      go to ipc-nsd-eds-475.
       ipc-nsd-eds-350.
      *                      *-----------------------------------------*
      *                      * Se variabile 'nsd-edsnnn' esistente     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Record letto in area di ridefini-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      s-alf                to   w-ipc-nsd-eds-wrl      .
      *                          *-------------------------------------*
      *                          * Inizializzazione indice 01..07 per  *
      *                          * scansione 7 elementi  del record    *
      *                          *-------------------------------------*
           move      zero                 to   w-ipc-nsd-eds-i03      .
       ipc-nsd-eds-375.
      *                          *-------------------------------------*
      *                          * Incremento indice 01..07            *
      *                          *-------------------------------------*
           add       1                    to   w-ipc-nsd-eds-i03      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : a lettura re- *
      *                          * cord successivo                     *
      *                          *-------------------------------------*
           if        w-ipc-nsd-eds-i03    >    7
                     go to ipc-nsd-eds-250.
       ipc-nsd-eds-400.
      *                          *-------------------------------------*
      *                          * Se elemento da non considerare : a  *
      *                          * fine caricamento                    *
      *                          *-------------------------------------*
           if        w-ipc-nsd-eds-wrn
                    (w-ipc-nsd-eds-i03)   not  numeric
                     go to ipc-nsd-eds-500.
           if        w-ipc-nsd-eds-wrn
                    (w-ipc-nsd-eds-i03)   =    zero
                     go to ipc-nsd-eds-500.
       ipc-nsd-eds-425.
      *                          *-------------------------------------*
      *                          * Bufferizzazione elemento            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento numero elementi buf- *
      *                              * ferizzati                       *
      *                              *---------------------------------*
           add       1                    to   w-ipc-nsd-eds-i02      .
      *                              *---------------------------------*
      *                              * Memorizzazione                  *
      *                              *---------------------------------*
           move      w-ipc-nsd-eds-wrn
                    (w-ipc-nsd-eds-i03)   to   w-ipc-nsd-eds-nsc
                                              (w-ipc-nsd-eds-i02)     .
      *                              *---------------------------------*
      *                              * Se raggiunta la massima capaci- *
      *                              * ta' della tabella di bufferiz-  *
      *                              * zazione : a fine caricamento    *
      *                              *---------------------------------*
           if        w-ipc-nsd-eds-i02    =    999
                     go to ipc-nsd-eds-500.
       ipc-nsd-eds-450.
      *                          *-------------------------------------*
      *                          * Ad esame elemento successivo        *
      *                          *-------------------------------------*
           go to     ipc-nsd-eds-375.
       ipc-nsd-eds-475.
      *                      *-----------------------------------------*
      *                      * Se variabile 'nsd-edsnnn' non esistente *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A fine caricamento                  *
      *                          *-------------------------------------*
           go to     ipc-nsd-eds-500.
       ipc-nsd-eds-500.
      *                      *-----------------------------------------*
      *                      * Fine caricamento                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero effettivo di numeri scadenza *
      *                          * da escludere                        *
      *                          *-------------------------------------*
           move      w-ipc-nsd-eds-i02    to   w-ipc-nsd-eds-val      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     ipc-nsd-eds-999.
       ipc-nsd-eds-900.
      *              *-------------------------------------------------*
      *              * Se variabile 'nsd-eds' non esistente            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-nsd-eds-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-nsd-eds-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-nsd-eds-999.
       ipc-nsd-eds-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente : Si                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico : Si                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-cic      .
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
      *              *-------------------------------------------------*
      *              * Open file [sdb]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * Open file [cli]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Open file [dcc]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
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
      *              *-------------------------------------------------*
      *              * Close file [sdb]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * Close file [cli]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Close file [dcc]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
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
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           perform   acc-dpz-cli-000      thru acc-dpz-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da ricercare                  *
      *                  *---------------------------------------------*
           perform   acc-tip-sdb-000      thru acc-tip-sdb-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Data scadenza minima da ricercare           *
      *                  *---------------------------------------------*
           perform   acc-dts-min-000      thru acc-dts-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da ricercare          *
      *                  *---------------------------------------------*
           perform   acc-dts-max-000      thru acc-dts-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Importo da ricercare                        *
      *                  *---------------------------------------------*
           perform   acc-imp-sdb-000      thru acc-imp-sdb-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Status scadenze da ricercare                *
      *                  *---------------------------------------------*
           perform   acc-sts-sdb-000      thru acc-sts-sdb-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
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
           move      "#CNF"               to   v-not                  .
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
      *              * Prompt codice cliente                           *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice cliente                  *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice cliente, ragione sociale *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice cliente, indirizzo       *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-via-000  thru vis-cod-cli-via-999    .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice cliente, localita'       *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-loc-000  thru vis-cod-cli-loc-999    .
      *              *-------------------------------------------------*
      *              * Prompt codice dipendenza cliente                *
      *              *-------------------------------------------------*
           perform   pmt-dpz-cli-000      thru pmt-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice dipendenza del cliente   *
      *              *-------------------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice dipendenza del cliente,  *
      *              * ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice dipendenza del cliente,  *
      *              * indirizzo                                       *
      *              *-------------------------------------------------*
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice dipendenza del cliente,  *
      *              * localita'                                       *
      *              *-------------------------------------------------*
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *              *-------------------------------------------------*
      *              * Prompt tipi scadenza da ricercare               *
      *              *-------------------------------------------------*
           perform   pmt-tip-sdb-000      thru pmt-tip-sdb-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipi scadenza da ricercare      *
      *              *-------------------------------------------------*
           perform   vis-tip-sdb-000      thru vis-tip-sdb-999        .
      *              *-------------------------------------------------*
      *              * Prompt data scadenza minima da ricercare        *
      *              *-------------------------------------------------*
           perform   pmt-dts-min-000      thru pmt-dts-min-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data scadenza minima da ricer-  *
      *              * care                                            *
      *              *-------------------------------------------------*
           perform   vis-dts-min-000      thru vis-dts-min-999        .
      *              *-------------------------------------------------*
      *              * Prompt data scadenza massima da ricercare       *
      *              *-------------------------------------------------*
           perform   pmt-dts-max-000      thru pmt-dts-max-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data scadenza massima da ricer- *
      *              * care                                            *
      *              *-------------------------------------------------*
           perform   vis-dts-max-000      thru vis-dts-max-999        .
      *              *-------------------------------------------------*
      *              * Prompt importo scadenza da ricercare            *
      *              *-------------------------------------------------*
           perform   pmt-imp-sdb-000      thru pmt-imp-sdb-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione importo scadenza da ricercare   *
      *              *-------------------------------------------------*
           perform   vis-imp-sdb-000      thru vis-imp-sdb-999        .
      *              *-------------------------------------------------*
      *              * Prompt status scadenze da ricercare             *
      *              *-------------------------------------------------*
           perform   pmt-sts-sdb-000      thru pmt-sts-sdb-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione status scadenze da ricercare    *
      *              *-------------------------------------------------*
           perform   vis-sts-sdb-000      thru vis-sts-sdb-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt codice cliente                                     *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
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
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza del cliente     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipi scadenza da ricercare                         *
      *    *-----------------------------------------------------------*
       pmt-tip-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipi scadenza da ricercare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-sdb-999.
           exit.

      *    *===========================================================*
      *    * Prompt data scadenza minima da ricercare                  *
      *    *-----------------------------------------------------------*
       pmt-dts-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con data di scadenza  dal  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dts-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt data scadenza massima da ricercare                 *
      *    *-----------------------------------------------------------*
       pmt-dts-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                       al  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dts-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt importo da ricercare                               *
      *    *-----------------------------------------------------------*
       pmt-imp-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con importo scadenza       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-imp-sdb-999.
           exit.

      *    *===========================================================*
      *    * Prompt status scadenze da ricercare                       *
      *    *-----------------------------------------------------------*
       pmt-sts-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su status        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sts-sdb-999.
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
           move      05                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      05                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      06                   to   w-cod-mne-dcc-vln      .
           move      41                   to   w-cod-mne-dcc-vps      .
           move      07                   to   w-cod-mne-dcc-lln      .
           move      41                   to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
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
      *                  * Determinazione se cliente con dipendenze    *
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
           move      05                   to   v-lin                  .
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
           move      05                   to   v-lin                  .
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
           move      06                   to   v-lin                  .
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
           move      07                   to   v-lin                  .
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
           move      09                   to   w-cod-cod-dcc-lin      .
           move      30                   to   w-cod-cod-dcc-pos      .
           move      09                   to   w-cod-cod-dcc-rln      .
           move      41                   to   w-cod-cod-dcc-rps      .
           move      10                   to   w-cod-cod-dcc-vln      .
           move      41                   to   w-cod-cod-dcc-vps      .
           move      11                   to   w-cod-cod-dcc-lln      .
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
           move      09                   to   v-lin                  .
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
           move      09                   to   v-lin                  .
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
           move      10                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-cli-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipi scadenza da ricercare                 *
      *    *-----------------------------------------------------------*
       acc-tip-sdb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-sdb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsc-drc-lun    to   v-car                  .
           move      w-exp-tsc-drc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tsc-drc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-tip-sdb           =    00
                     move  01             to   v-num
           else if   rr-tip-sdb           =    01
                     move  02             to   v-num
           else if   rr-tip-sdb           =    02
                     move  03             to   v-num
           else if   rr-tip-sdb           =    03
                     move  04             to   v-num
           else if   rr-tip-sdb           =    04
                     move  05             to   v-num
           else if   rr-tip-sdb           =    05
                     move  06             to   v-num
           else if   rr-tip-sdb           =    06
                     move  07             to   v-num
           else if   rr-tip-sdb           =    07
                     move  08             to   v-num
           else if   rr-tip-sdb           =    08
                     move  09             to   v-num
           else if   rr-tip-sdb           =    09
                     move  10             to   v-num
           else if   rr-tip-sdb           =    10
                     move  11             to   v-num
           else if   rr-tip-sdb           =    11
                     move  12             to   v-num
           else if   rr-tip-sdb           =    61
                     move  13             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-sdb-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-sdb-999.
       acc-tip-sdb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  00             to   rr-tip-sdb
           else if   v-num                =    02
                     move  01             to   rr-tip-sdb
           else if   v-num                =    03
                     move  02             to   rr-tip-sdb
           else if   v-num                =    04
                     move  03             to   rr-tip-sdb
           else if   v-num                =    05
                     move  04             to   rr-tip-sdb
           else if   v-num                =    06
                     move  05             to   rr-tip-sdb
           else if   v-num                =    07
                     move  06             to   rr-tip-sdb
           else if   v-num                =    08
                     move  07             to   rr-tip-sdb
           else if   v-num                =    09
                     move  08             to   rr-tip-sdb
           else if   v-num                =    10
                     move  09             to   rr-tip-sdb
           else if   v-num                =    11
                     move  10             to   rr-tip-sdb
           else if   v-num                =    12
                     move  11             to   rr-tip-sdb
           else if   v-num                =    13
                     move  61             to   rr-tip-sdb
           else      move  zero           to   rr-tip-sdb             .
       acc-tip-sdb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-sdb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-sdb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-sdb-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-sdb-100.
       acc-tip-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipi scadenza da ricercare              *
      *    *-----------------------------------------------------------*
       vis-tip-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsc-drc-lun    to   v-car                  .
           move      w-exp-tsc-drc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tsc-drc-tbl    to   v-txt                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-tip-sdb           =    00
                     move  01             to   v-num
           else if   rr-tip-sdb           =    01
                     move  02             to   v-num
           else if   rr-tip-sdb           =    02
                     move  03             to   v-num
           else if   rr-tip-sdb           =    03
                     move  04             to   v-num
           else if   rr-tip-sdb           =    04
                     move  05             to   v-num
           else if   rr-tip-sdb           =    05
                     move  06             to   v-num
           else if   rr-tip-sdb           =    06
                     move  07             to   v-num
           else if   rr-tip-sdb           =    07
                     move  08             to   v-num
           else if   rr-tip-sdb           =    08
                     move  09             to   v-num
           else if   rr-tip-sdb           =    09
                     move  10             to   v-num
           else if   rr-tip-sdb           =    10
                     move  11             to   v-num
           else if   rr-tip-sdb           =    11
                     move  12             to   v-num
           else if   rr-tip-sdb           =    61
                     move  13             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-sdb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data scadenza minima da ricercare          *
      *    *-----------------------------------------------------------*
       acc-dts-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dts-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dts-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dts-min-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dts-min-999.
       acc-dts-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dts-min             .
       acc-dts-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dts-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dts-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dts-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dts-min-100.
       acc-dts-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data scadenza minima da ricercare       *
      *    *-----------------------------------------------------------*
       vis-dts-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dts-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dts-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data scadenza massima da ricercare         *
      *    *-----------------------------------------------------------*
       acc-dts-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dts-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dts-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dts-max-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dts-max-999.
       acc-dts-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dts-max             .
       acc-dts-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dts-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dts-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dts-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dts-max-100.
       acc-dts-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data scadenza massima da ricercare      *
      *    *-----------------------------------------------------------*
       vis-dts-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dts-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dts-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Importo scadenza da ricercare              *
      *    *-----------------------------------------------------------*
       acc-imp-sdb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-imp-sdb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-imp-sdb           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-imp-sdb-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-imp-sdb-999.
       acc-imp-sdb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-imp-sdb             .
       acc-imp-sdb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-imp-sdb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-imp-sdb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-imp-sdb-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-imp-sdb-100.
       acc-imp-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Importo scadenza da ricercare           *
      *    *-----------------------------------------------------------*
       vis-imp-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-imp-sdb           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-imp-sdb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Status scadenze da ricercare               *
      *    *-----------------------------------------------------------*
       acc-sts-sdb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sts-sdb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-sdb-lun    to   v-car                  .
           move      w-exp-sts-sdb-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-sdb-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sts-sdb           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sts-sdb-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sts-sdb-999.
       acc-sts-sdb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sts-sdb             .
       acc-sts-sdb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sts-sdb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sts-sdb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sts-sdb-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sts-sdb-100.
       acc-sts-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Status scadenze da ricercare            *
      *    *-----------------------------------------------------------*
       vis-sts-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-sdb-lun    to   v-car                  .
           move      w-exp-sts-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-sdb-tbl    to   v-txt                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sts-sdb           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-sts-sdb-999.
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
      *              * Controllo su codice cliente                     *
      *              *-------------------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca il codice cliente                           
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su tipo scadenza da ricercare         *
      *              *-------------------------------------------------*
           if        rr-tip-sdb           =    00 or
                     rr-tip-sdb           =    01 or
                     rr-tip-sdb           =    02 or
                     rr-tip-sdb           =    03 or
                     rr-tip-sdb           =    04 or
                     rr-tip-sdb           =    05 or
                     rr-tip-sdb           =    06 or
                     rr-tip-sdb           =    07 or
                     rr-tip-sdb           =    08 or
                     rr-tip-sdb           =    09 or
                     rr-tip-sdb           =    10 or
                     rr-tip-sdb           =    11 or
                     rr-tip-sdb           =    61
                     go to tdo-ric-sel-300.
           move      "Tipo scadenza da ricercare errato                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su data scadenza minima e massima     *
      *              *-------------------------------------------------*
           if        rr-dts-min           =    zero or
                     rr-dts-max           =    zero
                     go to tdo-ric-sel-400.
           if        rr-dts-max           not  < rr-dts-min
                     go to tdo-ric-sel-400.
           move      "Data scadenza massima inferiore alla data scadenza
      -              " minima        "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-400.
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
      *              * Status scadenze da ricercare                    *
      *              *-------------------------------------------------*
           if        rr-sts-sdb           =    zero
                     move  01             to   rr-sts-sdb             .
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
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-cli             .
      *                  *---------------------------------------------*
      *                  * Codice cliente, ragione sociale             *
      *                  *---------------------------------------------*
           move      spaces               to   rr-cod-cli-rag         .
      *                  *---------------------------------------------*
      *                  * Codice cliente, indirizzo                   *
      *                  *---------------------------------------------*
           move      spaces               to   rr-cod-cli-via         .
      *                  *---------------------------------------------*
      *                  * Codice cliente, localita'                   *
      *                  *---------------------------------------------*
           move      spaces               to   rr-cod-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dpz-cli             .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente, ragione so-  *
      *                  * ciale                                       *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dpz-cli-rag         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente, indirizzo    *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dpz-cli-via         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente, localita'    *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dpz-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Tipi di scadenza da ricercare               *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tip-sdb             .
      *                  *---------------------------------------------*
      *                  * Data scadenza minima da ricercare           *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dts-min             .
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da ricercare          *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dts-max             .
      *                  *---------------------------------------------*
      *                  * Importo da ricercare                        *
      *                  *---------------------------------------------*
           move      zero                 to   rr-imp-sdb             .
      *                  *---------------------------------------------*
      *                  * Status scadenze da selezionare              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-sts-sdb             .
       nor-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Preparazione defaults                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa : no preparazione default      *
      *                      *-----------------------------------------*
           if        w-ipc-cod-cli-snx    not  = "S"
                     go to nor-ric-sel-420.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * a zero : no preparazione default        *
      *                      *-----------------------------------------*
           if        w-ipc-cod-cli-val    =    zero
                     go to nor-ric-sel-420.
      *                      *-----------------------------------------*
      *                      * Lettura archivio [dcc]                  *
      *                      *-----------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-ipc-cod-cli-val    to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           if        w-let-arc-dcc-flg    not  = spaces
                     go to nor-ric-sel-420.
           move      w-ipc-cod-cli-val    to   rr-cod-cli             .
           move      w-let-arc-dcc-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-cod-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-cod-cli-loc         .
       nor-ric-sel-420.
      *                  *---------------------------------------------*
      *                  * Dipendenza cliente                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa : no preparazione default      *
      *                      *-----------------------------------------*
           if        w-ipc-dpz-cli-snx    not  = "S"
                     go to nor-ric-sel-440.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * a spaces : no preparazione default      *
      *                      *-----------------------------------------*
           if        w-ipc-dpz-cli-val    =    spaces
                     go to nor-ric-sel-440.
      *                      *-----------------------------------------*
      *                      * Lettura archivio [dcc]                  *
      *                      *-----------------------------------------*
           if        w-ipc-dpz-cli-val    =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      w-ipc-dpz-cli-val    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           if        w-let-arc-dcc-flg    not  = spaces
                     go to nor-ric-sel-440.
           move      w-ipc-dpz-cli-val    to   rr-dpz-cli             .
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
       nor-ric-sel-440.
      *                  *---------------------------------------------*
      *                  * Tipi di scadenza da ricercare               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa al tipo scadenza : no prepa-   *
      *                      * razione default                         *
      *                      *-----------------------------------------*
           if        w-ipc-tip-sdb-snx    not  = "S"
                     go to nor-ric-sel-460.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-tip-sdb-val    <    01 or
                     w-ipc-tip-sdb-val    >    11
                     go to nor-ric-sel-460.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-tip-sdb-val    to   rr-tip-sdb             .
      *                      *-----------------------------------------*
      *                      * Se il default preparato non corrisponde *
      *                      * ad un paghero' cambiario : continuazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        rr-tip-sdb           not  = 11
                     go to nor-ric-sel-460.
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa al tipo acquisizione scadenza  *
      *                      * : continuazione                         *
      *                      *-----------------------------------------*
           if        w-ipc-tac-sdb-snx    not  = "S"
                     go to nor-ric-sel-460.
      *                      *-----------------------------------------*
      *                      * Se il valore della variabile di i.p.c.  *
      *                      * non indica che la scadenza e' ststa ce- *
      *                      * duta da terzi : continuazione           *
      *                      *-----------------------------------------*
           if        w-ipc-tac-sdb-val    not  = 02
                     go to nor-ric-sel-460.
      *                      *-----------------------------------------*
      *                      * Modifica del default da 11 a 61         *
      *                      *-----------------------------------------*
           move      61                   to   rr-tip-sdb             .
       nor-ric-sel-460.
      *                  *---------------------------------------------*
      *                  * Status scadenze da ricercare                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa : no preparazione default      *
      *                      *-----------------------------------------*
           if        w-ipc-sts-sdb-snx    not  = "S"
                     go to nor-ric-sel-480.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-sts-sdb-val    <    01 or
                     w-ipc-sts-sdb-val    >    12
                     go to nor-ric-sel-480.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-sts-sdb-val    to   rr-sts-sdb             .
       nor-ric-sel-480.
      *                      *-----------------------------------------*
      *                      * Preparazione default in assenza di va-  *
      *                      * riabili di i.p.c.                       *
      *                      *-----------------------------------------*
           if        rr-sts-sdb           =    zero
                     move  01             to   rr-sts-sdb             .
       nor-ric-sel-500.
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
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di End-of-file relativo    *
      *              * alla ripristino Start                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-rip-str-sdb-eof      .
       qry-str-ini-100.
      *              *-------------------------------------------------*
      *              * Start su [sdb] per cliente                      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DBTDTS    "         to   f-key                  .
           move      01                   to   rf-sdb-tip-dbt         .
           move      rr-cod-cli           to   rf-sdb-cod-dbt         .
           move      rr-dts-min           to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della start *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to qry-str-ini-300
           else      go to qry-str-ini-200.
       qry-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se start errata                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Flag di End-of-file relativo al ripristino  *
      *                  * della Start                                 *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rip-str-sdb-eof      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-300.
      *              *-------------------------------------------------*
      *              * Se start Ok                                     *
      *              *-------------------------------------------------*
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
           move      "Nessuna scadenza entro i limiti assegnati !       
      -              "               "    to   w-err-box-err-msg      .
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
       qry-let-seq-100.
      *              *-------------------------------------------------*
      *              * Next su [sdb] per cliente                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to qry-let-seq-300
           else      go to qry-let-seq-200.
       qry-let-seq-200.
      *              *-------------------------------------------------*
      *              * Se fine file                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Flag di End-of-file relativo al ripristino  *
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rip-str-sdb-eof      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-let-seq-999.
       qry-let-seq-300.
      *              *-------------------------------------------------*
      *              * Se lettura Ok                                   *
      *              *-------------------------------------------------*
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
       qry-tst-max-100.
      *              *-------------------------------------------------*
      *              * Se fine cliente : superamento                   *
      *              *-------------------------------------------------*
           if        rf-sdb-tip-dbt       not  = 01
                     go to qry-tst-max-900.
           if        rf-sdb-cod-dbt       not  = rr-cod-cli
                     go to qry-tst-max-900.
       qry-tst-max-200.
      *              *-------------------------------------------------*
      *              * Se oltre data scadenza massima : superamento    *
      *              *-------------------------------------------------*
           if        rr-dts-max           =    zero
                     go to qry-tst-max-300.
           if        rf-sdb-dts-sdb       >    rr-dts-max
                     go to qry-tst-max-900.
       qry-tst-max-300.
      *              *-------------------------------------------------*
      *              * Fine test max                                   *
      *              *-------------------------------------------------*
           go to     qry-tst-max-999.
       qry-tst-max-900.
      *              *-------------------------------------------------*
      *              * Se fine file                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita con errore                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Flag di End-of-file relativo al ripristino  *
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rip-str-sdb-eof      .
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
      *              * Selezione su codice dipendenza del cliente      *
      *              *-------------------------------------------------*
           if        rr-dpz-cli           =    "*   "
                     go to qry-sel-rec-200.
           if        rf-sdb-dpz-dbt       =    rr-dpz-cli
                     go to qry-sel-rec-200
           else      go to qry-sel-rec-900.
       qry-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Selezione su tipo scadenza                      *
      *              *-------------------------------------------------*
       qry-sel-rec-205.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza da ricercare pari a zero : *
      *                  * nessuna selezione sul tipo scadenza         *
      *                  *---------------------------------------------*
           if        rr-tip-sdb           =    zero
                     go to qry-sel-rec-300.
       qry-sel-rec-210.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo scadenza let- *
      *                  * to                                          *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    11
                     go to qry-sel-rec-215
           else      go to qry-sel-rec-230.
       qry-sel-rec-215.
      *                  *---------------------------------------------*
      *                  * Se il tipo di scadenza letto corrisponde ad *
      *                  * un paghero' cambiario                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di acqui- *
      *                      * sizione della scadenza                  *
      *                      *-----------------------------------------*
           if        rf-sdb-tac-sdb       =    02
                     go to qry-sel-rec-225
           else      go to qry-sel-rec-220.
       qry-sel-rec-220.
      *                      *-----------------------------------------*
      *                      * Se la scadenza e' stata emessa diretta- *
      *                      * mente                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        rr-tip-sdb           =    11
                     go to qry-sel-rec-300
           else      go to qry-sel-rec-900.
       qry-sel-rec-225.
      *                      *-----------------------------------------*
      *                      * Se la scadenza e' stata ceduta da terzi *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        rr-tip-sdb           =    61
                     go to qry-sel-rec-300
           else      go to qry-sel-rec-900.
       qry-sel-rec-230.
      *                  *---------------------------------------------*
      *                  * Se il tipo di scadenza letto non corrispon- *
      *                  * de ad un paghero' cambiario                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       =    rr-tip-sdb
                     go to qry-sel-rec-300
           else      go to qry-sel-rec-900.
       qry-sel-rec-300.
      *              *-------------------------------------------------*
      *              * Selezione su data scadenza minima               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se data scadenza minima da ricercare pari   *
      *                  * a zero : nessuna selezione sulla data sca-  *
      *                  * denza minima                                *
      *                  *---------------------------------------------*
           if        rr-dts-min           =    zero
                     go to qry-sel-rec-400.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       not  < rr-dts-min
                     go to qry-sel-rec-400
           else      go to qry-sel-rec-900.
       qry-sel-rec-400.
      *              *-------------------------------------------------*
      *              * Selezione su data scadenza massima              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se data scadenza massima da ricercare pari  *
      *                  * a zero : nessuna selezione sulla data sca-  *
      *                  * denza massima                               *
      *                  *---------------------------------------------*
           if        rr-dts-max           =    zero
                     go to qry-sel-rec-500.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       not  > rr-dts-max
                     go to qry-sel-rec-500
           else      go to qry-sel-rec-900.
       qry-sel-rec-500.
      *              *-------------------------------------------------*
      *              * Selezione su importo da ricercare               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se importo da ricercare pari a zero : nes-  *
      *                  * suna selezione sull'importo                 *
      *                  *---------------------------------------------*
           if        rr-imp-sdb           =    zero
                     go to qry-sel-rec-600.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-sdb-imp-sdb       =    rr-imp-sdb
                     go to qry-sel-rec-600
           else      go to qry-sel-rec-900.
       qry-sel-rec-600.
      *              *-------------------------------------------------*
      *              * Selezione su status scadenza                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione status scadenza              *
      *                  *---------------------------------------------*
           perform   det-sts-sdb-000      thru det-sts-sdb-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della selezione su   *
      *                  * status scadenza                             *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-610
                     qry-sel-rec-620
                     qry-sel-rec-630
                     qry-sel-rec-640
                     qry-sel-rec-650
                     qry-sel-rec-660
                     qry-sel-rec-670
                     qry-sel-rec-680
                     qry-sel-rec-690
                     qry-sel-rec-700
                     qry-sel-rec-710
                     qry-sel-rec-720
                     depending            on   rr-sts-sdb             .
       qry-sel-rec-610.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle ancora aperte *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-sts    =    "A"
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-620.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Tutte                     *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-800.
       qry-sel-rec-630.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle solo emesse   *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "EMI" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-640.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle stornate      *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "SSC" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-650.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle riscosse      *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "RIS" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-660.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle pagate        *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "PAG" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-670.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle compensate    *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "CMP" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-680.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle presentate    *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "IID" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-690.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle insolute      *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "ISP" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-700.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle richiamate    *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "RSP" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-710.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle accreditate   *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "ACS" 
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-720.
      *                  *---------------------------------------------*
      *                  * Status scadenze : Solo quelle esitate       *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    =    "NBE" or
                     w-det-sts-sdb-suo    =    "PBE"
                     go to qry-sel-rec-800
           else      go to qry-sel-rec-900.
       qry-sel-rec-800.
      *              *-------------------------------------------------*
      *              * Controllo che la scadenza non faccia gia' parte *
      *              * del gruppo di scadenze appartenenti alla ri-    *
      *              * scossione in corso di formazione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' stata passata una variabile per   *
      *                  * i numeri scadenza appartenenti alla riscos- *
      *                  * sione in corso di formazione : no selezione *
      *                  *---------------------------------------------*
           if        w-ipc-nsd-eds-snx    not  = "S"
                     go to qry-sel-rec-880.
      *                  *---------------------------------------------*
      *                  * Se il numero scadenze appartenenti alla ri- *
      *                  * scossione in corso di formazione e' pari a  *
      *                  * zero : no selezione                         *
      *                  *---------------------------------------------*
           if        w-ipc-nsd-eds-val    =    zero
                     go to qry-sel-rec-880.
      *                  *---------------------------------------------*
      *                  * Controllo effettivo                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-nsd-eds-i02      .
       qry-sel-rec-820.
           add       1                    to   w-ipc-nsd-eds-i02      .
           if        w-ipc-nsd-eds-i02    >    w-ipc-nsd-eds-val
                     go to qry-sel-rec-880.
           if        w-ipc-nsd-eds-nsc
                    (w-ipc-nsd-eds-i02)   =    rf-sdb-num-sdb
                     go to qry-sel-rec-900
           else      go to qry-sel-rec-820.
       qry-sel-rec-880.
      *              *-------------------------------------------------*
      *              * Fine selezioni                                  *
      *              *-------------------------------------------------*
           go to     qry-sel-rec-999.
       qry-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Se selezione non superata                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione non superata              *
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
      *              * Intestazione per interrogazione                 *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
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
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        v-res                >    1
                     go to qry-liv-det-100.
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
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero scadenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "B"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-sdb-num-sdb       to   v-num                  .
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
      *
           if        rf-sdb-tip-sdb       =    01    and
                     rf-sdb-snx-dlc       =    "S"
                     move  "DC  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    01    and
                     rf-sdb-snx-cts       =    "S"
                     move  "CT  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    01    and
                     rf-sdb-snx-dlc       not  = "S" and
                     rf-sdb-snx-cts       not  = "S"
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
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       =    zero
                     go to qry-liv-det-210
           else      go to qry-liv-det-220.
       qry-liv-det-210.
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      " A vista"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-230.
       qry-liv-det-220.
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      rf-sdb-dts-sdb       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-230.
       qry-liv-det-230.
      *                  *---------------------------------------------*
      *                  * Importo scadenza in valuta base             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      29                   to   v-pos                  .
           move      rf-sdb-imp-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Status scadenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      46                   to   v-pos                  .
      *
           if        w-det-sts-sdb-suo    =    "EMI"
                     move  "Emessa     "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "SSC"
                     move  "Stornata   "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "RIS"
                     move  "Riscossa   "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "PAG"
                     move  "Pagata     "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "CMP"
                     move  "Compensata "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "IID"
                     move  "Presentata "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "RSP"
                     move  "Richiamata "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "ACS"
                     move  "Accreditata"  to   v-alf
           else if   w-det-sts-sdb-suo    =    "NBE"
                     move  "Esitata    "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "PBE"
                     move  "Esitata    "  to   v-alf
           else if   w-det-sts-sdb-suo    =    "ISP"
                     move  "Insoluta   "  to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           move      "+"                  to   v-edm                  .
           move      rf-sdb-num-sdb       to   w-mpn-num-sdb          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo documento di riferimento               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      59                   to   v-pos                  .
      *
           if        rf-sdb-tip-ddr       =    01
                     move  "FT"           to   v-alf
           else if   rf-sdb-tip-ddr       =    02
                     move  "NA"           to   v-alf
           else if   rf-sdb-tip-ddr       =    11
                     move  "NC"           to   v-alf
           else if   rf-sdb-tip-sdb       =    21
                     move  "AC"           to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento di riferimento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Allineamento a destra                   *
      *                      *-----------------------------------------*
           move      10                   to   w-all-str-lun          .
           move      rf-sdb-num-ddr       to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      62                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento di riferimento               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      73                   to   v-pos                  .
           move      rf-sdb-dat-ddr       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Fine stampa                                 *
      *                  *---------------------------------------------*
           go to     qry-liv-det-999.
       qry-liv-det-999.
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
       stp-int-pag-100.
      *              *-------------------------------------------------*
      *              * Prime tre linee                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "            SCADENZE CLIENTE            "
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
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-int-pag-200.
      *              *-------------------------------------------------*
      *              * Sub-intestazione per il cliente                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Composizione stringa                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-int-pag-x80      .
           move      1                    to   w-stp-int-pag-pnt      .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           string    "Cliente : "
                                delimited by   size
                                          into w-stp-int-pag-x80
                                  with pointer w-stp-int-pag-pnt      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * String                              *
      *                          *-------------------------------------*
           string    v-edt
                                delimited by   spaces
                                          into w-stp-int-pag-x80
                                  with pointer w-stp-int-pag-pnt      .
      *                      *-----------------------------------------*
      *                      * Dipendenza fornitore                    *
      *                      *-----------------------------------------*
           if        rr-dpz-cli           =    spaces or
                     rr-dpz-cli           =    "*   "
                     go to stp-int-pag-220.
           string    "-"
                                delimited by   size
                     rr-dpz-cli
                                delimited by   spaces
                                          into w-stp-int-pag-x80
                                  with pointer w-stp-int-pag-pnt      .
       stp-int-pag-220.
      *                      *-----------------------------------------*
      *                      * Ragione sociale cliente                 *
      *                      *-----------------------------------------*
           string    " "
                                delimited by   size
                     rr-cod-cli-rag
                                delimited by   size
                                          into w-stp-int-pag-x80
                                  with pointer w-stp-int-pag-pnt      .
      *                  *---------------------------------------------*
      *                  * Stampa stringa composta                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-int-pag-x80    to   v-alf                  .
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
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per fincatura                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "   Numero    Tipo  Scadenza      Importo        St
      -              "atus          Riferimenti     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per sottolineatura fincatura        *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-----------  ----  --------  --------------  -----
      -              "------  ----------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *                  * Se programma di espansione scadenze gia'    *
      *                  * attivo : no function key EXPD               *
      *                  *---------------------------------------------*
           move      "ESPSDB    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     move  spaces         to   v-pfk (02)
                     go to qry-det-fky-200.
      *                  *---------------------------------------------*
      *                  * Ammissibilita' del tasto Expd               *
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
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Scrittura della variabile 'num-sdb' per il  *
      *                  * livello di profondita' precedente o per lo  *
      *                  * stesso livello di profondita' applicativa a *
      *                  * seconda se il sottoprogramma e' stato ri-   *
      *                  * chiamato dal main oppure da un sottopro-    *
      *                  * gramma dello stesso livello                 *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
           if        w-ipc-tdc-mos-snx    =    "S" and
                     w-ipc-tdc-mos-val    =    "M"
                     move  "-"            to   s-dop
           else      move  "="            to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-num-sdb        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Segnale di interruzione interrogazione      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-int      .
      *                  *---------------------------------------------*
      *                  * Segnale di uscita dal programma             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-rou-pri      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-300.
      *              *-------------------------------------------------*
      *              * Se function-key Expd                            *
      *              *-------------------------------------------------*
       qry-trt-fun-305.
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       qry-trt-fun-310.
      *                  *---------------------------------------------*
      *                  * Se programma di espansione scadenze gia'    *
      *                  * attivo : uscita                             *
      *                  *---------------------------------------------*
           move      "ESPSDB    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     go to qry-trt-fun-999.
       qry-trt-fun-315.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname completo per l'esecu- *
      *                  * zione del sottoprogramma da richiamare, ri- *
      *                  * cercando nella tabella dei tipi interroga-  *
      *                  * zione; se non trovato : uscita              *
      *                  *---------------------------------------------*
           move      zero                 to   w-tin-ele-inx          .
       qry-trt-fun-320.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-num
                     go to qry-trt-fun-999.
           if        w-tin-alf-tin
                    (w-tin-ele-inx)       not  = "ESPSDB    "
                     go to qry-trt-fun-320.
           if        w-tin-ovy-tin
                    (w-tin-ele-inx)       =    spaces
                     go to qry-trt-fun-999.
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-ovy-exe-pos          .
       qry-trt-fun-325.
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'num-sdb' per *
      *                  * lo stesso livello di profondita' applicati- *
      *                  * va per il numero scadenza da espandere      *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-num-sdb        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       qry-trt-fun-330.
      *                  *---------------------------------------------*
      *                  * Salvataggio parametri per Start su [sdb]    *
      *                  *---------------------------------------------*
           if        w-rip-str-sdb-eof    not  = spaces
                     go to qry-trt-fun-332.
           move      rf-sdb-tip-dbt       to   w-rip-str-sdb-tip      .
           move      rf-sdb-cod-dbt       to   w-rip-str-sdb-cod      .
           move      rf-sdb-dts-sdb       to   w-rip-str-sdb-dts      .
           move      rf-sdb-num-sdb       to   w-rip-str-sdb-num      .
       qry-trt-fun-332.
      *                  *---------------------------------------------*
      *                  * Richiamo del sottoprogramma                 *
      *                  *---------------------------------------------*
           add       1                    to   w-ovy-exe-inx          .
           move      w-ovy-exe-pos        to   w-ovy-exe-spv
                                              (w-ovy-exe-inx)         .
           call      w-ovy-exe-pat       using i-ide
                                               w-ovy-exe
                                               w-tin
                                               w-spg                  .
       qry-trt-fun-335.
      *                  *---------------------------------------------*
      *                  * Cancellazione del sottoprogramma            *
      *                  *---------------------------------------------*
           move      w-ovy-exe-spv
                    (w-ovy-exe-inx)       to   w-ovy-exe-pos          .
           cancel    w-ovy-exe-pat                                    .
           subtract  1                    from w-ovy-exe-inx          .
       qry-trt-fun-340.
      *                  *---------------------------------------------*
      *                  * Se flag di fine file in On : uscita         *
      *                  *---------------------------------------------*
           if        w-rip-str-sdb-eof    not  = spaces
                     go to qry-trt-fun-390.
       qry-trt-fun-350.
      *                  *---------------------------------------------*
      *                  * Ripristino start                            *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DBTDTS    "         to   f-key                  .
           move      w-rip-str-sdb-tip    to   rf-sdb-tip-dbt         .
           move      w-rip-str-sdb-cod    to   rf-sdb-cod-dbt         .
           move      w-rip-str-sdb-dts    to   rf-sdb-dts-sdb         .
           move      w-rip-str-sdb-num    to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                      *-----------------------------------------*
      *                      * Test su esito start                     *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-trt-fun-380.
       qry-trt-fun-360.
      *                  *---------------------------------------------*
      *                  * Next su [sdb]                               *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                      *-----------------------------------------*
      *                      * Test su esito read next                 *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-trt-fun-380
           else      go to qry-trt-fun-390.
       qry-trt-fun-380.
      *                  *---------------------------------------------*
      *                  * Se ripristino non possibile                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di interruzione                    *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-int      .
       qry-trt-fun-390.
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
      *    * Determinazione status scadenza                            *
      *    *-----------------------------------------------------------*
       det-sts-sdb-000.
      *              *-------------------------------------------------*
      *              * Determinazione sigla ultima operazione eseguita *
      *              * sulla scadenza                                  *
      *              *-------------------------------------------------*
       det-sts-sdb-025.
      *                  *---------------------------------------------*
      *                  * Normalizzazione iniziale sigla              *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-sts-sdb-suo      .
       det-sts-sdb-050.
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-emi       not  = zero
                     move  "EMI"          to   w-det-sts-sdb-suo      .
       det-sts-sdb-075.
      *                  *---------------------------------------------*
      *                  * Storno                                      *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-sto       not  = zero
                     move  "SSC"          to   w-det-sts-sdb-suo
                     go to det-sts-sdb-500.
       det-sts-sdb-100.
      *                  *---------------------------------------------*
      *                  * Riscossione                                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-ris       =    zero
                     go to det-sts-sdb-125.
      *                      *-----------------------------------------*
      *                      * Determinazione in funizione delle moda- *
      *                      * lita' di riscossione                    *
      *                      *-----------------------------------------*
           if        rf-sdb-mod-ris       =    01 or
                     rf-sdb-mod-ris       =    02 or
                     rf-sdb-mod-ris       =    03 or
                     rf-sdb-mod-ris       =    04
                     move  "RIS"          to   w-det-sts-sdb-suo
           else if   rf-sdb-mod-ris       =    21 or
                     rf-sdb-mod-ris       =    22 or
                     rf-sdb-mod-ris       =    23 or
                     rf-sdb-mod-ris       =    24
                     move  "PAG"          to   w-det-sts-sdb-suo
           else if   rf-sdb-mod-ris       =    50
                     move  "CMP"          to   w-det-sts-sdb-suo      .
           go to     det-sts-sdb-500.
       det-sts-sdb-125.
      *                  *---------------------------------------------*
      *                  * Inclusione in distinta                      *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ddp       not  = zero
                     move  "IID"          to   w-det-sts-sdb-suo      .
       det-sts-sdb-175.
      *                  *---------------------------------------------*
      *                  * Richiamo della scadenza presentata          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-rsp       not  = zero
                     move  "RSP"          to   w-det-sts-sdb-suo
                     go to det-sts-sdb-500.
       det-sts-sdb-200.
      *                  *---------------------------------------------*
      *                  * Accredito scadenza al dopo incasso          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-acs       not  = zero
                     move  "ACS"          to   w-det-sts-sdb-suo
                     go to det-sts-sdb-500.
       det-sts-sdb-225.
      *                  *---------------------------------------------*
      *                  * Notizia di buon esito sulla scadenza        *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-nbe       not  = zero
                     move  "NBE"          to   w-det-sts-sdb-suo
                     go to det-sts-sdb-500.
       det-sts-sdb-250.
      *                  *---------------------------------------------*
      *                  * Presunto buon esito sulla scadenza          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-pbe       not  = zero
                     move  "PBE"          to   w-det-sts-sdb-suo
                     go to det-sts-sdb-500.
       det-sts-sdb-275.
      *                  *---------------------------------------------*
      *                  * Insoluto sulla scadenza presentata          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-isp       not  = zero
                     move  "ISP"          to   w-det-sts-sdb-suo
                     go to det-sts-sdb-500.
       det-sts-sdb-500.
      *              *-------------------------------------------------*
      *              * Determinazione dello status della scadenza in   *
      *              * funzione della sigla dell'ultima operazione e-  *
      *              * seguita sulla scadenza                          *
      *              *-------------------------------------------------*
           if        w-det-sts-sdb-suo    =    "EMI" or
                     w-det-sts-sdb-suo    =    "IID"
                     move  "A"            to   w-det-sts-sdb-sts
           else if   w-det-sts-sdb-suo    =    "SSC" or
                     w-det-sts-sdb-suo    =    "RIS" or
                     w-det-sts-sdb-suo    =    "PAG" or
                     w-det-sts-sdb-suo    =    "CMP" or
                     w-det-sts-sdb-suo    =    "RSP" or
                     w-det-sts-sdb-suo    =    "ACS" or
                     w-det-sts-sdb-suo    =    "NBE" or
                     w-det-sts-sdb-suo    =    "PBE" or
                     w-det-sts-sdb-suo    =    "ISP"
                     move  "C"            to   w-det-sts-sdb-sts
           else      move  spaces         to   w-det-sts-sdb-sts      .
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento in caso di insoluto *
      *                  *---------------------------------------------*
           if        w-det-sts-sdb-suo    not  = "ISP"
                     go to det-sts-sdb-999.
      *                      *-----------------------------------------*
      *                      * Se la scadenza e' comunque stata ri-    *
      *                      * scossa : tutto inalterato               *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-ris       not  = zero
                     go to det-sts-sdb-999.
      *                      *-----------------------------------------*
      *                      * Se a fronte dell'insoluto non e' stata  *
      *                      * emessa una nuova scadenza, essa e' da   *
      *                      * intendersi ancora aperta                *
      *                      *-----------------------------------------*
           if        rf-sdb-ens-isp       =    02   and
                     rf-sdb-nns-isp       =    zero
                     move  "A"            to   w-det-sts-sdb-sts      .
       det-sts-sdb-999.
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

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .
