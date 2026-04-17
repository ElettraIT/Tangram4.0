       Identification Division.
       Program-Id.                                 pgep3014           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    mov                 *
      *                                   Fase:    gep301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/01/92    *
      *                       Ultima revisione:    NdK del 14/09/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti per gestione   *
      *                    portafoglio                                 *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Ricerca scadenze per inclusione in distinta *
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
                     "RICERCA SCADENZE PER FORMAZIONE DISTINTA"       .

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
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [axc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxc"                          .

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
      *        * Per data composizione distinta                        *
      *        *-------------------------------------------------------*
           05  w-ipc-dtr-com.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla data di   *
      *            * composizione distinta                             *
      *            *---------------------------------------------------*
               10  w-ipc-dtr-com-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * data di composizione distinta                     *
      *            *---------------------------------------------------*
               10  w-ipc-dtr-com-val      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per numero distinta                                   *
      *        *-------------------------------------------------------*
           05  w-ipc-num-ddp.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al numero di-  *
      *            * stinta                                            *
      *            *---------------------------------------------------*
               10  w-ipc-num-ddp-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al nu-  *
      *            * mero distinta                                     *
      *            *---------------------------------------------------*
               10  w-ipc-num-ddp-val      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Per tipo distinta                                     *
      *        *-------------------------------------------------------*
           05  w-ipc-tip-ddp.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * distinta                                          *
      *            *---------------------------------------------------*
               10  w-ipc-tip-ddp-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al ti-  *
      *            * po di distinta                                    *
      *            *---------------------------------------------------*
               10  w-ipc-tip-ddp-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per tipo avviso richiesto per incassi elettronici     *
      *        *-------------------------------------------------------*
           05  w-ipc-tav-ric.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * avviso richiesto per incassi elettronici          *
      *            *---------------------------------------------------*
               10  w-ipc-tav-ric-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al ti-  *
      *            * po di avviso richiesto per incassi elettronici    *
      *            *---------------------------------------------------*
               10  w-ipc-tav-ric-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per tipi scadenza da includere in distinta            *
      *        *-------------------------------------------------------*
           05  w-ipc-tsc-das.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa ai tipi di     *
      *            * scadenza da includere in distinta                 *
      *            *---------------------------------------------------*
               10  w-ipc-tsc-das-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa ai ti-  *
      *            * pi di scadenza da includere in distinta           *
      *            *---------------------------------------------------*
               10  w-ipc-tsc-das-val      pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Per data scadenza minima da selezionare               *
      *        *-------------------------------------------------------*
           05  w-ipc-dts-min.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla data di   *
      *            * scadenza minima da selezionare                    *
      *            *---------------------------------------------------*
               10  w-ipc-dts-min-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * data di scadenza minima da selezionare            *
      *            *---------------------------------------------------*
               10  w-ipc-dts-min-val      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per data scadenza massima da selezionare              *
      *        *-------------------------------------------------------*
           05  w-ipc-dts-max.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla data di   *
      *            * scadenza massima da selezionare                   *
      *            *---------------------------------------------------*
               10  w-ipc-dts-max-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * data di scadenza massima da selezionare           *
      *            *---------------------------------------------------*
               10  w-ipc-dts-max-val      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per si/no scadenze a vista da selezionare             *
      *        *-------------------------------------------------------*
           05  w-ipc-dts-vis.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla scelta    *
      *            * si/no scadenze a vista da selezionare             *
      *            *---------------------------------------------------*
               10  w-ipc-dts-vis-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * scelta si/no scadenze a vista da selezionare      *
      *            *---------------------------------------------------*
               10  w-ipc-dts-vis-val      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per selezione su circuiti interbancari                *
      *        *-------------------------------------------------------*
           05  w-ipc-cir-itb.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla selezione *
      *            * su circuiti interbancari                          *
      *            *---------------------------------------------------*
               10  w-ipc-cir-itb-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * selezione su circuiti interbancari                *
      *            *---------------------------------------------------*
               10  w-ipc-cir-itb-val.
                   15  w-ipc-cir-itb-vlx
                               occurs 05  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Per importo massimo da selezionare                    *
      *        *-------------------------------------------------------*
           05  w-ipc-imp-max.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa all'importo    *
      *            * massimo da selezionare                            *
      *            *---------------------------------------------------*
               10  w-ipc-imp-max-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa all'im- *
      *            * porto massimo da selezionare                      *
      *            *---------------------------------------------------*
               10  w-ipc-imp-max-val      pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Per numeri scadenza da escludere dalla selezione in   *
      *        * quanto gia' appartenenti ad una distinta di presen-   *
      *        * tazione in corso di composizione                      *
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
      *        * Data composizione distinta                            *
      *        *-------------------------------------------------------*
           05  rr-dtr-com                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero distinta                                       *
      *        *-------------------------------------------------------*
           05  rr-num-ddp                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Tipo distinta di presentazione                        *
      *        * - 01 : Distinta Incassi Elettronici                   *
      *        * - 02 : Distinta Effetti                               *
      *        * - 03 : Distinta Paghero' Cambiari                     *
      *        * - 04 : Distinta Cessioni                              *
      *        *-------------------------------------------------------*
           05  rr-tip-ddp                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo avviso richiesto                                 *
      *        * - 01 : Conferme d'ordine oppure Ri.Ba.                *
      *        * - 02 : Conferme d'ordine oppure M.Av.                 *
      *        * - 03 : Solo Ri.Ba.                                    *
      *        * - 04 : Solo M.Av.                                     *
      *        * - 05 : Solo R.I.D.                                    *
      *        *-------------------------------------------------------*
           05  rr-tav-ric                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipi scadenza da includere in distinta                *
      *        * - 0002 : Incassi elettronici                          *
      *        * - 0003 : Ri.Ba.                                       *
      *        * - 0004 : C.d.O.                                       *
      *        * - 0005 : M.Av.                                        *
      *        * - 0006 : R.I.D.                                       *
      *        * - 0009 : Ricevute bancarie                            *
      *        * - 0010 : Tratte                                       *
      *        * - 0011 : Paghero' cambiari                            *
      *        * - 0061 : Paghero' cambiari avuti in cessione          *
      *        * - 0203 : Incassi elettronici e Ri.Ba.                 *
      *        * - 0204 : Incassi elettronici e C.d.O.                 *
      *        * - 0205 : Incassi elettronici e M.Av.                  *
      *        * - 0910 : Ricevute bancarie e Tratte                   *
      *        *-------------------------------------------------------*
           05  rr-tsc-das                 pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Data scadenza minima da selezionare                   *
      *        *-------------------------------------------------------*
           05  rr-dts-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data scadenza massima da selezionare                  *
      *        *-------------------------------------------------------*
           05  rr-dts-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/No scadenze a vista da selezionare                 *
      *        * - S : Si                                              *
      *        * - N : No                                              *
      *        *-------------------------------------------------------*
           05  rr-dts-vis                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Selezione su circuiti interbancari                    *
      *        *-------------------------------------------------------*
           05  rr-cir-itb.
               10  rr-cod-cib  occurs 05  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Importo massimo da selezionare                        *
      *        *-------------------------------------------------------*
           05  rr-imp-max                 pic s9(13)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio debitori                     *
      *        *-------------------------------------------------------*
           05  w-let-arc-dbt.
               10  w-let-arc-dbt-flg      pic  x(01)                  .
               10  w-let-arc-dbt-tip      pic  9(02)                  .
               10  w-let-arc-dbt-cod      pic  9(07)                  .
               10  w-let-arc-dbt-rag      pic  x(40)                  .
               10  w-let-arc-dbt-via      pic  x(40)                  .
               10  w-let-arc-dbt-loc      pic  x(40)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axi.
               10  w-let-arc-axi-flg      pic  x(01)                  .
               10  w-let-arc-axi-cod      pic  9(05)                  .
               10  w-let-arc-axi-den      pic  x(40)                  .
               10  w-let-arc-axi-cib.
                   15  w-let-arc-axi-cic
                               occurs 05  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axs.
               10  w-let-arc-axs-flg      pic  x(01)                  .
               10  w-let-arc-axs-abi      pic  9(05)                  .
               10  w-let-arc-axs-cab      pic  9(05)                  .
               10  w-let-arc-axs-den      pic  x(40)                  .
               10  w-let-arc-axs-via      pic  x(40)                  .
               10  w-let-arc-axs-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axc.
               10  w-let-arc-axc-flg      pic  x(01)                  .
               10  w-let-arc-axc-cod      pic  x(02)                  .
               10  w-let-arc-axc-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazione se tutte le richieste sono    *
      *        * state preparate da variabili di i.p.c. oppure no      *
      *        *-------------------------------------------------------*
           05  w-det-ric-ipc.
      *            *---------------------------------------------------*
      *            * Esito determinazione                              *
      *            * - S : Si, tutte preparate da i.p.c.               *
      *            * - N   No, non tutte preparate da i.p.c.           *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Data composizione distinta            *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-dcd      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Numero distinta                       *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-nmd      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Tipo di distinta                      *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-tdd      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Tipo avviso richiesto                 *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-tar      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Tipi scadenza da includere            *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-tsi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Data scadenza minima                  *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-dmi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Data scadenza massima                 *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-dma      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Data scadenza a vista                 *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-dvi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Circuiti interbancari                 *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-cir      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Richiesta : Importo massimo da selezionare        *
      *            *---------------------------------------------------*
               10  w-det-ric-ipc-max      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo distinta di presentazione             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ddp.
               10  w-exp-tip-ddp-num      pic  9(02)       value 04   .
               10  w-exp-tip-ddp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ddp-tbl.
                   15  filler             pic  x(40) value
                            "distinta Incassi elettronici            ".
                   15  filler             pic  x(40) value
                            "distinta Effetti                        ".
                   15  filler             pic  x(40) value
                            "distinta Paghero' cambiari              ".
                   15  filler             pic  x(40) value
                            "distinta Cessioni                       ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo avviso richiesto in presentazione     *
      *        *-------------------------------------------------------*
           05  w-exp-tav-ric.
               10  w-exp-tav-ric-num      pic  9(02)       value 05   .
               10  w-exp-tav-ric-lun      pic  9(02)       value 40   .
               10  w-exp-tav-ric-tbl.
                   15  filler             pic  x(40) value
                            "CDO, o in alternativa RIBA              ".
                   15  filler             pic  x(40) value
                            "CDO, o in alternativa MAV               ".
                   15  filler             pic  x(40) value
                            "Tutte RIBA                              ".
                   15  filler             pic  x(40) value
                            "Tutti MAV                               ".
                   15  filler             pic  x(40) value
                            "Tutti RID                               ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo scadenze da selezionare               *
      *        *-------------------------------------------------------*
           05  w-exp-tsc-das.
               10  w-exp-tsc-das-num      pic  9(02)                  .
               10  w-exp-tsc-das-lun      pic  9(02)       value 40   .
               10  w-exp-tsc-das-tbl.
                   15  w-exp-tsc-das-ele
                               occurs 05  pic  x(40)                  .
               10  w-exp-tsc-das-val.
                   15  w-exp-tsc-das-vel
                               occurs 05  pic  9(04)                  .
               10  w-exp-tsc-das-inx      pic  9(02)                  .
      *
               10  w-exp-tsc-das-n01      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t01.
                   15  filler             pic  x(40) value
                            "Solo di tipo IE                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v01.
                   15  filler             pic  9(04) value 0002       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n02      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t02.
                   15  filler             pic  x(40) value
                            "Solo di tipo IE                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v02.
                   15  filler             pic  9(04) value 0002       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n03      pic  9(02)       value 02   .
               10  w-exp-tsc-das-t03.
                   15  filler             pic  x(40) value
                            "Di tipo IE e RIBA                       ".
                   15  filler             pic  x(40) value
                            "Solo di tipo RIBA                       ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v03.
                   15  filler             pic  9(04) value 0203       .
                   15  filler             pic  9(04) value 0003       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n04      pic  9(02)       value 02   .
               10  w-exp-tsc-das-t04.
                   15  filler             pic  x(40) value
                            "Di tipo IE e MAV                        ".
                   15  filler             pic  x(40) value
                            "Solo di tipo MAV                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v04.
                   15  filler             pic  9(04) value 0205       .
                   15  filler             pic  9(04) value 0005       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n05      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t05.
                   15  filler             pic  x(40) value
                            "Solo di tipo RID                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v05.
                   15  filler             pic  9(04) value 0006       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n06      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t06.
                   15  filler             pic  x(40) value
                            "Di tipo RB e TR                         ".
                   15  filler             pic  x(40) value
                            "Solo di tipo RB                         ".
                   15  filler             pic  x(40) value
                            "Solo di tipo TR                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v06.
                   15  filler             pic  9(04) value 0910       .
                   15  filler             pic  9(04) value 0009       .
                   15  filler             pic  9(04) value 0010       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n07      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t07.
                   15  filler             pic  x(40) value
                            "Solo di tipo PC                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v07.
                   15  filler             pic  9(04) value 0011       .
                   15  filler             pic  9(04) value 0009       .
                   15  filler             pic  9(04) value 0010       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n08      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t08.
                   15  filler             pic  x(40) value
                            "Solo di tipo PC avuti in cessione       ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v08.
                   15  filler             pic  9(04) value 0061       .
                   15  filler             pic  9(04) value 0009       .
                   15  filler             pic  9(04) value 0010       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su scadenze a vista              *
      *        *-------------------------------------------------------*
           05  w-exp-dts-vis.
               10  w-exp-dts-vis-num      pic  9(02)       value 02   .
               10  w-exp-dts-vis-lun      pic  9(02)       value 02   .
               10  w-exp-dts-vis-tbl.
                   15  filler             pic  x(02) value
                            "Si"                                      .
                   15  filler             pic  x(02) value
                            "No"                                      .

      *    *===========================================================*
      *    * Work per accettazioni particolari                         *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per : Circuiti interbancari                      *
      *        *-------------------------------------------------------*
           05  w-acc-cir-itb.
      *            *---------------------------------------------------*
      *            * Indice 1..5                                       *
      *            *---------------------------------------------------*
               10  w-acc-cir-itb-inx      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Altri indici di comodo                            *
      *            *---------------------------------------------------*
               10  w-acc-cir-itb-i01      pic  9(02)                  .
               10  w-acc-cir-itb-i02      pic  9(02)                  .
               10  w-acc-cir-itb-i03      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Flag di compattazione elementi eseguita           *
      *            * - Spaces : non necessaria compattazione           *
      *            * - #      : compattazione eseguita                 *
      *            *---------------------------------------------------*
               10  w-acc-cir-itb-fce      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * 5 elementi editati                                *
      *            *---------------------------------------------------*
               10  w-acc-cir-itb-edt.
                   15  w-acc-cir-itb-edy occurs 05.
                       20  w-acc-cir-itb-edx
                                          pic  x(02)                  .
                       20  filler         pic  x(02)                  .

      *    *===========================================================*
      *    * Work per visualizzazioni particolari                      *
      *    *-----------------------------------------------------------*
       01  w-vis.
      *        *-------------------------------------------------------*
      *        * Work per : Circuiti interbancari                      *
      *        *-------------------------------------------------------*
           05  w-vis-cir-itb.
      *            *---------------------------------------------------*
      *            * 5 elementi editati                                *
      *            *---------------------------------------------------*
               10  w-vis-cir-itb-edt.
                   15  w-vis-cir-itb-edy occurs 05.
                       20  w-vis-cir-itb-edx
                                          pic  x(02)                  .
                       20  filler         pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Indice 1..5                                       *
      *            *---------------------------------------------------*
               10  w-vis-cir-itb-inx      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per lettura sequenziale scadenze                *
      *    *-----------------------------------------------------------*
       01  w-let-seq-sdb.
      *        *-------------------------------------------------------*
      *        * Flag per lettura scadenze a vista                     *
      *        * - Spaces : non ancora completata                      *
      *        * - #      : completata                                 *
      *        *-------------------------------------------------------*
           05  w-let-seq-sdb-lsv          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Accumulo per importo massimo da selezionare           *
      *        *-------------------------------------------------------*
           05  w-let-seq-sdb-igm          pic s9(13)                  .

      *    *===========================================================*
      *    * Work per selezione scadenza letta per inclusione in di-   *
      *    * stinta, nella fase di composizione distinta               *
      *    *-----------------------------------------------------------*
       01  w-sel-sdb.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-sel-sdb-inp.
      *            *---------------------------------------------------*
      *            * Data composizione distinta                        *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-dcd      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero distinta                                   *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-nmd      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo di distinta                                  *
      *            * - 01 : Distinta Incassi Elettronici               *
      *            * - 02 : Distinta Effetti                           *
      *            * - 03 : Distinta Paghero' Cambiari                 *
      *            * - 04 : Distinta Cessioni                          *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-tdd      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo di avviso richiesto se distinta per incassi  *
      *            * elettronici                                       *
      *            * - 01 : Conferme d'ordine, oppure Ri.Ba.           *
      *            * - 02 : Conferme d'ordine, oppure M.Av.            *
      *            * - 03 : Solo Ri.Ba.                                *
      *            * - 04 : Solo M.Av.                                 *
      *            * - 05 : Solo R.I.D.                                *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-tar      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo di scadenza da includere in distinta         *
      *            * - 0002 : Incassi elettronici                      *
      *            * - 0003 : Ri.Ba.                                   *
      *            * - 0004 : C.d.O.                                   *
      *            * - 0005 : M.Av.                                    *
      *            * - 0006 : R.I.D.                                   *
      *            * - 0009 : Ricevute bancarie                        *
      *            * - 0010 : Tratte                                   *
      *            * - 0011 : Paghero' cambiari                        *
      *            * - 0061 : Paghero' cambiari avuti in cessione      *
      *            * - 0203 : Incassi elettronici e Ri.Ba.             *
      *            * - 0204 : Incassi elettronici e C.d.O.             *
      *            * - 0205 : Incassi elettronici e M.Av.              *
      *            * - 0910 : Ricevute bancarie e Tratte               *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-tsd      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Data scadenza minima da selezionare               *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-smi      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data scadenza massima da selezionare              *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-sma      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Anche scadenze a vista da selezionare             *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-asv      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Selezione su circuiti interbancari                *
      *            *---------------------------------------------------*
               10  w-sel-sdb-inp-cib.
                   15  w-sel-sdb-inp-cic
                                occurs 05 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in output                                   *
      *        *-------------------------------------------------------*
           05  w-sel-sdb-out.
      *            *---------------------------------------------------*
      *            * Flag fondamentale di uscita                       *
      *            * - 00 : Selezione effettuata                       *
      *            * - nn : Selezione non effettuata                   *
      *            *---------------------------------------------------*
               10  w-sel-sdb-out-flg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Messaggio di errore se flag diverso da 00         *
      *            *---------------------------------------------------*
               10  w-sel-sdb-out-err      pic  x(65)                  .
      *        *-------------------------------------------------------*
      *        * Work per selezione di tipo 700                        *
      *        *-------------------------------------------------------*
           05  w-sel-sdb-700.
               10  w-sel-sdb-700-i01      pic  9(02)                  .
               10  w-sel-sdb-700-i02      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice circuito interbancario  *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acdeaxc0.acl"                   .

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
      *              * la data composizione distinta                   *
      *              *-------------------------------------------------*
           perform   ipc-dtr-com-000      thru ipc-dtr-com-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il numero distinta                              *
      *              *-------------------------------------------------*
           perform   ipc-num-ddp-000      thru ipc-num-ddp-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo di distinta                             *
      *              *-------------------------------------------------*
           perform   ipc-tip-ddp-000      thru ipc-tip-ddp-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo avviso richiesto per incassi elettroni- *
      *              * ci                                              *
      *              *-------------------------------------------------*
           perform   ipc-tav-ric-000      thru ipc-tav-ric-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * i tipi scadenza da includere in distinta        *
      *              *-------------------------------------------------*
           perform   ipc-tsc-das-000      thru ipc-tsc-das-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la data scadenza minima da selezionare          *
      *              *-------------------------------------------------*
           perform   ipc-dts-min-000      thru ipc-dts-min-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la data scadenza massima da selezionare         *
      *              *-------------------------------------------------*
           perform   ipc-dts-max-000      thru ipc-dts-max-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la scelta si/no scadenze a vista da selezionare *
      *              *-------------------------------------------------*
           perform   ipc-dts-vis-000      thru ipc-dts-vis-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la selezione su circuiti interbancari           *
      *              *-------------------------------------------------*
           perform   ipc-cir-itb-000      thru ipc-cir-itb-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * l'importo massimo da selezionare                *
      *              *-------------------------------------------------*
           perform   ipc-imp-max-000      thru ipc-imp-max-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * l'esclusione di certi numeri scadenza dalla se- *
      *              * lezione, in quanto gia' appartenenti ad una di- *
      *              * stinta di presentazione in corso di composizio- *
      *              * ne                                              *
      *              *-------------------------------------------------*
           perform   ipc-nsd-eds-000      thru ipc-nsd-eds-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "INTIID    "         to   w-spg-alf-gat          .
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
           move      "INTIID    "         to   w-spg-alf-gat          .
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
      *    * composizione distinta                                     *
      *    *-----------------------------------------------------------*
       ipc-dtr-com-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dtr-com' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dtr-com"            to   s-var                  .
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
                     go to ipc-dtr-com-200
           else      go to ipc-dtr-com-400.
       ipc-dtr-com-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dtr-com-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-dat                to   w-ipc-dtr-com-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dtr-com-999.
       ipc-dtr-com-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dtr-com-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-dtr-com-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dtr-com-999.
       ipc-dtr-com-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il numero *
      *    * distinta                                                  *
      *    *-----------------------------------------------------------*
       ipc-num-ddp-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'num-ddp' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-ddp"            to   s-var                  .
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
                     go to ipc-num-ddp-200
           else      go to ipc-num-ddp-400.
       ipc-num-ddp-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-num-ddp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-num-ddp-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-ddp-999.
       ipc-num-ddp-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-num-ddp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-num-ddp-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-ddp-999.
       ipc-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di distinta                                               *
      *    *-----------------------------------------------------------*
       ipc-tip-ddp-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tip-ddp' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tip-ddp"            to   s-var                  .
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
                     go to ipc-tip-ddp-200
           else      go to ipc-tip-ddp-400.
       ipc-tip-ddp-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tip-ddp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-tip-ddp-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tip-ddp-999.
       ipc-tip-ddp-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tip-ddp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-tip-ddp-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tip-ddp-999.
       ipc-tip-ddp-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di avviso richiesto per Incassi Elettronici               *
      *    *-----------------------------------------------------------*
       ipc-tav-ric-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tav-ric' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tav-ric"            to   s-var                  .
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
                     go to ipc-tav-ric-200
           else      go to ipc-tav-ric-400.
       ipc-tav-ric-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tav-ric-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-tav-ric-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tav-ric-999.
       ipc-tav-ric-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tav-ric-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-tav-ric-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tav-ric-999.
       ipc-tav-ric-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per i tipi    *
      *    * scadenza da includere in distinta                         *
      *    *-----------------------------------------------------------*
       ipc-tsc-das-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tsc-das' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tsc-das"            to   s-var                  .
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
                     go to ipc-tsc-das-200
           else      go to ipc-tsc-das-400.
       ipc-tsc-das-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tsc-das-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tsc-das-999.
       ipc-tsc-das-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tsc-das-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tsc-das-999.
       ipc-tsc-das-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per la data   *
      *    * scadenza minima da selezionare                            *
      *    *-----------------------------------------------------------*
       ipc-dts-min-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dts-min' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dts-min"            to   s-var                  .
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
                     go to ipc-dts-min-200
           else      go to ipc-dts-min-400.
       ipc-dts-min-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dts-min-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-dat                to   w-ipc-dts-min-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dts-min-999.
       ipc-dts-min-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dts-min-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-dts-min-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dts-min-999.
       ipc-dts-min-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per la data   *
      *    * scadenza massima da selezionare                           *
      *    *-----------------------------------------------------------*
       ipc-dts-max-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dts-max' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dts-max"            to   s-var                  .
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
                     go to ipc-dts-max-200
           else      go to ipc-dts-max-400.
       ipc-dts-max-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dts-max-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-dat                to   w-ipc-dts-max-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dts-max-999.
       ipc-dts-max-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dts-max-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-dts-max-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dts-max-999.
       ipc-dts-max-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per la scelta *
      *    * si/no selezione anche delle scadenze a vista              *
      *    *-----------------------------------------------------------*
       ipc-dts-vis-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dts-vis' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dts-vis"            to   s-var                  .
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
                     go to ipc-dts-vis-200
           else      go to ipc-dts-vis-400.
       ipc-dts-vis-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dts-vis-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-dts-vis-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dts-vis-999.
       ipc-dts-vis-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dts-vis-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-dts-vis-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dts-vis-999.
       ipc-dts-vis-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per la sele-  *
      *    * zione sui circuiti interbancari                           *
      *    *-----------------------------------------------------------*
       ipc-cir-itb-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'cir-itb' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cir-itb"            to   s-var                  .
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
                     go to ipc-cir-itb-200
           else      go to ipc-cir-itb-400.
       ipc-cir-itb-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-cir-itb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-cir-itb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cir-itb-999.
       ipc-cir-itb-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-cir-itb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-cir-itb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cir-itb-999.
       ipc-cir-itb-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per l'importo *
      *    * massimo da selezionare                                    *
      *    *-----------------------------------------------------------*
       ipc-imp-max-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'imp-max' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "imp-max"            to   s-var                  .
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
                     go to ipc-imp-max-200
           else      go to ipc-imp-max-400.
       ipc-imp-max-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-imp-max-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-imp-max-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-imp-max-999.
       ipc-imp-max-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-imp-max-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-imp-max-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-imp-max-999.
       ipc-imp-max-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per i numeri  *
      *    * scadenza da escludere dalla selezione in quanto gia' ap-  *
      *    * partenenti alla distinta in corso di formazione           *
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
      *              * Preparazione area richieste da variabili di i.  *
      *              * p.c. lette, con determinazione se tutte prepa-  *
      *              * rate oppure no                                  *
      *              *-------------------------------------------------*
           perform   pre-ric-ipc-000      thru pre-ric-ipc-999        .
       pre-tip-fun-100.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tutte le richieste necessarie sono state *
      *                  * preparate per mezzo di variabili di i.p.c.  *
      *                  * : No richieste; altrimenti : Si richieste   *
      *                  *---------------------------------------------*
           if        w-det-ric-ipc-snx    =    "S"
                     move  "N"            to   w-cnt-fun-snx-ric
           else      move  "S"            to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico : No                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-cic      .
       pre-tip-fun-400.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico : Si             *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Preparazione area richieste da variabili di i.p.c., con   *
      *    * determinazione se tutte preparate oppure no               *
      *    *-----------------------------------------------------------*
       pre-ric-ipc-000.
      *              *-------------------------------------------------*
      *              * Data composizione distinta                      *
      *              *-------------------------------------------------*
       pre-ric-ipc-005.
      *                  *---------------------------------------------*
      *                  * Test se variabile passata oppure no         *
      *                  *---------------------------------------------*
           if        w-ipc-dtr-com-snx    =    "S"
                     go to pre-ric-ipc-010
           else      go to pre-ric-ipc-020.
       pre-ric-ipc-010.
      *                  *---------------------------------------------*
      *                  * Se variabile passata                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore accettabile              *
      *                      *-----------------------------------------*
           if        w-ipc-dtr-com-val    not  = zero
                     go to pre-ric-ipc-015
           else      go to pre-ric-ipc-020.
       pre-ric-ipc-015.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-dcd      .
           move      w-ipc-dtr-com-val    to   rr-dtr-com             .
           go to     pre-ric-ipc-050.
       pre-ric-ipc-020.
      *                  *---------------------------------------------*
      *                  * Se variabile non passata                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-dcd      .
           move      zero                 to   rr-dtr-com             .
           go to     pre-ric-ipc-050.
       pre-ric-ipc-050.
      *              *-------------------------------------------------*
      *              * Numero distinta                                 *
      *              *-------------------------------------------------*
       pre-ric-ipc-055.
      *                  *---------------------------------------------*
      *                  * Test se variabile passata oppure no         *
      *                  *---------------------------------------------*
           if        w-ipc-num-ddp-snx    =    "S"
                     go to pre-ric-ipc-060
           else      go to pre-ric-ipc-070.
       pre-ric-ipc-060.
      *                  *---------------------------------------------*
      *                  * Se variabile passata                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore accettabile              *
      *                      *-----------------------------------------*
           if        w-ipc-num-ddp-val    not  = zero
                     go to pre-ric-ipc-065
           else      go to pre-ric-ipc-070.
       pre-ric-ipc-065.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-nmd      .
           move      w-ipc-num-ddp-val    to   rr-num-ddp             .
           go to     pre-ric-ipc-100.
       pre-ric-ipc-070.
      *                  *---------------------------------------------*
      *                  * Se variabile non passata                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-nmd      .
           move      zero                 to   rr-num-ddp             .
           go to     pre-ric-ipc-100.
       pre-ric-ipc-100.
      *              *-------------------------------------------------*
      *              * Tipo di distinta                                *
      *              *-------------------------------------------------*
       pre-ric-ipc-105.
      *                  *---------------------------------------------*
      *                  * Test se variabile passata oppure no         *
      *                  *---------------------------------------------*
           if        w-ipc-tip-ddp-snx    =    "S"
                     go to pre-ric-ipc-110
           else      go to pre-ric-ipc-120.
       pre-ric-ipc-110.
      *                  *---------------------------------------------*
      *                  * Se variabile passata                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore accettabile              *
      *                      *-----------------------------------------*
           if        w-ipc-tip-ddp-val    =    01 or
                     w-ipc-tip-ddp-val    =    02 or
                     w-ipc-tip-ddp-val    =    03 or
                     w-ipc-tip-ddp-val    =    04
                     go to pre-ric-ipc-115
           else      go to pre-ric-ipc-120.
       pre-ric-ipc-115.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tdd      .
           move      w-ipc-tip-ddp-val    to   rr-tip-ddp             .
           go to     pre-ric-ipc-200.
       pre-ric-ipc-120.
      *                  *---------------------------------------------*
      *                  * Se variabile non passata                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-tdd      .
           move      zero                 to   rr-tip-ddp             .
           go to     pre-ric-ipc-200.
       pre-ric-ipc-200.
      *              *-------------------------------------------------*
      *              * Tipo di avviso richiesto                        *
      *              *-------------------------------------------------*
       pre-ric-ipc-205.
      *                  *---------------------------------------------*
      *                  * Test se variabile necessaria oppure no      *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to pre-ric-ipc-210
           else      go to pre-ric-ipc-230.
       pre-ric-ipc-210.
      *                  *---------------------------------------------*
      *                  * Se variabile necessaria                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se variabile passata oppure no     *
      *                      *-----------------------------------------*
           if        w-ipc-tav-ric-snx    =    "S"
                     go to pre-ric-ipc-215
           else      go to pre-ric-ipc-225.
       pre-ric-ipc-215.
      *                      *-----------------------------------------*
      *                      * Se variabile passata                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se valore accettabile          *
      *                          *-------------------------------------*
           if        w-ipc-tav-ric-val    =    01 or
                     w-ipc-tav-ric-val    =    02 or
                     w-ipc-tav-ric-val    =    03 or
                     w-ipc-tav-ric-val    =    04 or
                     w-ipc-tav-ric-val    =    05
                     go to pre-ric-ipc-220
           else      go to pre-ric-ipc-225.
       pre-ric-ipc-220.
      *                          *-------------------------------------*
      *                          * Preparazione                        *
      *                          *-------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tar      .
           move      w-ipc-tav-ric-val    to   rr-tav-ric             .
           go to     pre-ric-ipc-300.
       pre-ric-ipc-225.
      *                      *-----------------------------------------*
      *                      * Se variabile non passata                *
      *                      *-----------------------------------------*
           move      "N"                  to   w-det-ric-ipc-tar      .
           move      zero                 to   rr-tav-ric             .
           go to     pre-ric-ipc-300.
       pre-ric-ipc-230.
      *                  *---------------------------------------------*
      *                  * Se variabile non necessaria                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tar      .
           move      zero                 to   rr-tav-ric             .
           go to     pre-ric-ipc-300.
       pre-ric-ipc-300.
      *              *-------------------------------------------------*
      *              * Tipi scadenza da includere                      *
      *              *-------------------------------------------------*
       pre-ric-ipc-305.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo distinta    *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to pre-ric-ipc-310
           else if   rr-tip-ddp           =    02
                     go to pre-ric-ipc-350
           else if   rr-tip-ddp           =    03
                     go to pre-ric-ipc-355
           else if   rr-tip-ddp           =    04
                     go to pre-ric-ipc-360
           else      go to pre-ric-ipc-365.
       pre-ric-ipc-310.
      *                  *---------------------------------------------*
      *                  * Se tipo distinta passato : 01               *
      *                  *---------------------------------------------*
       pre-ric-ipc-315.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo avviso  *
      *                      * richiesto                               *
      *                      *-----------------------------------------*
           if        rr-tav-ric           =    01
                     go to pre-ric-ipc-320
           else if   rr-tav-ric           =    02
                     go to pre-ric-ipc-325
           else if   rr-tav-ric           =    03
                     go to pre-ric-ipc-330
           else if   rr-tav-ric           =    04
                     go to pre-ric-ipc-335
           else if   rr-tav-ric           =    05
                     go to pre-ric-ipc-340
           else      go to pre-ric-ipc-345.
       pre-ric-ipc-320.
      *                      *-----------------------------------------*
      *                      * Se tipo avviso richiesto : 01           *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      0002                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-325.
      *                      *-----------------------------------------*
      *                      * Se tipo avviso richiesto : 02           *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      0002                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-330.
      *                      *-----------------------------------------*
      *                      * Se tipo avviso richiesto : 03           *
      *                      *-----------------------------------------*
           if        w-ipc-tsc-das-snx    not  = "S"
                     go to pre-ric-ipc-345.
           if        w-ipc-tsc-das-val    not  = 0203 and
                     w-ipc-tsc-das-val    not  = 0002
                     go to pre-ric-ipc-345.
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      w-ipc-tsc-das-val    to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-335.
      *                      *-----------------------------------------*
      *                      * Se tipo avviso richiesto : 04           *
      *                      *-----------------------------------------*
           if        w-ipc-tsc-das-snx    not  = "S"
                     go to pre-ric-ipc-345.
           if        w-ipc-tsc-das-val    not  = 0205 and
                     w-ipc-tsc-das-val    not  = 0005
                     go to pre-ric-ipc-345.
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      w-ipc-tsc-das-val    to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-340.
      *                      *-----------------------------------------*
      *                      * Se tipo avviso richiesto : 05           *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      0006                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-345.
      *                      *-----------------------------------------*
      *                      * Se tipo avviso richiesto non passato    *
      *                      *-----------------------------------------*
           move      "N"                  to   w-det-ric-ipc-tsi      .
           move      zero                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-350.
      *                  *---------------------------------------------*
      *                  * Se tipo distinta passato : 02               *
      *                  *---------------------------------------------*
           if        w-ipc-tsc-das-snx    not  = "S"
                     go to pre-ric-ipc-365.
           if        w-ipc-tsc-das-val    not  = 0910 and
                     w-ipc-tsc-das-val    not  = 0009 and
                     w-ipc-tsc-das-val    not  = 0010
                     go to pre-ric-ipc-365.
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      w-ipc-tsc-das-val    to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-355.
      *                  *---------------------------------------------*
      *                  * Se tipo distinta passato : 03               *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      0011                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-360.
      *                  *---------------------------------------------*
      *                  * Se tipo distinta passato : 04               *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-tsi      .
           move      0061                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-365.
      *                  *---------------------------------------------*
      *                  * Se tipo distinta non passato                *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-tsi      .
           move      zero                 to   rr-tsc-das             .
           go to     pre-ric-ipc-400.
       pre-ric-ipc-400.
      *              *-------------------------------------------------*
      *              * Data scadenza minima                            *
      *              *-------------------------------------------------*
       pre-ric-ipc-405.
      *                  *---------------------------------------------*
      *                  * Test se variabile passata oppure no         *
      *                  *---------------------------------------------*
           if        w-ipc-dts-min-snx    =    "S"
                     go to pre-ric-ipc-410
           else      go to pre-ric-ipc-415.
       pre-ric-ipc-410.
      *                  *---------------------------------------------*
      *                  * Se variabile passata                        *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-dmi      .
           move      w-ipc-dts-min-val    to   rr-dts-min             .
           go to     pre-ric-ipc-500.
       pre-ric-ipc-415.
      *                  *---------------------------------------------*
      *                  * Se variabile non passata                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-dmi      .
           move      zero                 to   rr-dts-min             .
           go to     pre-ric-ipc-500.
       pre-ric-ipc-500.
      *              *-------------------------------------------------*
      *              * Data scadenza massima                           *
      *              *-------------------------------------------------*
       pre-ric-ipc-505.
      *                  *---------------------------------------------*
      *                  * Test se variabile passata oppure no         *
      *                  *---------------------------------------------*
           if        w-ipc-dts-max-snx    =    "S"
                     go to pre-ric-ipc-510
           else      go to pre-ric-ipc-515.
       pre-ric-ipc-510.
      *                  *---------------------------------------------*
      *                  * Se variabile passata                        *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-dma      .
           move      w-ipc-dts-max-val    to   rr-dts-max             .
           if        rr-dts-min           =    zero or
                     rr-dts-max           =    zero
                     go to pre-ric-ipc-600.
           if        rr-dts-max           not  < rr-dts-min
                     go to pre-ric-ipc-600
           else      go to pre-ric-ipc-515.
       pre-ric-ipc-515.
      *                  *---------------------------------------------*
      *                  * Se variabile non passata                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-dma      .
           move      zero                 to   rr-dts-max             .
           go to     pre-ric-ipc-600.
       pre-ric-ipc-600.
      *              *-------------------------------------------------*
      *              * Si/No scadenze a vista                          *
      *              *-------------------------------------------------*
       pre-ric-ipc-605.
      *                  *---------------------------------------------*
      *                  * Test se variabile necessaria oppure no      *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    02 or
                     rr-tip-ddp           =    03 or
                     rr-tip-ddp           =    04 
                     go to pre-ric-ipc-610
           else      go to pre-ric-ipc-630.
       pre-ric-ipc-610.
      *                  *---------------------------------------------*
      *                  * Se variabile necessaria                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se variabile passata oppure no     *
      *                      *-----------------------------------------*
           if        w-ipc-dts-vis-snx    =    "S"
                     go to pre-ric-ipc-615
           else      go to pre-ric-ipc-625.
       pre-ric-ipc-615.
      *                      *-----------------------------------------*
      *                      * Se variabile passata                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se valore accettabile          *
      *                          *-------------------------------------*
           if        w-ipc-dts-vis-val    =    "S" or
                     w-ipc-dts-vis-val    =    "N"
                     go to pre-ric-ipc-620
           else      go to pre-ric-ipc-625.
       pre-ric-ipc-620.
      *                          *-------------------------------------*
      *                          * Preparazione                        *
      *                          *-------------------------------------*
           move      "S"                  to   w-det-ric-ipc-dvi      .
           move      w-ipc-dts-vis-val    to   rr-dts-vis             .
           go to     pre-ric-ipc-700.
       pre-ric-ipc-625.
      *                      *-----------------------------------------*
      *                      * Se variabile non passata                *
      *                      *-----------------------------------------*
           move      "N"                  to   w-det-ric-ipc-dvi      .
           move      zero                 to   rr-dts-vis             .
           go to     pre-ric-ipc-700.
       pre-ric-ipc-630.
      *                  *---------------------------------------------*
      *                  * Se variabile non necessaria                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-dvi      .
           move      spaces               to   rr-dts-vis             .
           go to     pre-ric-ipc-700.
       pre-ric-ipc-700.
      *              *-------------------------------------------------*
      *              * Selezione su circuiti interbancari              *
      *              *-------------------------------------------------*
       pre-ric-ipc-705.
      *                  *---------------------------------------------*
      *                  * Test se variabile necessaria oppure no      *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to pre-ric-ipc-710
           else      go to pre-ric-ipc-725.
       pre-ric-ipc-710.
      *                  *---------------------------------------------*
      *                  * Se variabile necessaria                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se variabile passata oppure no     *
      *                      *-----------------------------------------*
           if        w-ipc-cir-itb-snx    =    "S"
                     go to pre-ric-ipc-715
           else      go to pre-ric-ipc-720.
       pre-ric-ipc-715.
      *                      *-----------------------------------------*
      *                      * Se variabile passata                    *
      *                      *-----------------------------------------*
           move      "S"                  to   w-det-ric-ipc-cir      .
           move      w-ipc-cir-itb-val    to   rr-cir-itb             .
           go to     pre-ric-ipc-800.
       pre-ric-ipc-720.
      *                      *-----------------------------------------*
      *                      * Se variabile non passata                *
      *                      *-----------------------------------------*
           move      "N"                  to   w-det-ric-ipc-cir      .
           move      zero                 to   rr-cir-itb             .
           go to     pre-ric-ipc-800.
       pre-ric-ipc-725.
      *                  *---------------------------------------------*
      *                  * Se variabile non necessaria                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-cir      .
           move      spaces               to   rr-cir-itb             .
           go to     pre-ric-ipc-800.
       pre-ric-ipc-800.
      *              *-------------------------------------------------*
      *              * Importo massimo da selezionare                  *
      *              *-------------------------------------------------*
       pre-ric-ipc-805.
      *                  *---------------------------------------------*
      *                  * Test se variabile passata oppure no         *
      *                  *---------------------------------------------*
           if        w-ipc-imp-max-snx    =    "S"
                     go to pre-ric-ipc-810
           else      go to pre-ric-ipc-815.
       pre-ric-ipc-810.
      *                  *---------------------------------------------*
      *                  * Se variabile passata                        *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-ric-ipc-max      .
           move      w-ipc-dts-min-val    to   rr-imp-max             .
           go to     pre-ric-ipc-900.
       pre-ric-ipc-815.
      *                  *---------------------------------------------*
      *                  * Se variabile non passata                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-ric-ipc-max      .
           move      zero                 to   rr-imp-max             .
           go to     pre-ric-ipc-900.
       pre-ric-ipc-900.
      *              *-------------------------------------------------*
      *              * Determinazione se tutte le richieste necessarie *
      *              * sono state passate per mezzo di variabili di    *
      *              * i.p.c. oppure no                                *
      *              *-------------------------------------------------*
           if        w-det-ric-ipc-dcd    =    "S" and
                     w-det-ric-ipc-nmd    =    "S" and
                     w-det-ric-ipc-tdd    =    "S" and
                     w-det-ric-ipc-tar    =    "S" and
                     w-det-ric-ipc-tsi    =    "S" and
                     w-det-ric-ipc-dmi    =    "S" and
                     w-det-ric-ipc-dma    =    "S" and
                     w-det-ric-ipc-dvi    =    "S" and
                     w-det-ric-ipc-cir    =    "S" and
                     w-det-ric-ipc-max    =    "S"
                     move  "S"            to   w-det-ric-ipc-snx
           else      move  "N"            to   w-det-ric-ipc-snx      .
       pre-ric-ipc-999.
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
      *              * Open file [ddp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
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
      *              *-------------------------------------------------*
      *              * Open file [axi]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * Open file [axs]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * Open file [axc]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axc                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice circuito in-    *
      *              * terbancario                                     *
      *              *-------------------------------------------------*
           perform   cod-des-axc-opn-000  thru cod-des-axc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * Close file [ddp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
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
      *              *-------------------------------------------------*
      *              * Close file [axi]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * Close file [axs]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * Close file [axc]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axc                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice circuito in-   *
      *              * terbancario                                     *
      *              *-------------------------------------------------*
           perform   cod-des-axc-cls-000  thru cod-des-axc-cls-999    .
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
      *                  * Data composizione distinta                  *
      *                  *---------------------------------------------*
           perform   acc-dtr-com-000      thru acc-dtr-com-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-125.
      *                  *---------------------------------------------*
      *                  * Numero distinta                             *
      *                  *---------------------------------------------*
           perform   acc-num-ddp-000      thru acc-num-ddp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Tipo distinta                               *
      *                  *---------------------------------------------*
           perform   acc-tip-ddp-000      thru acc-tip-ddp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-125.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo avviso richiesto                       *
      *                  *---------------------------------------------*
           perform   acc-tav-ric-000      thru acc-tav-ric-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da includere in distinta      *
      *                  *---------------------------------------------*
           perform   acc-tsc-das-000      thru acc-tsc-das-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data scadenza minima da selezionare         *
      *                  *---------------------------------------------*
           perform   acc-dts-min-000      thru acc-dts-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da selezionare        *
      *                  *---------------------------------------------*
           perform   acc-dts-max-000      thru acc-dts-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Si/No scadenze a vista da selezionare       *
      *                  *---------------------------------------------*
           perform   acc-dts-vis-000      thru acc-dts-vis-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Selezione su circuiti interbancari          *
      *                  *---------------------------------------------*
           perform   acc-cir-itb-000      thru acc-cir-itb-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Importo massimo da selezionare              *
      *                  *---------------------------------------------*
           perform   acc-imp-max-000      thru acc-imp-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
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
           move      "Conferma esecuzione ricerca scadenze (S/N/E) ?"
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
      *              * Prompt data composizione distinta               *
      *              *-------------------------------------------------*
           perform   pmt-dtr-com-000      thru pmt-dtr-com-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data composizione distinta      *
      *              *-------------------------------------------------*
           perform   vis-dtr-com-000      thru vis-dtr-com-999        .
      *              *-------------------------------------------------*
      *              * Prompt numero distinta                          *
      *              *-------------------------------------------------*
           perform   pmt-num-ddp-000      thru pmt-num-ddp-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione numero distinta                 *
      *              *-------------------------------------------------*
           perform   vis-num-ddp-000      thru vis-num-ddp-999        .
      *              *-------------------------------------------------*
      *              * Prompt tipo distinta                            *
      *              *-------------------------------------------------*
           perform   pmt-tip-ddp-000      thru pmt-tip-ddp-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo distinta                   *
      *              *-------------------------------------------------*
           perform   vis-tip-ddp-000      thru vis-tip-ddp-999        .
      *              *-------------------------------------------------*
      *              * Prompt tipo avviso richiesto                    *
      *              *-------------------------------------------------*
           perform   pmt-tav-ric-000      thru pmt-tav-ric-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo avviso richiesto           *
      *              *-------------------------------------------------*
           perform   vis-tav-ric-000      thru vis-tav-ric-999        .
      *              *-------------------------------------------------*
      *              * Prompt tipi scadenza da includere in distinta   *
      *              *-------------------------------------------------*
           perform   pmt-tsc-das-000      thru pmt-tsc-das-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipi scadenza da includere in   *
      *              * distinta                                        *
      *              *-------------------------------------------------*
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
      *              *-------------------------------------------------*
      *              * Prompt data scadenza minima da selezionare      *
      *              *-------------------------------------------------*
           perform   pmt-dts-min-000      thru pmt-dts-min-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data scadenza minima da sele-   *
      *              * zionare                                         *
      *              *-------------------------------------------------*
           perform   vis-dts-min-000      thru vis-dts-min-999        .
      *              *-------------------------------------------------*
      *              * Prompt data scadenza massima da selezionare     *
      *              *-------------------------------------------------*
           perform   pmt-dts-max-000      thru pmt-dts-max-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data scadenza massima da sele-  *
      *              * zionare                                         *
      *              *-------------------------------------------------*
           perform   vis-dts-max-000      thru vis-dts-max-999        .
      *              *-------------------------------------------------*
      *              * Prompt si/no scadenze a vista da selezionare    *
      *              *-------------------------------------------------*
           perform   pmt-dts-vis-000      thru pmt-dts-vis-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione si/no scadenze a vista da sele- *
      *              * zionare                                         *
      *              *-------------------------------------------------*
           perform   vis-dts-vis-000      thru vis-dts-vis-999        .
      *              *-------------------------------------------------*
      *              * Prompt selezione su circuiti interbancari       *
      *              *-------------------------------------------------*
           perform   pmt-cir-itb-000      thru pmt-cir-itb-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione selezione su circuiti interban- *
      *              * cari                                            *
      *              *-------------------------------------------------*
           perform   vis-cir-itb-000      thru vis-cir-itb-999        .
      *              *-------------------------------------------------*
      *              * Prompt importo massimo da selezionare           *
      *              *-------------------------------------------------*
           perform   pmt-imp-max-000      thru pmt-imp-max-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione importo massimo da selezionare  *
      *              *-------------------------------------------------*
           perform   vis-imp-max-000      thru vis-imp-max-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt data composizione distinta                         *
      *    *-----------------------------------------------------------*
       pmt-dtr-com-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data composizione distinta :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dtr-com-999.
           exit.

      *    *===========================================================*
      *    * Prompt numero distinta                                    *
      *    *-----------------------------------------------------------*
       pmt-num-ddp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Numero distinta :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo distinta                                      *
      *    *-----------------------------------------------------------*
       pmt-tip-ddp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo distinta              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ddp-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo avviso richiesto                              *
      *    *-----------------------------------------------------------*
       pmt-tav-ric-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo avviso richiesto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tav-ric-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipi scadenza da includere in distinta             *
      *    *-----------------------------------------------------------*
       pmt-tsc-das-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipi scadenza da includere :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               in distinta  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tsc-das-999.
           exit.

      *    *===========================================================*
      *    * Prompt data scadenza minima da selezionare                *
      *    *-----------------------------------------------------------*
       pmt-dts-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con data di scadenza  dal  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dts-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt data scadenza massima da selezionare               *
      *    *-----------------------------------------------------------*
       pmt-dts-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                       al  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dts-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt si/no scadenze a vista da selezionare              *
      *    *-----------------------------------------------------------*
       pmt-dts-vis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Anche scadenze a vista     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dts-vis-999.
           exit.

      *    *===========================================================*
      *    * Prompt selezione su circuiti interbancari                 *
      *    *-----------------------------------------------------------*
       pmt-cir-itb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sel. su circuiti interbanc.:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cir-itb-999.
           exit.

      *    *===========================================================*
      *    * Prompt importo massimo da selezionare                     *
      *    *-----------------------------------------------------------*
       pmt-imp-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo massimo scadenze   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          da selezionare    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-imp-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data composizione distinta                 *
      *    *-----------------------------------------------------------*
       acc-dtr-com-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore gia' presente in quanto passato   *
      *                  * per mezzo della variabile di i.p.c. : no    *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
           if        rr-dtr-com           not  = zero and
                     w-ipc-dtr-com-snx    =    "S"
                     go to acc-dtr-com-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-dtr-com           not  = zero
                     go to acc-dtr-com-100.
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rr-dtr-com             .
           perform   vis-dtr-com-000      thru vis-dtr-com-999        .
       acc-dtr-com-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dtr-com           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dtr-com-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dtr-com-999.
       acc-dtr-com-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dtr-com             .
       acc-dtr-com-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che non sia uguale a zero                   *
      *                  *---------------------------------------------*
           if        rr-dtr-com           =    zero
                     go to acc-dtr-com-100.
       acc-dtr-com-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dtr-com-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dtr-com-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dtr-com-100.
       acc-dtr-com-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data composizione distinta              *
      *    *-----------------------------------------------------------*
       vis-dtr-com-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dtr-com           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dtr-com-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data composizione distinta                 *
      *    *-----------------------------------------------------------*
       acc-num-ddp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore gia' presente in quanto passato   *
      *                  * per mezzo della variabile di i.p.c. : no    *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
           if        rr-num-ddp           not  = zero and
                     w-ipc-num-ddp-snx    =    "S"
                     go to acc-num-ddp-999.
       acc-num-ddp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-ddp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-num-ddp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-ddp-999.
       acc-num-ddp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-ddp             .
       acc-num-ddp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-ddp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-ddp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-ddp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-ddp-100.
       acc-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero distinta                         *
      *    *-----------------------------------------------------------*
       vis-num-ddp-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      rr-num-ddp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo distinta                              *
      *    *-----------------------------------------------------------*
       acc-tip-ddp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore gia' presente in quanto passato   *
      *                  * per mezzo della variabile di i.p.c. : no    *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           not  = zero and
                     w-ipc-tip-ddp-snx    =    "S"
                     go to acc-tip-ddp-999.
       acc-tip-ddp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddp-lun    to   v-car                  .
           move      w-exp-tip-ddp-num    to   v-ldt                  .
           move      "IEPC#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ddp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-ddp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ddp-999.
       acc-tip-ddp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-ddp             .
       acc-tip-ddp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che sia un valore accettabile          *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    zero           or
                     rr-tip-ddp           >    w-exp-tip-ddp-num
                     go to acc-tip-ddp-100.
       acc-tip-ddp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ddp-625.
      *                  *---------------------------------------------*
      *                  * Tipo avviso richiesto                       *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to acc-tip-ddp-650.
           if        rr-tav-ric           =    zero
                     go to acc-tip-ddp-650.
           move      zero                 to   rr-tav-ric             .
           perform   vis-tav-ric-000      thru vis-tav-ric-999        .
           go to     acc-tip-ddp-650.
       acc-tip-ddp-650.
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da includere in distinta      *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to acc-tip-ddp-660
           else if   rr-tip-ddp           =    02
                     go to acc-tip-ddp-670
           else if   rr-tip-ddp           =    03
                     go to acc-tip-ddp-680
           else if   rr-tip-ddp           =    04
                     go to acc-tip-ddp-690.
       acc-tip-ddp-660.
           if        rr-tav-ric           =    01
                     go to acc-tip-ddp-661
           else if   rr-tav-ric           =    02
                     go to acc-tip-ddp-662
           else if   rr-tav-ric           =    03
                     go to acc-tip-ddp-663
           else if   rr-tav-ric           =    04
                     go to acc-tip-ddp-664
           else if   rr-tav-ric           =    05
                     go to acc-tip-ddp-665
           else      go to acc-tip-ddp-666.
       acc-tip-ddp-661.
           if        rr-tsc-das           =    0002
                     go to acc-tip-ddp-700.
           move      0002                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-662.
           if        rr-tsc-das           =    0002
                     go to acc-tip-ddp-700.
           move      0002                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-663.
           if        rr-tsc-das           =    0203 or
                     rr-tsc-das           =    0003 or
                     rr-tsc-das           =    zero
                     go to acc-tip-ddp-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-664.
           if        rr-tsc-das           =    0205 or
                     rr-tsc-das           =    0005 or
                     rr-tsc-das           =    zero
                     go to acc-tip-ddp-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-665.
           if        rr-tsc-das           =    0006
                     go to acc-tip-ddp-700.
           move      0006                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-666.
           if        rr-tsc-das           =    zero
                     go to acc-tip-ddp-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-670.
           if        rr-tsc-das           =    0910 or
                     rr-tsc-das           =    0009 or
                     rr-tsc-das           =    0010 or
                     rr-tsc-das           =    zero
                     go to acc-tip-ddp-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-680.
           if        rr-tsc-das           =    0011
                     go to acc-tip-ddp-700.
           move      0011                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-690.
           if        rr-tsc-das           =    0061
                     go to acc-tip-ddp-700.
           move      0061                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tip-ddp-700.
       acc-tip-ddp-700.
      *                  *---------------------------------------------*
      *                  * Si/No scadenze a vista da selezionare       *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    02 or
                     rr-tip-ddp           =    03 or
                     rr-tip-ddp           =    04
                     go to acc-tip-ddp-725.
           if        rr-dts-vis           =    spaces
                     go to acc-tip-ddp-725.
           move      spaces               to   rr-dts-vis             .
           perform   vis-dts-vis-000      thru vis-dts-vis-999        .
           go to     acc-tip-ddp-725.
       acc-tip-ddp-725.
      *                  *---------------------------------------------*
      *                  * Selezione su circuiti interbancari          *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to acc-tip-ddp-750.
           if        rr-cir-itb           =    spaces
                     go to acc-tip-ddp-750.
           move      spaces               to   rr-cir-itb             .
           perform   vis-cir-itb-000      thru vis-cir-itb-999        .
           go to     acc-tip-ddp-750.
       acc-tip-ddp-750.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-tip-ddp-800.
       acc-tip-ddp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ddp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ddp-100.
       acc-tip-ddp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo distinta                           *
      *    *-----------------------------------------------------------*
       vis-tip-ddp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddp-lun    to   v-car                  .
           move      w-exp-tip-ddp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddp-tbl    to   v-txt                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ddp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-ddp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo avviso richiesto                      *
      *    *-----------------------------------------------------------*
       acc-tav-ric-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore gia' presente in quanto passato   *
      *                  * per mezzo della variabile di i.p.c. : no    *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
           if        rr-tav-ric           not  = zero and
                     w-ipc-tav-ric-snx    =    "S"
                     go to acc-tav-ric-999.
      *                  *---------------------------------------------*
      *                  * Se tipo distinta diverso da 01 : no accet-  *
      *                  * tazione                                     *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           not  = 01
                     go to acc-tav-ric-999.
       acc-tav-ric-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tav-ric-lun    to   v-car                  .
           move      w-exp-tav-ric-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tav-ric-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tav-ric           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tav-ric-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tav-ric-999.
       acc-tav-ric-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tav-ric             .
       acc-tav-ric-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che sia un valore accettabile          *
      *                  *---------------------------------------------*
           if        rr-tav-ric           =    zero           or
                     rr-tav-ric           >    w-exp-tav-ric-num
                     go to acc-tav-ric-100.
       acc-tav-ric-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tav-ric-650.
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da includere in distinta      *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to acc-tav-ric-660
           else if   rr-tip-ddp           =    02
                     go to acc-tav-ric-670
           else if   rr-tip-ddp           =    03
                     go to acc-tav-ric-680
           else if   rr-tip-ddp           =    04
                     go to acc-tav-ric-690.
       acc-tav-ric-660.
           if        rr-tav-ric           =    01
                     go to acc-tav-ric-661
           else if   rr-tav-ric           =    02
                     go to acc-tav-ric-662
           else if   rr-tav-ric           =    03
                     go to acc-tav-ric-663
           else if   rr-tav-ric           =    04
                     go to acc-tav-ric-664
           else if   rr-tav-ric           =    05
                     go to acc-tav-ric-665
           else      go to acc-tav-ric-666.
       acc-tav-ric-661.
           if        rr-tsc-das           =    0002
                     go to acc-tav-ric-700.
           move      0002                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-662.
           if        rr-tsc-das           =    0002
                     go to acc-tav-ric-700.
           move      0002                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-663.
           if        rr-tsc-das           =    0203 or
                     rr-tsc-das           =    0003 or
                     rr-tsc-das           =    zero
                     go to acc-tav-ric-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-664.
           if        rr-tsc-das           =    0205 or
                     rr-tsc-das           =    0005 or
                     rr-tsc-das           =    zero
                     go to acc-tav-ric-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-665.
           if        rr-tsc-das           =    0006
                     go to acc-tav-ric-700.
           move      0006                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-666.
           if        rr-tsc-das           =    zero
                     go to acc-tav-ric-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-670.
           if        rr-tsc-das           =    0910 or
                     rr-tsc-das           =    0009 or
                     rr-tsc-das           =    0010 or
                     rr-tsc-das           =    zero
                     go to acc-tav-ric-700.
           move      zero                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-680.
           if        rr-tsc-das           =    0011
                     go to acc-tav-ric-700.
           move      0011                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-690.
           if        rr-tsc-das           =    0061
                     go to acc-tav-ric-700.
           move      0061                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     acc-tav-ric-700.
       acc-tav-ric-700.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-tav-ric-800.
       acc-tav-ric-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tav-ric-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tav-ric-100.
       acc-tav-ric-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo avviso richiesto                   *
      *    *-----------------------------------------------------------*
       vis-tav-ric-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tav-ric-lun    to   v-car                  .
           move      w-exp-tav-ric-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tav-ric-tbl    to   v-txt                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tav-ric           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tav-ric-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipi scadenza da includere in distinta     *
      *    *-----------------------------------------------------------*
       acc-tsc-das-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tsc-das-010.
      *                  *---------------------------------------------*
      *                  * Se valore gia' presente in quanto passato   *
      *                  * per mezzo della variabile di i.p.c. : no    *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
           if        rr-tsc-das           not  = zero and
                     w-ipc-tsc-das-snx    =    "S"
                     go to acc-tsc-das-999.
       acc-tsc-das-025.
      *                  *---------------------------------------------*
      *                  * Preparazione work per l'accettazione del    *
      *                  * campo espanso Tipi scadenza da includere    *
      *                  * in distinta                                 *
      *                  *---------------------------------------------*
           perform   pex-tsc-das-000      thru pex-tsc-das-999        .
       acc-tsc-das-050.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di ele-   *
      *                  * menti che compongono la tabella delle pos-  *
      *                  * sibili scelte alternative                   *
      *                  *---------------------------------------------*
           if        w-exp-tsc-das-num    =    zero
                     go to acc-tsc-das-060
           else if   w-exp-tsc-das-num    =    1
                     go to acc-tsc-das-070
           else      go to acc-tsc-das-080.
       acc-tsc-das-060.
      *                  *---------------------------------------------*
      *                  * Se zero scelte possibili                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore da accettare *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tsc-das             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del valore              *
      *                      *-----------------------------------------*
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tsc-das-999.
       acc-tsc-das-070.
      *                  *---------------------------------------------*
      *                  * Se una sola scelta possibile                *
      *                  *---------------------------------------------*
       acc-tsc-das-072.
      *                      *-----------------------------------------*
      *                      * Forzatura e visualizzazione del valore  *
      *                      * da accettare                            *
      *                      *-----------------------------------------*
           if        rr-tsc-das           =    w-exp-tsc-das-vel (1)
                     go to acc-tsc-das-074.
           move      w-exp-tsc-das-vel (1)
                                          to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
       acc-tsc-das-074.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tsc-das-999.
       acc-tsc-das-080.
      *                  *---------------------------------------------*
      *                  * Se piu' di una scelta possibile             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazione                         *
      *                      *-----------------------------------------*
           go to     acc-tsc-das-100.
       acc-tsc-das-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
       acc-tsc-das-102.
      *                  *---------------------------------------------*
      *                  * Preparazioni generali                       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsc-das-lun    to   v-car                  .
           move      w-exp-tsc-das-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tsc-das-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
       acc-tsc-das-105.
      *                  *---------------------------------------------*
      *                  * Preparazione indice corrispondente al valo- *
      *                  * re attuale                                  *
      *                  *---------------------------------------------*
           if        rr-tsc-das           =    zero
                     move  zero           to   v-num
                     go to acc-tsc-das-125.
       acc-tsc-das-110.
           move      zero                 to   w-exp-tsc-das-inx      .
       acc-tsc-das-115.
           add       1                    to   w-exp-tsc-das-inx      .
           if        w-exp-tsc-das-inx    >    w-exp-tsc-das-num
                     move  zero           to   v-num
                     go to acc-tsc-das-125.
           if        w-exp-tsc-das-vel
                    (w-exp-tsc-das-inx)   =    rr-tsc-das
                     move  w-exp-tsc-das-inx
                                          to   v-num
                     go to acc-tsc-das-125
           else      go to acc-tsc-das-115.
       acc-tsc-das-125.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tsc-das-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tsc-das-999.
       acc-tsc-das-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-tsc-das-inx      .
           if        w-exp-tsc-das-inx    =    zero           or
                     w-exp-tsc-das-inx    >    w-exp-tsc-das-num
                     move  zero           to   rr-tsc-das
                     go to acc-tsc-das-400.
           move      w-exp-tsc-das-vel
                    (w-exp-tsc-das-inx)   to   rr-tsc-das             .
       acc-tsc-das-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia a zero, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        rr-tsc-das           not  = zero
                     go to acc-tsc-das-600.
           if        v-key                =    "UP  "
                     go to acc-tsc-das-600
           else      go to acc-tsc-das-100.
       acc-tsc-das-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tsc-das-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tsc-das-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tsc-das-100.
       acc-tsc-das-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipi scadenza da includere in distinta  *
      *    *-----------------------------------------------------------*
       vis-tsc-das-000.
      *              *-------------------------------------------------*
      *              * Preparazione work per l'accettazione del campo  *
      *              * espanso Tipi scadenza da includere in distinta  *
      *              *-------------------------------------------------*
           perform   pex-tsc-das-000      thru pex-tsc-das-999        .
       vis-tsc-das-200.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero di elementi   *
      *              * che compongono la tabella delle possibili scel- *
      *              * te alternative                                  *
      *              *-------------------------------------------------*
           if        w-exp-tsc-das-num    =    zero
                     go to vis-tsc-das-250
           else      go to vis-tsc-das-300.
       vis-tsc-das-250.
      *              *-------------------------------------------------*
      *              * Se zero scelte possibili                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione a spaces                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsc-das-lun    to   v-car                  .
           move      w-exp-tsc-das-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tsc-das-tbl    to   v-txt                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tsc-das-999.
       vis-tsc-das-300.
      *              *-------------------------------------------------*
      *              * Se almeno una scelta possibile                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione valore                      *
      *                  *---------------------------------------------*
       vis-tsc-das-325.
      *                      *-----------------------------------------*
      *                      * Preparazioni generali                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsc-das-lun    to   v-car                  .
           move      w-exp-tsc-das-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tsc-das-tbl    to   v-txt                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
       vis-tsc-das-350.
      *                      *-----------------------------------------*
      *                      * Preparazioni indice corrispondente al   *
      *                      * valore attuale                          *
      *                      *-----------------------------------------*
           if        rr-tsc-das           =    zero
                     move  zero           to   v-num
                     go to vis-tsc-das-375.
           move      zero                 to   w-exp-tsc-das-inx      .
       vis-tsc-das-355.
           add       1                    to   w-exp-tsc-das-inx      .
           if        w-exp-tsc-das-inx    >    w-exp-tsc-das-num
                     move  zero           to   v-num
                     go to vis-tsc-das-375.
           if        w-exp-tsc-das-vel
                    (w-exp-tsc-das-inx)   =    rr-tsc-das
                     move  w-exp-tsc-das-inx
                                          to   v-num
                     go to vis-tsc-das-375
           else      go to vis-tsc-das-355.
       vis-tsc-das-375.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tsc-das-999.
       vis-tsc-das-999.
           exit.

      *    *===========================================================*
      *    * Preparazione work per l'accettazione del campo espanso :  *
      *    * Tipi scadenza da includere in distinta                    *
      *    *-----------------------------------------------------------*
       pex-tsc-das-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo distinta        *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to pex-tsc-das-100
           else if   rr-tip-ddp           =    02
                     go to pex-tsc-das-200
           else if   rr-tip-ddp           =    03
                     go to pex-tsc-das-300
           else if   rr-tip-ddp           =    04
                     go to pex-tsc-das-400
           else      go to pex-tsc-das-900.
       pex-tsc-das-100.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 01                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo avviso ri-  *
      *                  * chiesto                                     *
      *                  *---------------------------------------------*
           if        rr-tav-ric           =    01
                     go to pex-tsc-das-110
           else if   rr-tav-ric           =    02
                     go to pex-tsc-das-120
           else if   rr-tav-ric           =    03
                     go to pex-tsc-das-130
           else if   rr-tav-ric           =    04
                     go to pex-tsc-das-140
           else if   rr-tav-ric           =    05
                     go to pex-tsc-das-150
           else      go to pex-tsc-das-190.
       pex-tsc-das-110.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 01               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione work accettazione per :    *
      *                      * - Incassi elettronici                   *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n01    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t01    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v01    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-120.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 02               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione work accettazione per :    *
      *                      * - Incassi elettronici                   *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n02    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t02    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v02    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-130.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 03               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione work accettazione per :    *
      *                      * - Incassi elettronici e Ri.Ba.          *
      *                      * - Solo scadenze di tipo Ri.Ba.          *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n03    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t03    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v03    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-140.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 04               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione work accettazione per :    *
      *                      * - Incassi elettronici e M.Av.           *
      *                      * - Solo scadenze di tipo M.Av.           *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n04    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t04    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v04    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-150.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 05               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione work accettazione per :    *
      *                      * - Solo scadenze di tipo R.I.D.          *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n05    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t05    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v05    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-190.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto non riconosciuto   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A normalizzazione work-area             *
      *                      *-----------------------------------------*
           go to     pex-tsc-das-900.
       pex-tsc-das-200.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 02                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione work per accettazione per :    *
      *                  * - Ricevute bancarie e Tratte                *
      *                  * - Solo Ricevute bancarie                    *
      *                  * - Solo Tratte                               *
      *                  *---------------------------------------------*
           move      w-exp-tsc-das-n06    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t06    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v06    to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-300.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 03                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione work per accettazione per :    *
      *                  * - Paghero' cambiari                         *
      *                  *---------------------------------------------*
           move      w-exp-tsc-das-n07    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t07    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v07    to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-400.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 04                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione work per accettazione per :    *
      *                  * - Paghero' cambiari avuti in cessione       *
      *                  *---------------------------------------------*
           move      w-exp-tsc-das-n08    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t08    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v08    to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-900.
      *              *-------------------------------------------------*
      *              * Se tipo distinta non riconosciuto               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work per accettazione       *
      *                  *---------------------------------------------*
           move      zero                 to   w-exp-tsc-das-num      .
           move      spaces               to   w-exp-tsc-das-tbl      .
           move      all   "0"            to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pex-tsc-das-999.
       pex-tsc-das-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data scadenza minima da selezionare        *
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
           move      13                   to   v-lin                  .
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
      *    * Visualizzazione : Data scadenza minima da selezionare     *
      *    *-----------------------------------------------------------*
       vis-dts-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dts-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dts-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data scadenza massima da selezionare       *
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
           move      14                   to   v-lin                  .
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
      *    * Visualizzazione : Data scadenza massima da selezionare    *
      *    *-----------------------------------------------------------*
       vis-dts-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dts-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dts-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Selezione scadenze a vista                 *
      *    *-----------------------------------------------------------*
       acc-dts-vis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo distinta che non prevede scadenze   *
      *                  * a vista : no accettazione                   *
      *                  *---------------------------------------------*
           if        rr-tip-ddp           not  = 02 and
                     rr-tip-ddp           not  = 03 and
                     rr-tip-ddp           not  = 04
                     go to acc-dts-vis-999.
       acc-dts-vis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dts-vis-lun    to   v-car                  .
           move      w-exp-dts-vis-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-dts-vis-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-dts-vis           =    "S"
                     move  01             to   v-num
           else if   rr-dts-vis           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dts-vis-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dts-vis-999.
       acc-dts-vis-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to  rr-dts-vis
           else if   v-num                =    02
                     move  "N"            to  rr-dts-vis
           else      move  spaces         to  rr-dts-vis              .
       acc-dts-vis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che sia un valore accettabile          *
      *                  *---------------------------------------------*
           if        rr-dts-vis           =    spaces and
                     v-key                =    "UP  "
                     go to acc-dts-vis-600.
           if        rr-dts-vis           =    "S" or
                     rr-dts-vis           =    "N"
                     go to acc-dts-vis-600
           else      go to acc-dts-vis-100.
       acc-dts-vis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dts-vis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dts-vis-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dts-vis-100.
       acc-dts-vis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Selezione scadenze a vista              *
      *    *-----------------------------------------------------------*
       vis-dts-vis-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dts-vis-lun    to   v-car                  .
           move      w-exp-dts-vis-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-dts-vis-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-dts-vis           =    "S"
                     move  01             to   v-num
           else if   rr-dts-vis           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dts-vis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Selezione su circuiti interbancari         *
      *    *-----------------------------------------------------------*
       acc-cir-itb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cir-itb-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo distinta diverso da 01 : no ac- *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           if        rr-tip-ddp           not  = 01
                     go to acc-cir-itb-999.
       acc-cir-itb-075.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice 1..5 per scansione  *
      *                  * sui 5 elementi                              *
      *                  *---------------------------------------------*
           move      01                   to   w-acc-cir-itb-inx      .
       acc-cir-itb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-axc-ope      .
           move      rr-cod-cib
                    (w-acc-cir-itb-inx)   to   w-cod-des-axc-cod      .
           move      18                   to   w-cod-des-axc-lin      .
           move      w-acc-cir-itb-inx    to   w-cod-des-axc-pos      .
           subtract  1                    from w-cod-des-axc-pos      .
           multiply  4                    by   w-cod-des-axc-pos      .
           add       30                   to   w-cod-des-axc-pos      .
           move      zero                 to   w-cod-des-axc-dln      .
           move      zero                 to   w-cod-des-axc-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-axc-cll-000  thru cod-des-axc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-axc-foi-000  thru cod-des-axc-foi-999    .
       acc-cod-cib-110.
           perform   cod-des-axc-cll-000  thru cod-des-axc-cll-999    .
           if        w-cod-des-axc-ope    =    "F+"
                     go to acc-cod-cib-115.
           if        w-cod-des-axc-ope    =    "AC"
                     go to acc-cod-cib-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cib-115.
           perform   cod-des-axc-foi-000  thru cod-des-axc-foi-999    .
           go to     acc-cod-cib-110.
       acc-cod-cib-120.
           move      w-cod-des-axc-cod    to   v-alf                  .
       acc-cod-cib-125.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cir-itb-999.
       acc-cir-itb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-cib
                                              (w-acc-cir-itb-inx)     .
       acc-cir-itb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cir-itb-410.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [axc]                      *
      *                  *---------------------------------------------*
           move      rr-cod-cib
                    (w-acc-cir-itb-inx)   to   w-let-arc-axc-cod      .
           perform   let-arc-axc-000      thru let-arc-axc-999        .
       acc-cir-itb-420.
      *                  *---------------------------------------------*
      *                  * Se record non esistente : a reimpostazione  *
      *                  *---------------------------------------------*
           if        w-let-arc-axc-flg    not  = spaces
                     go to acc-cir-itb-100.
       acc-cir-itb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cir-itb-610.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tasto di funzio- *
      *                  * ne impostato                                *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-cir-itb-620
           else if   v-key                =    "DOWN"
                     go to acc-cir-itb-630
           else if   v-key                =    "UP  "
                     go to acc-cir-itb-640
           else      go to acc-cir-itb-690.
       acc-cir-itb-620.
      *                  *---------------------------------------------*
      *                  * Se terminazione con Return                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Compattazione dei cinque circuiti       *
      *                      *-----------------------------------------*
           perform   cpx-cir-itb-000      thru cpx-cir-itb-999        .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se la compattazio- *
      *                      * ne ha avuto luogo o no                  *
      *                      *-----------------------------------------*
           if        w-acc-cir-itb-fce    =    spaces
                     go to acc-cir-itb-622
           else      go to acc-cir-itb-624.
       acc-cir-itb-622.
      *                      *-----------------------------------------*
      *                      * Se la compattazione non ha avuto luogo  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il valore corrispondente all'in- *
      *                          * dice e' a Spaces : uscita con Re-   *
      *                          * turn                                *
      *                          *-------------------------------------*
           if        rr-cod-cib
                    (w-acc-cir-itb-inx)   =    spaces
                     move  spaces         to   v-key
                     go to acc-cir-itb-800.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla all'imposta-  *
      *                          * zione sull' indice successivo, o si *
      *                          * va' alla fine se oltre il massimo   *
      *                          *-------------------------------------*
           add       1                    to   w-acc-cir-itb-inx      .
           if        w-acc-cir-itb-inx    >    5
                     go to acc-cir-itb-800
           else      go to acc-cir-itb-100.
       acc-cir-itb-624.
      *                      *-----------------------------------------*
      *                      * Se la compattazione ha avuto luogo      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Rivisualizzazione                   *
      *                          *-------------------------------------*
           perform   vis-cir-itb-000      thru vis-cir-itb-999        .
      *                          *-------------------------------------*
      *                          * Si ricicla all'impostazione sullo   *
      *                          * stesso indice                       *
      *                          *-------------------------------------*
           go to     acc-cir-itb-100.
       acc-cir-itb-630.
      *                  *---------------------------------------------*
      *                  * Se terminazione con Down                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Compattazione dei cinque circuiti       *
      *                      *-----------------------------------------*
           perform   cpx-cir-itb-000      thru cpx-cir-itb-999        .
      *                      *-----------------------------------------*
      *                      * Eventuale rivisualizzazione, se il com- *
      *                      * pattamento ha avuto luogo               *
      *                      *-----------------------------------------*
           if        w-acc-cir-itb-fce    =    spaces
                     go to acc-cir-itb-632.
           perform   vis-cir-itb-000      thru vis-cir-itb-999        .
       acc-cir-itb-632.
      *                      *-----------------------------------------*
      *                      * Uscita con Down                         *
      *                      *-----------------------------------------*
           move      "DOWN"               to   v-key                  .
           go to     acc-cir-itb-800.
       acc-cir-itb-640.
      *                  *---------------------------------------------*
      *                  * Se terminazione con Up                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Compattazione dei cinque circuiti       *
      *                      *-----------------------------------------*
           perform   cpx-cir-itb-000      thru cpx-cir-itb-999        .
      *                      *-----------------------------------------*
      *                      * Eventuale rivisualizzazione, se il com- *
      *                      * pattamento ha avuto luogo               *
      *                      *-----------------------------------------*
           if        w-acc-cir-itb-fce    =    spaces
                     go to acc-cir-itb-642.
           perform   vis-cir-itb-000      thru vis-cir-itb-999        .
       acc-cir-itb-642.
      *                      *-----------------------------------------*
      *                      * Se indice di scansione a 1 : si esce    *
      *                      * con Up                                  *
      *                      *-----------------------------------------*
           if        w-acc-cir-itb-inx    =    1
                     move  "UP  "         to   v-key
                     go to acc-cir-itb-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti si decrementa l'indice di    *
      *                      * scansione e si ricicla all'impostazione *
      *                      *-----------------------------------------*
           subtract  1                    from w-acc-cir-itb-inx      .
           go to     acc-cir-itb-100.
       acc-cir-itb-690.
      *                  *---------------------------------------------*
      *                  * Se terminazione con altri tasti             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cir-itb-800.
       acc-cir-itb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cir-itb-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cir-itb-100.
       acc-cir-itb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Selezione su circuiti interbancari      *
      *    *-----------------------------------------------------------*
       vis-cir-itb-000.
      *              *-------------------------------------------------*
      *              * Preparazione campo da visualizzare              *
      *              *-------------------------------------------------*
           move      spaces               to   w-vis-cir-itb-edt      .
           move      zero                 to   w-vis-cir-itb-inx      .
       vis-cir-itb-050.
           add       1                    to   w-vis-cir-itb-inx      .
           if        w-vis-cir-itb-inx    >    5
                     go to vis-cir-itb-100.
           move      rr-cod-cib
                    (w-vis-cir-itb-inx)   to   w-vis-cir-itb-edx
                                              (w-vis-cir-itb-inx)     .
           go to     vis-cir-itb-050.
       vis-cir-itb-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-vis-cir-itb-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cir-itb-999.
           exit.

      *    *===========================================================*
      *    * Compattazione dei cinque elementi costituenti i circuiti  *
      *    * interbancari                                              *
      *    *-----------------------------------------------------------*
       cpx-cir-itb-000.
      *              *-------------------------------------------------*
      *              * Flag di compattazione eseguita in Off           *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cir-itb-fce      .
      *              *-------------------------------------------------*
      *              * Se tutto a Spaces : uscita                      *
      *              *-------------------------------------------------*
           if        rr-cir-itb           =    spaces
                     go to cpx-cir-itb-999.
       cpx-cir-itb-100.
      *              *-------------------------------------------------*
      *              * Eventuale compattamento e conseguente set in On *
      *              * del flag di compattazione eseguita              *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-cir-itb-i01      .
       cpx-cir-itb-200.
           add       1                    to   w-acc-cir-itb-i01      .
           if        w-acc-cir-itb-i01    not < 5
                     go to cpx-cir-itb-999.
           if        rr-cod-cib
                    (w-acc-cir-itb-i01)   not  = spaces
                     go to cpx-cir-itb-200.
           move      w-acc-cir-itb-i01    to   w-acc-cir-itb-i02      .
       cpx-cir-itb-300.
           add       1                    to   w-acc-cir-itb-i02      .
           if        w-acc-cir-itb-i02    >    5
                     go to cpx-cir-itb-999.
           if        rr-cod-cib
                    (w-acc-cir-itb-i02)   =    spaces
                     go to cpx-cir-itb-300.
           move      "#"                  to   w-acc-cir-itb-fce      .
           move      w-acc-cir-itb-i01    to   w-acc-cir-itb-i03      .
       cpx-cir-itb-400.
           move      rr-cod-cib
                    (w-acc-cir-itb-i02)   to   rr-cod-cib
                                              (w-acc-cir-itb-i03)     .
           add       1                    to   w-acc-cir-itb-i02      .
           add       1                    to   w-acc-cir-itb-i03      .
           if        w-acc-cir-itb-i02    >    5
                     go to cpx-cir-itb-500
           else      go to cpx-cir-itb-400.
       cpx-cir-itb-500.
           move      spaces               to   rr-cod-cib
                                              (w-acc-cir-itb-i03)     .
           add       1                    to   w-acc-cir-itb-i03      .
           if        w-acc-cir-itb-i02    >    5
                     go to cpx-cir-itb-200
           else      go to cpx-cir-itb-500.
       cpx-cir-itb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Importo massimo da selezionare             *
      *    *-----------------------------------------------------------*
       acc-imp-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-imp-max-100.
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
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-imp-max           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-imp-max-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-imp-max-999.
       acc-imp-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-imp-max             .
       acc-imp-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-imp-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-imp-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-imp-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-imp-max-100.
       acc-imp-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Importo massimo da selezionare          *
      *    *-----------------------------------------------------------*
       vis-imp-max-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-imp-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-imp-max-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-025.
      *              *-------------------------------------------------*
      *              * Controllo su data composizione distinta         *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           not  = zero
                     go to tdo-ric-sel-050.
           move      "Manca la data composizione distinta               
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-050.
      *              *-------------------------------------------------*
      *              * Controllo su tipo distinta                      *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           not  = zero
                     go to tdo-ric-sel-055.
           move      "Manca il tipo distinta                            
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-055.
           if        rr-tip-ddp           =    01 or
                     rr-tip-ddp           =    02 or
                     rr-tip-ddp           =    03 or
                     rr-tip-ddp           =    04
                     go to tdo-ric-sel-100.
           move      "Tipo distinta errato                              
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controllo su tipo avviso richiesto              *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           not  = 01
                     go to tdo-ric-sel-150.
           if        rr-tav-ric           not  = zero
                     go to tdo-ric-sel-105.
           move      "Manca il tipo avviso richiesto                    
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-105.
           if        rr-tav-ric           =    01 or
                     rr-tav-ric           =    02 or
                     rr-tav-ric           =    03 or
                     rr-tav-ric           =    04 or
                     rr-tav-ric           =    05
                     go to tdo-ric-sel-150.
           move      "Tipo avviso richiesto errato                      
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-150.
      *              *-------------------------------------------------*
      *              * Controllo su tipo scadenze da includere in di-  *
      *              * stinta                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to tdo-ric-sel-155
           else if   rr-tip-ddp           =    02
                     go to tdo-ric-sel-185
           else if   rr-tip-ddp           =    03
                     go to tdo-ric-sel-190
           else if   rr-tip-ddp           =    04
                     go to tdo-ric-sel-195.
       tdo-ric-sel-155.
           if        rr-tav-ric           =    01
                     go to tdo-ric-sel-160
           else if   rr-tav-ric           =    02
                     go to tdo-ric-sel-165
           else if   rr-tav-ric           =    03
                     go to tdo-ric-sel-170
           else if   rr-tav-ric           =    04
                     go to tdo-ric-sel-175
           else if   rr-tav-ric           =    05
                     go to tdo-ric-sel-180.
       tdo-ric-sel-160.
           if        rr-tsc-das           =    0002
                     go to tdo-ric-sel-200.
           move      0002                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     tdo-ric-sel-200.
       tdo-ric-sel-165.
           if        rr-tsc-das           =    0002
                     go to tdo-ric-sel-200.
           move      0002                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     tdo-ric-sel-200.
       tdo-ric-sel-170.
           if        rr-tsc-das           not  = zero
                     go to tdo-ric-sel-171.
           move      "Manca la selezione sui tipi scadenza da includere 
      -              "in distinta    "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-171.
           if        rr-tsc-das           =    0203 or
                     rr-tsc-das           =    0003
                     go to tdo-ric-sel-200.
           move      "Selezione sui tipi scadenza da includere in distin
      -              "ta errata      "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-175.
           if        rr-tsc-das           not  = zero
                     go to tdo-ric-sel-176.
           move      "Manca la selezione sui tipi scadenza da includere 
      -              "in distinta    "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-176.
           if        rr-tsc-das           =    0205 or
                     rr-tsc-das           =    0005
                     go to tdo-ric-sel-200.
           move      "Selezione sui tipi scadenza da includere in distin
      -              "ta errata      "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-180.
           if        rr-tsc-das           =    0006
                     go to tdo-ric-sel-200.
           move      0006                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     tdo-ric-sel-200.
       tdo-ric-sel-185.
           if        rr-tsc-das           not  = zero
                     go to tdo-ric-sel-186.
           move      "Manca la selezione sui tipi scadenza da includere 
      -              "in distinta    "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-186.
           if        rr-tsc-das           =    0910 or
                     rr-tsc-das           =    0009 or
                     rr-tsc-das           =    0010
                     go to tdo-ric-sel-200.
           move      "Selezione sui tipi scadenza da includere in distin
      -              "ta errata      "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-190.
           if        rr-tsc-das           =    0011
                     go to tdo-ric-sel-200.
           move      0011                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     tdo-ric-sel-200.
       tdo-ric-sel-195.
           if        rr-tsc-das           =    0061
                     go to tdo-ric-sel-200.
           move      0061                 to   rr-tsc-das             .
           perform   vis-tsc-das-000      thru vis-tsc-das-999        .
           go to     tdo-ric-sel-200.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su data scadenza iniziale             *
      *              *-------------------------------------------------*
           if        rr-dts-min           =    zero
                     go to tdo-ric-sel-210.
           if        rr-dts-min           not  < rr-dtr-com
                     go to tdo-ric-sel-210.
           move      "La data scadenza iniziale non puo' essere inferior
      -              "e alla         "    to   w-err-box-err-msg      .
           move      "data di composizione della distinta               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-210.
      *              *-------------------------------------------------*
      *              * Controllo su data scadenza iniziale             *
      *              *-------------------------------------------------*
           if        rr-dts-max           =    zero
                     go to tdo-ric-sel-220.
           if        rr-dts-max           not  < rr-dtr-com
                     go to tdo-ric-sel-220.
           move      "La data scadenza finale non puo' essere inferiore 
      -              "alla           "    to   w-err-box-err-msg      .
           move      "data di composizione della distinta               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-220.
      *              *-------------------------------------------------*
      *              * Controllo su data scadenza iniziale e finale    *
      *              *-------------------------------------------------*
           if        rr-dts-min           =    zero or
                     rr-dts-max           =    zero
                     go to tdo-ric-sel-250.
           if        rr-dts-max           not  < rr-dts-min
                     go to tdo-ric-sel-250.
           move      "La data scadenza finale non puo' essere inferiore 
      -              "all'iniziale   "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-250.
      *              *-------------------------------------------------*
      *              * Controllo su data scadenza a vista              *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to tdo-ric-sel-255
           else      go to tdo-ric-sel-260.
       tdo-ric-sel-255.
           if        rr-dts-vis           =    spaces
                     go to tdo-ric-sel-300.
           move      spaces               to   rr-dts-vis             .
           perform   vis-dts-vis-000      thru vis-dts-vis-999        .
           go to     tdo-ric-sel-300.
       tdo-ric-sel-260.
           if        rr-dts-vis           not  = spaces
                     go to tdo-ric-sel-265.
           move      "Manca la selezione si/no scadenze a vista         
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-265.
           if        rr-dts-vis           =    "S" or
                     rr-dts-vis           =    "N"
                     go to tdo-ric-sel-300.
           move      "La selezione si/no scadenze a vista e' errata     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su data scadenza a vista              *
      *              *-------------------------------------------------*
           if        rr-tip-ddp           =    01
                     go to tdo-ric-sel-350.
           if        rr-cir-itb           =    spaces
                     go to tdo-ric-sel-350.
           move      spaces               to   rr-cir-itb             .
           perform   vis-cir-itb-000      thru vis-cir-itb-999        .
       tdo-ric-sel-350.
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
      *                  * Data composizione distinta                  *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dtr-com             .
      *                  *---------------------------------------------*
      *                  * Numero distinta                             *
      *                  *---------------------------------------------*
           move      zero                 to   rr-num-ddp             .
      *                  *---------------------------------------------*
      *                  * Tipo distinta di presentazione              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tip-ddp             .
      *                  *---------------------------------------------*
      *                  * Tipo avviso richiesto                       *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tav-ric             .
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da includere in distinta      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tsc-das             .
      *                  *---------------------------------------------*
      *                  * Data scadenza minima da selezionare         *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dts-min             .
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da selezionare        *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dts-max             .
      *                  *---------------------------------------------*
      *                  * Si/No scadenze a vista da selezionare       *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dts-vis             .
      *                  *---------------------------------------------*
      *                  * Selezione su circuiti interbancari          *
      *                  *---------------------------------------------*
           move      spaces               to   rr-cir-itb             .
      *                  *---------------------------------------------*
      *                  * Importo massimo da selezionare              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-imp-max             .
       nor-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Preparazione defaults                           *
      *              *-------------------------------------------------*
       nor-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Preparazione area richieste da variabili di *
      *                  * i.p.c. lette, con determinazione se tutte   *
      *                  * preparate oppure no                         *
      *                  *---------------------------------------------*
           perform   pre-ric-ipc-000      thru pre-ric-ipc-999        .
       nor-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Fine preparazione defaults                  *
      *                  *---------------------------------------------*
           go to     nor-ric-sel-999.
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di selezione in esecuzione              *
      *    *-----------------------------------------------------------*
       msg-sel-exe-000.
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
           move      08                   to   v-pos                  .
           move      "   Selezione scadenze per inclusione in distinta i
      -              "n esecuzione   "    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-sel-exe-999.
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
      *              * Box per messaggio di selezione in esecuzione    *
      *              *-------------------------------------------------*
           perform   msg-sel-exe-000      thru msg-sel-exe-999        .
       qry-str-ini-200.
      *              *-------------------------------------------------*
      *              * Inizializzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accumulo per importo massimo da selezionare *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-seq-sdb-igm      .
       qry-str-ini-250.
      *              *-------------------------------------------------*
      *              * Preparazione parametri di selezione generali    *
      *              * per le routines di selezione sulle scadenze     *
      *              * lette                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data composizione distinta                  *
      *                  *---------------------------------------------*
           move      rr-dtr-com           to   w-sel-sdb-inp-dcd      .
      *                  *---------------------------------------------*
      *                  * Numero distinta                             *
      *                  *---------------------------------------------*
           move      rr-num-ddp           to   w-sel-sdb-inp-nmd      .
      *                  *---------------------------------------------*
      *                  * Tipo di distinta                            *
      *                  *---------------------------------------------*
           move      rr-tip-ddp           to   w-sel-sdb-inp-tdd      .
      *                  *---------------------------------------------*
      *                  * Tipo di avviso richiesto se distinta per    *
      *                  * incassi elettronici                         *
      *                  *---------------------------------------------*
           move      rr-tav-ric           to   w-sel-sdb-inp-tar      .
      *                  *---------------------------------------------*
      *                  * Tipi di scadenza da includere in distinta   *
      *                  *---------------------------------------------*
           move      rr-tsc-das           to   w-sel-sdb-inp-tsd      .
      *                  *---------------------------------------------*
      *                  * Data scadenza minima da selezionare         *
      *                  *---------------------------------------------*
           move      rr-dts-min           to   w-sel-sdb-inp-smi      .
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da selezionare        *
      *                  *---------------------------------------------*
           move      rr-dts-max           to   w-sel-sdb-inp-sma      .
      *                  *---------------------------------------------*
      *                  * Anche scadenze a vista da selezionare       *
      *                  *---------------------------------------------*
           move      rr-dts-vis           to   w-sel-sdb-inp-asv      .
      *                  *---------------------------------------------*
      *                  * Selezione su circuiti interbancari          *
      *                  *---------------------------------------------*
           move      rr-cir-itb           to   w-sel-sdb-inp-cib      .
       qry-str-ini-275.
      *              *-------------------------------------------------*
      *              * Lettura record relativo alla distinta richiesta *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record distinta             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *                  *---------------------------------------------*
      *                  * Se numero distinta richiesto a zero : nes-  *
      *                  * suna lettura                                *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-nmd    =    zero
                     go to qry-str-ini-300.
      *                  *---------------------------------------------*
      *                  * Lettura da archivio [ddp]                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      w-sel-sdb-inp-nmd    to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *                  *---------------------------------------------*
      *                  * Se record esistente : nessun'altra azione   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to qry-str-ini-300.
      *                  *---------------------------------------------*
      *                  * Rinormalizzazione record distinta           *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
       qry-str-ini-300.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se scadenze a vista da in- *
      *              * cludere oppure no                               *
      *              *-------------------------------------------------*
           if        rr-dts-vis           =    "S"
                     go to qry-str-ini-400
           else      go to qry-str-ini-500.
       qry-str-ini-400.
      *              *-------------------------------------------------*
      *              * Se scadenze a vista da considerare              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag per lettura scadenze a vista : non an- *
      *                  * cora completata                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-seq-sdb-lsv      .
      *                  *---------------------------------------------*
      *                  * Start per scadenze a vista                  *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DTSNRS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a trattamento errore      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-str-ini-900.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita normale                 *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-500.
      *              *-------------------------------------------------*
      *              * Se scadenze a vista da non considerare          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag per lettura scadenze a vista : comple- *
      *                  * tata                                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-seq-sdb-lsv      .
      *                  *---------------------------------------------*
      *                  * Start per scadenze non a vista              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DTSNRS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           if        rr-dts-min           <    rr-dtr-com
                     move  rr-dtr-com     to   rf-sdb-dts-sdb
           else      move  rr-dts-min     to   rf-sdb-dts-sdb         .
           if        rf-sdb-dts-sdb       =    zero
                     move   0000001       to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a trattamento errore      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-str-ini-900.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita normale                 *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-900.
      *              *-------------------------------------------------*
      *              * Se Start errata                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
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
           move      15                   to   v-lto                  .
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
           move      08                   to   v-pos                  .
           move      "Nessuna scadenza utile per essere inclusa in disti
      -              "nta!           "    to   v-alf                  .
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
      *              * Next su [sdb] per data scadenza                 *
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
      *              * Deviazione a seconda se si e' in corso di trat- *
      *              * tamento delle scadenze a vista oppure no        *
      *              *-------------------------------------------------*
           if        w-let-seq-sdb-lsv    =    spaces
                     go to qry-tst-max-200
           else      go to qry-tst-max-400.
       qry-tst-max-200.
      *              *-------------------------------------------------*
      *              * Se si stanno leggendo le scadenze a vista       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data scadenza della scadenza letta    *
      *                  * non e' a vista si va' al trattamento per    *
      *                  * le scadenze non a vista                     *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       not  = zero
                     go to qry-tst-max-300.
      *                  *---------------------------------------------*
      *                  * Altrimenti si esce normalmente              *
      *                  *---------------------------------------------*
           go to     qry-tst-max-999.
       qry-tst-max-300.
      *                  *---------------------------------------------*
      *                  * Flag per lettura scadenze a vista : comple- *
      *                  * tata                                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-seq-sdb-lsv      .
      *                  *---------------------------------------------*
      *                  * Start per scadenze non a vista              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DTSNRS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           if        rr-dts-min           <    rr-dtr-com
                     move  rr-dtr-com     to   rf-sdb-dts-sdb
           else      move  rr-dts-min     to   rf-sdb-dts-sdb         .
           if        rf-sdb-dts-sdb       =    zero
                     move   0000001       to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a trattamento fine file   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-tst-max-900.
      *                  *---------------------------------------------*
      *                  * Next su [sdb] per data scadenza             *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se Next errata : a trattamento fine file    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-tst-max-900.
      *                  *---------------------------------------------*
      *                  * Altrimenti : a trattamento scadenze non a   *
      *                  * vista                                       *
      *                  *---------------------------------------------*
           go to     qry-tst-max-400.
       qry-tst-max-400.
      *              *-------------------------------------------------*
      *              * Se si stanno leggendo le scadenze non a vista   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se data scadenza massima a zero : nessun    *
      *                  * test sul max : uscita normale               *
      *                  *---------------------------------------------*
           if        rr-dts-max           =    zero
                     go to qry-tst-max-999.
      *                  *---------------------------------------------*
      *                  * Test con deviazione corrispondente all'esi- *
      *                  * to del controllo                            *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       >    rr-dts-max
                     go to qry-tst-max-900
           else      go to qry-tst-max-999.
       qry-tst-max-900.
      *              *-------------------------------------------------*
      *              * Se fine lettura                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-tst-max-999.
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
       qry-sel-rec-050.
      *              *-------------------------------------------------*
      *              * Selezione preliminare : se la scadenza e' in    *
      *              * stato di 'Appartenente ad una distinta solo     *
      *              * composta ma non ancora presentata', e la di-    *
      *              * stinta cui appartiene e' esattamente la di-     *
      *              * stinta richiesta, allora significa che la sca-  *
      *              * denza appartiene ad una distinta che e' in cor- *
      *              * so di modifica, pertanto viene sicuramente se-  *
      *              * lezionata, a meno che non faccia parte della    *
      *              * lista scadenze da non selezionare               *
      *              *-------------------------------------------------*
       qry-sel-rec-052.
      *                  *---------------------------------------------*
      *                  * Se non e' stato richiesto un particolare    *
      *                  * numero distinta : a normale selezione       *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-nmd    =    zero
                     go to qry-sel-rec-100.
       qry-sel-rec-054.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non appartiene ad una di-    *
      *                  * stinta : a normale selezione                *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ddp       =    zero
                     go to qry-sel-rec-100.
       qry-sel-rec-056.
      *                  *---------------------------------------------*
      *                  * Se la scadenza appartiene ad una distinta   *
      *                  * diversa da quella richiesta : a normale se- *
      *                  * lezione                                     *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ddp       not  = w-sel-sdb-inp-nmd
                     go to qry-sel-rec-100.
       qry-sel-rec-058.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non ha una data registrazio- *
      *                  * ne per il movimento di emissione : a norma- *
      *                  * le selezione                                *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-emi       =    zero
                     go to qry-sel-rec-100.
       qry-sel-rec-060.
      *                  *---------------------------------------------*
      *                  * Se la scadenza ha una data registrazione    *
      *                  * per un qualsiasi movimento che non sia il   *
      *                  * movimento di emissione : a normale sele-    *
      *                  * zione                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su storno                          *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-sto       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su riscossione                     *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-ris       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su richiamo                        *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-rsp       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su accredito scadenza              *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-acs       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su notizia di buon esito           *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-nbe       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su presunto buon esito             *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-pbe       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su insoluto                        *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-isp       not  = zero
                     go to qry-sel-rec-100.
       qry-sel-rec-065.
      *                  *---------------------------------------------*
      *                  * Se non esiste il record relativo alla di-   *
      *                  * stinta richiesta : a normale selezione      *
      *                  *---------------------------------------------*
           if        rf-ddp-num-ddp       =    zero
                     go to qry-sel-rec-100.
       qry-sel-rec-067.
      *                  *---------------------------------------------*
      *                  * Se il record relativo alla distinta richie- *
      *                  * sta esiste, ma la distinta ha subito movi-  *
      *                  * menti successivi alla composizione : a nor- *
      *                  * male selezione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su presentazione                   *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-pre       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su accettazione                    *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-act       not  = zero
                     go to qry-sel-rec-100.
      *                      *-----------------------------------------*
      *                      * Test su accredito                       *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-acd       not  = zero
                     go to qry-sel-rec-100.
       qry-sel-rec-075.
      *                  *---------------------------------------------*
      *                  * Se tutti i test precedenti sono stati supe- *
      *                  * rati : si va' al controllo finale che la    *
      *                  * scadenza non appartenga al set di scadenze  *
      *                  * che comunque non sono selezionabili         *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-600.
       qry-sel-rec-100.
      *              *-------------------------------------------------*
      *              * 1. gruppo di selezioni sulla scadenza, su :     *
      *              *                                                 *
      *              * - Tipo di scadenza, che sia riconosciuto        *
      *              * - Tipo di distinta, che sia riconosciuto        *
      *              * - Tipo di avviso richiesto, che sia riconosciu- *
      *              *   to                                            *
      *              * - Tipo di scadenza, che sia ammissibile per il  *
      *              *   tipo di distinta                              *
      *              * - Tipo di scadenza, che sia ammissibile per il  *
      *              *   tipo di avviso richiesto                      *
      *              * - Tipo di acquisizione della scadenza, che sia  *
      *              *   ammissibile per il tipo di distinta           *
      *              * - Data di registrazione movimento di emissione  *
      *              *   scadenza, che non sia superiore alla data di  *
      *              *   composizione della distinta                   *
      *              * - Importo della scadenza, che sia maggiore di   *
      *              *   zero                                          *
      *              * - Status della scadenza, che non abbia avuto    *
      *              *   movimenti successivi a quello di emissione,   *
      *              *-------------------------------------------------*
           perform   sel-100-sdb-000      thru sel-100-sdb-999        .
           if        w-sel-sdb-out-flg    not  = zero
                     go to qry-sel-rec-900.
       qry-sel-rec-200.
      *              *-------------------------------------------------*
      *              * 2. gruppo di selezioni sulla scadenza, su :     *
      *              *                                                 *
      *              * - Tipo di scadenza, che sia nell'elenco dei ti- *
      *              *   pi scadenza da includere                      *
      *              * - Data di scadenza, diversa da scadenza a vista,*
      *              *   che sia compresa tra la data di scadenza mi-  *
      *              *   nima e la data di scadenza massima da sele-   *
      *              *   zionare                                       *
      *              * - Data di scadenza, se a vista, che sia previ-  *
      *              *   sta come data scadenza da selezionare         *
      *              *-------------------------------------------------*
           perform   sel-300-sdb-000      thru sel-300-sdb-999        .
           if        w-sel-sdb-out-flg    not  = zero
                     go to qry-sel-rec-900.
       qry-sel-rec-300.
      *              *-------------------------------------------------*
      *              * 3. gruppo di selezioni sulla scadenza, su :     *
      *              *                                                 *
      *              * - Codice ABI, che sia presente se necessario, e *
      *              *   che trovi corrispondenza nell'archivio isti-  *
      *              *   tuti di credito                               *
      *              * - Codice CAB, che sia presente se necessario, e *
      *              *   che trovi corrispondenza nell'archivio spor-  *
      *              *   telli istituti di credito                     *
      *              * - Codice c/c di presentazione, che sia presente *
      *              *   se necessario                                 *
      *              *-------------------------------------------------*
           perform   sel-500-sdb-000      thru sel-500-sdb-999        .
           if        w-sel-sdb-out-flg    not  = zero
                     go to qry-sel-rec-900.
       qry-sel-rec-400.
      *              *-------------------------------------------------*
      *              * 4. gruppo di selezioni sulla scadenza, su :     *
      *              *                                                 *
      *              * - Circuiti interbancari, se necessario          *
      *              *-------------------------------------------------*
           perform   sel-700-sdb-000      thru sel-700-sdb-999        .
           if        w-sel-sdb-out-flg    not  = zero
                     go to qry-sel-rec-900.
       qry-sel-rec-500.
      *              *-------------------------------------------------*
      *              * Selezione su importo massimo da selezionare     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se importo massimo da selezionare a zero :  *
      *                  * nessuna selezione                           *
      *                  *---------------------------------------------*
           if        rr-imp-max           =    zero
                     go to qry-sel-rec-600.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-let-seq-sdb-igm      .
           if        w-let-seq-sdb-igm    >    rr-imp-max
                     subtract  rf-sdb-imp-sdb
                                          from rr-imp-max
                     go to qry-sel-rec-900.
       qry-sel-rec-600.
      *              *-------------------------------------------------*
      *              * Controllo che la scadenza non faccia gia' parte *
      *              * del gruppo di scadenze appartenenti alla di-    *
      *              * stinta in corso di formazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' stata passata una variabile per   *
      *                  * i numeri scadenza appartenenti alla distin- *
      *                  * ta in corso di formazione : no selezione    *
      *                  *---------------------------------------------*
           if        w-ipc-nsd-eds-snx    not  = "S"
                     go to qry-sel-rec-700.
      *                  *---------------------------------------------*
      *                  * Se il numero scadenze appartenenti alla di- *
      *                  * stinta in corso di formazione e' pari a ze- *
      *                  * ro : no selezione                           *
      *                  *---------------------------------------------*
           if        w-ipc-nsd-eds-val    =    zero
                     go to qry-sel-rec-700.
       qry-sel-rec-625.
      *                  *---------------------------------------------*
      *                  * Controllo effettivo                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-nsd-eds-i02      .
       qry-sel-rec-630.
           add       1                    to   w-ipc-nsd-eds-i02      .
           if        w-ipc-nsd-eds-i02    >    w-ipc-nsd-eds-val
                     go to qry-sel-rec-700.
           if        w-ipc-nsd-eds-nsc
                    (w-ipc-nsd-eds-i02)   =    rf-sdb-num-sdb
                     go to qry-sel-rec-900
           else      go to qry-sel-rec-630.
       qry-sel-rec-700.
      *              *-------------------------------------------------*
      *              * Fine selezioni                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-999.
       qry-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Se selezione non superata                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-999.
       qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Selezione scadenza letta per inclusione in distinta, nel- *
      *    * presentazione. Selezione di tipo 100.                     *
      *    *                                                           *
      *    * - Sul tipo di scadenza, che sia riconosciuto              *
      *    *                                                           *
      *    * - Sul tipo di distinta, che sia riconosciuto              *
      *    *                                                           *
      *    * - Sul tipo di avviso richiesto, che sia riconosciuto      *
      *    *                                                           *
      *    * - Sul tipo di scadenza, che sia ammissibile per il tipo   *
      *    *   di distinta                                             *
      *    *                                                           *
      *    * - Sul tipo di scadenza, che sia ammissibile per il tipo   *
      *    *   di avviso richiesto                                     *
      *    *                                                           *
      *    * - Sul tipo di acquisizione della scadenza, che sia am-    *
      *    *   missibile per il tipo di distinta                       *
      *    *                                                           *
      *    * - Sulla data di registrazione movimento di emissione sca- *
      *    *   denza, che non sia superiore alla data di composizione  *
      *    *   della distinta                                          *
      *    *                                                           *
      *    * - Su importo scadenza, che sia maggiore di zero           *
      *    *                                                           *
      *    * - Su status della scadenza, che non abbia avuto movimenti *
      *    *   successivi a quello di emissione                        *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - Area parametri w-sel-sdb-inp preparata                 *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-sel-sdb-out-flg : Flag fondamentale di uscita        *
      *    *    - 00 : Selezione superata                              *
      *    *    - nn : Selezione non superata, con causa codice nn     *
      *    *           - 11 : Tipo scadenza non riconosciuto           *
      *    *           - 12 : Tipo distinta non riconosciuto           *
      *    *           - 13 : Tipo avviso richiesto non riconosciuto   *
      *    *           - 14 : Tipo scadenza non ammissibile per il ti- *
      *    *                  po di distinta di presentazione          *
      *    *           - 15 : Tipo scadenza non ammissibile per il ti- *
      *    *                  po di avviso richiesto                   *
      *    *           - 16 : Tipo acquisizione scadenza non ammissi-  *
      *    *                  bile per il tipo di distinta di presen-  *
      *    *                  tazione                                  *
      *    *           - 17 : Data di registrazione del movimento di   *
      *    *                  emissione scadenza superiore alla data   *
      *    *                  di composizione della distinta           *
      *    *           - 18 : Importo scadenza non superiore a zero    *
      *    *           - 19 : Status scadenza indicante movimenti suc- *
      *    *                  cessivi a quello di emissione            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-100-sdb-000.
      *              *-------------------------------------------------*
      *              * Selezione su tipo scadenza, che sia riconosciu- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        rf-sdb-tip-sdb       =    01 or
                     rf-sdb-tip-sdb       =    02 or
                     rf-sdb-tip-sdb       =    03 or
                     rf-sdb-tip-sdb       =    04 or
                     rf-sdb-tip-sdb       =    05 or
                     rf-sdb-tip-sdb       =    06 or
                     rf-sdb-tip-sdb       =    07 or
                     rf-sdb-tip-sdb       =    08 or
                     rf-sdb-tip-sdb       =    09 or
                     rf-sdb-tip-sdb       =    10 or
                     rf-sdb-tip-sdb       =    11
                     go to sel-100-sdb-100.
           move      11                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non riconosciuto                    
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-100.
      *              *-------------------------------------------------*
      *              * Selezione su tipo distinta, che sia riconosciu- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01 or
                     w-sel-sdb-inp-tdd    =    02 or
                     w-sel-sdb-inp-tdd    =    03 or
                     w-sel-sdb-inp-tdd    =    04
                     go to sel-100-sdb-300.
           move      12                   to   w-sel-sdb-out-flg      .
           move      "Tipo distinta non riconosciuto                    
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-200.
      *              *-------------------------------------------------*
      *              * Selezione su tipo avviso richiesto, che sia ri- *
      *              * conosciuto                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di distinta *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01
                     go to sel-100-sdb-220
           else if   w-sel-sdb-inp-tdd    =    02
                     go to sel-100-sdb-280
           else if   w-sel-sdb-inp-tdd    =    03
                     go to sel-100-sdb-280
           else if   w-sel-sdb-inp-tdd    =    04
                     go to sel-100-sdb-280.
       sel-100-sdb-220.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 01 : Incassi elettroni- *
      *                  * ci                                          *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tar    =    01 or
                     w-sel-sdb-inp-tar    =    02 or
                     w-sel-sdb-inp-tar    =    03 or
                     w-sel-sdb-inp-tar    =    04 or
                     w-sel-sdb-inp-tar    =    05
                     go to sel-100-sdb-300.
           move      13                   to   w-sel-sdb-out-flg      .
           move      "Tipo avviso richiesto non riconosciuto            
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-280.
      *                  *---------------------------------------------*
      *                  * Per tutti gli altri tipi di distinta        *
      *                  *---------------------------------------------*
           go to     sel-100-sdb-300.
       sel-100-sdb-300.
      *              *-------------------------------------------------*
      *              * Selezione sul tipo di scadenza, che sia ammis-  *
      *              * sibile per il tipo di distinta                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di distinta *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01
                     go to sel-100-sdb-320
           else if   w-sel-sdb-inp-tdd    =    02
                     go to sel-100-sdb-340
           else if   w-sel-sdb-inp-tdd    =    03
                     go to sel-100-sdb-360
           else if   w-sel-sdb-inp-tdd    =    04
                     go to sel-100-sdb-380.
       sel-100-sdb-320.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 01 : Incassi elettroni- *
      *                  * ci                                          *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    02 or
                     rf-sdb-tip-sdb       =    03 or
                     rf-sdb-tip-sdb       =    04 or
                     rf-sdb-tip-sdb       =    05 or
                     rf-sdb-tip-sdb       =    06
                     go to sel-100-sdb-500.
           move      14                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non ammissibile per il tipo di disti
      -              "nta            "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-340.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 02 : Effetti            *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    09 or
                     rf-sdb-tip-sdb       =    10
                     go to sel-100-sdb-500.
           move      14                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non ammissibile per il tipo di disti
      -              "nta            "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-360.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 03 : Paghero' cambiari  *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    11
                     go to sel-100-sdb-500.
           move      14                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non ammissibile per il tipo di disti
      -              "nta            "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-380.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 04 : Cessioni           *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    11
                     go to sel-100-sdb-500.
           move      14                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non ammissibile per il tipo di disti
      -              "nta            "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-400.
      *              *-------------------------------------------------*
      *              * Selezione sul tipo di scadenza, che sia ammis-  *
      *              * sibile per il tipo di avviso richiesto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di distinta *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01
                     go to sel-100-sdb-420
           else if   w-sel-sdb-inp-tdd    =    02
                     go to sel-100-sdb-480
           else if   w-sel-sdb-inp-tdd    =    03
                     go to sel-100-sdb-480
           else if   w-sel-sdb-inp-tdd    =    04
                     go to sel-100-sdb-480.
       sel-100-sdb-420.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 01 : Incassi elettroni- *
      *                  * ci                                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di av-  *
      *                      * viso richiesto                          *
      *                      *-----------------------------------------*
           if        w-sel-sdb-inp-tar    =    01
                     go to sel-100-sdb-422
           else if   w-sel-sdb-inp-tar    =    02
                     go to sel-100-sdb-422
           else if   w-sel-sdb-inp-tar    =    03
                     go to sel-100-sdb-422
           else if   w-sel-sdb-inp-tar    =    04
                     go to sel-100-sdb-422
           else if   w-sel-sdb-inp-tar    =    05
                     go to sel-100-sdb-428.
       sel-100-sdb-422.
      *                      *-----------------------------------------*
      *                      * Se tipo di avviso richiesto             *
      *                      * - 01 : Conferme d'ordine, o Ri.Ba.      *
      *                      * - 02 : Conferme d'ordine, o M.Av.       *
      *                      * - 03 : Solo Ri.Ba.                      *
      *                      * - 04 : Solo M.Av.                       *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       =    02 or
                     rf-sdb-tip-sdb       =    03 or
                     rf-sdb-tip-sdb       =    04 or
                     rf-sdb-tip-sdb       =    05
                     go to sel-100-sdb-500.
           move      15                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non ammissibile per il tipo di avvis
      -              "o richiesto    "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-428.
      *                      *-----------------------------------------*
      *                      * Se tipo di avviso richiesto             *
      *                      * - 05 : Solo R.I.D.                      *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       =    06
                     go to sel-100-sdb-500.
           move      15                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non ammissibile per il tipo di avvis
      -              "o richiesto    "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-480.
      *                  *---------------------------------------------*
      *                  * Per tutti gli altri tipi di distinta        *
      *                  * ci                                          *
      *                  *---------------------------------------------*
           go to     sel-100-sdb-500.
       sel-100-sdb-500.
      *              *-------------------------------------------------*
      *              * Selezione sul tipo di acquisizione scadenza,    *
      *              * che sia ammissibile per il tipo di distinta     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di distinta *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01
                     go to sel-100-sdb-520
           else if   w-sel-sdb-inp-tdd    =    02
                     go to sel-100-sdb-540
           else if   w-sel-sdb-inp-tdd    =    03
                     go to sel-100-sdb-560
           else if   w-sel-sdb-inp-tdd    =    04
                     go to sel-100-sdb-580.
       sel-100-sdb-520.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 01 : Incassi elettroni- *
      *                  * ci                                          *
      *                  *---------------------------------------------*
           go to     sel-100-sdb-600.
       sel-100-sdb-540.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 02 : Effetti            *
      *                  *---------------------------------------------*
           go to     sel-100-sdb-600.
       sel-100-sdb-560.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 03 : Paghero' cambiari  *
      *                  *---------------------------------------------*
           if        rf-sdb-tac-sdb       not  = 02
                     go to sel-100-sdb-600.
           move      16                   to   w-sel-sdb-out-flg      .
           move      "Tipo acquisizione scadenza non ammissibile per il 
      -              "tipo distinta  "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-580.
      *                  *---------------------------------------------*
      *                  * Se tipo di distinta 04 : Cessioni           *
      *                  *---------------------------------------------*
           if        rf-sdb-tac-sdb       =    02
                     go to sel-100-sdb-600.
           move      16                   to   w-sel-sdb-out-flg      .
           move      "Tipo acquisizione scadenza non ammissibile per il 
      -              "tipo distinta  "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-600.
      *              *-------------------------------------------------*
      *              * Selezione sulla data di registrazione movimento *
      *              * di emissione scadenza, che non sia superiore    *
      *              * alla data di composizione della distinta        *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-emi       not > w-sel-sdb-inp-dcd
                     go to sel-100-sdb-700.
           move      17                   to   w-sel-sdb-out-flg      .
           move      "Data di emissione scadenza inferiore alla data del
      -              "la distinta    "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-700.
      *              *-------------------------------------------------*
      *              * Selezione su importo scadenza, che sia superio- *
      *              * re a zero                                       *
      *              *-------------------------------------------------*
           if        rf-sdb-imp-sdb       >    zero
                     go to sel-100-sdb-800.
           move      18                   to   w-sel-sdb-out-flg      .
           move      "Importo scadenza non positivo                     
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-800.
      *              *-------------------------------------------------*
      *              * Selezione su status della scadenza, che non in- *
      *              * dichi movimenti successivi a quello di emissio- *
      *              * ne                                              *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-sto       =    zero and
                     rf-sdb-dtr-ris       =    zero and
                     rf-sdb-num-ddp       =    zero and
                     rf-sdb-dtr-rsp       =    zero and
                     rf-sdb-dtr-acs       =    zero and
                     rf-sdb-dtr-nbe       =    zero and
                     rf-sdb-dtr-pbe       =    zero and
                     rf-sdb-dtr-isp       =    zero
                     go to sel-100-sdb-900.
           move      19                   to   w-sel-sdb-out-flg      .
           move      "La scadenza ha avuto movimenti successivi all'emis
      -              "sione          "    to   w-sel-sdb-out-err      .
           go to     sel-100-sdb-999.
       sel-100-sdb-900.
      *              *-------------------------------------------------*
      *              * Selezione superata                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-sel-sdb-out-flg      .
           move      spaces               to   w-sel-sdb-out-err      .
       sel-100-sdb-999.
           exit.

      *    *===========================================================*
      *    * Selezione scadenza letta per inclusione in distinta, nel- *
      *    * presentazione. Selezione di tipo 300.                     *
      *    *                                                           *
      *    * - Sul tipo di scadenza, che sia nell'elenco dei tipi sca- *
      *    *   denza da includere                                      *
      *    *                                                           *
      *    * - Sulla data di scadenza, diversa da scadenza a vista,    *
      *    *   che sia compresa tra la data di scadenza minima e la    *
      *    *   data di scadenza massima da selezionare                 *
      *    *                                                           *
      *    * - Sulla data di scadenza, se a vista, che sia prevista    *
      *    *   come data scadenza da selezionare                       *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - Area parametri w-sel-sdb-inp preparata                 *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-sel-sdb-out-flg : Flag fondamentale di uscita        *
      *    *    - 00 : Selezione superata                              *
      *    *    - nn : Selezione non superata, con causa codice nn     *
      *    *           - 31 : Tipo scadenza non presente nella lista   *
      *    *                  dei tipi scadenza da includere           *
      *    *           - 32 : Data scadenza, diversa dalla scadenza a  *
      *    *                  vista, non compresa tra la date scadenza *
      *    *                  minima e massima da selezionare          *
      *    *           - 33 : Data scadenza a vista e selezione sulle  *
      *    *                  scadenze a vista non prevista            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-300-sdb-000.
      *              *-------------------------------------------------*
      *              * Selezione sul tipo scadenza, che sia nell'elen- *
      *              * co dei tipi scadenza da includere               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo scadenza    *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    02
                     go to sel-300-sdb-020
           else if   rf-sdb-tip-sdb       =    03
                     go to sel-300-sdb-030
           else if   rf-sdb-tip-sdb       =    04
                     go to sel-300-sdb-040
           else if   rf-sdb-tip-sdb       =    05
                     go to sel-300-sdb-050
           else if   rf-sdb-tip-sdb       =    06
                     go to sel-300-sdb-060
           else if   rf-sdb-tip-sdb       =    09
                     go to sel-300-sdb-090
           else if   rf-sdb-tip-sdb       =    10
                     go to sel-300-sdb-100
           else if   rf-sdb-tip-sdb       =    11
                     go to sel-300-sdb-110.
       sel-300-sdb-020.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 02                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0002 or
                     w-sel-sdb-inp-tsd    =    0203 or
                     w-sel-sdb-inp-tsd    =    0204 or
                     w-sel-sdb-inp-tsd    =    0206
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-030.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 03                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0003 or
                     w-sel-sdb-inp-tsd    =    0203
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-040.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 04                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0004 or
                     w-sel-sdb-inp-tsd    =    0204
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-050.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 05                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0005 or
                     w-sel-sdb-inp-tsd    =    0205
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-060.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 06                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0006
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-090.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 09                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0009 or
                     w-sel-sdb-inp-tsd    =    0910
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-100.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 10                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0010 or
                     w-sel-sdb-inp-tsd    =    0910
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-110.
      *                  *---------------------------------------------*
      *                  * Se tipo scadenza 11                         *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tsd    =    0011 or
                     w-sel-sdb-inp-tsd    =    0061
                     go to sel-300-sdb-300.
           move      31                   to   w-sel-sdb-out-flg      .
           move      "Tipo scadenza non presente nella lista tipi scaden
      -              "ze da includere"    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-300.
      *              *-------------------------------------------------*
      *              * Selezione sulla data di scadenza, diversa da    *
      *              * scadenza a vista, che sia compresa tra la data  *
      *              * di scadenza minima e la data di scadenza mas-   *
      *              * sima da selezionare                             *
      *              *-------------------------------------------------*
       sel-300-sdb-310.
           if        rf-sdb-dts-sdb       =    zero
                     go to sel-300-sdb-400.
       sel-300-sdb-320.
           if        w-sel-sdb-inp-smi    =    zero
                     go to sel-300-sdb-330.
           if        rf-sdb-dts-sdb       <    w-sel-sdb-inp-smi
                     go to sel-300-sdb-350.
       sel-300-sdb-330.
           if        w-sel-sdb-inp-sma    =    zero
                     go to sel-300-sdb-400.
           if        rf-sdb-dts-sdb       >    w-sel-sdb-inp-sma
                     go to sel-300-sdb-350
           else      go to sel-300-sdb-400.
       sel-300-sdb-350.
           move      32                   to   w-sel-sdb-out-flg      .
           move      "Data scadenza non compresa tra le date scadenza mi
      -              "nima e massima "    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-400.
      *              *-------------------------------------------------*
      *              * Selezione sulla data di scadenza a vista, che   *
      *              * sia prevista la selezione anche delle scadenze  *
      *              * a vista                                         *
      *              *-------------------------------------------------*
           if        rf-sdb-dts-sdb       not  = zero
                     go to sel-300-sdb-900.
           if        w-sel-sdb-inp-asv    =    "S"
                     go to sel-300-sdb-900.
           move      33                   to   w-sel-sdb-out-flg      .
           move      "Data scadenza a vista, e selezione scadenze a vist
      -              "a non prevista "    to   w-sel-sdb-out-err      .
           go to     sel-300-sdb-999.
       sel-300-sdb-900.
      *              *-------------------------------------------------*
      *              * Selezione superata                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-sel-sdb-out-flg      .
           move      spaces               to   w-sel-sdb-out-err      .
       sel-300-sdb-999.
           exit.

      *    *===========================================================*
      *    * Selezione scadenza letta per inclusione in distinta, nel- *
      *    * presentazione. Selezione di tipo 500.                     *
      *    *                                                           *
      *    * - Sul codice ABI, che sia presente se necessario, e che   *
      *    *   trovi corrispondenza nell'archivio istituti di credito  *
      *    *                                                           *
      *    * - Sul codice CAB, che sia presente se necessario, e che   *
      *    *   trovi corrispondenza nell'archivio sportelli istituti   *
      *    *   di credito                                              *
      *    *                                                           *
      *    * - Sul codice c/c di presentazione, che sia presente se    *
      *    *   necessario                                              *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - Area parametri w-sel-sdb-inp preparata                 *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-sel-sdb-out-flg : Flag fondamentale di uscita        *
      *    *    - 00 : Selezione superata                              *
      *    *    - nn : Selezione non superata, con causa codice nn     *
      *    *           - 51 : Manca il codice ABI per la presentazione *
      *    *           - 52 : Codice ABI presente, ma senza corrispon- *
      *    *                  denza nell'archivio istituti di credito  *
      *    *           - 53 : Manca il codice CAB per la presentazione *
      *    *           - 54 : Codice CAB presente, ma senza corrispon- *
      *    *                  denza nell'archivio sportelli istituti   *
      *    *                  di credito                               *
      *    *           - 55 : Manca il codice c/c per la presentazione *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-500-sdb-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di distinta     *
      *              *-------------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01
                     go to sel-500-sdb-100
           else if   w-sel-sdb-inp-tdd    =    02
                     go to sel-500-sdb-300
           else if   w-sel-sdb-inp-tdd    =    03
                     go to sel-500-sdb-500
           else if   w-sel-sdb-inp-tdd    =    04
                     go to sel-500-sdb-700.
       sel-500-sdb-100.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 01 : Incassi elettronici    *
      *              *-------------------------------------------------*
       sel-500-sdb-125.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di avviso   *
      *                  * richiesto                                   *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tar    =    01
                     go to sel-500-sdb-150
           else if   w-sel-sdb-inp-tar    =    02
                     go to sel-500-sdb-175
           else if   w-sel-sdb-inp-tar    =    03
                     go to sel-500-sdb-200
           else if   w-sel-sdb-inp-tar    =    04
                     go to sel-500-sdb-225
           else if   w-sel-sdb-inp-tar    =    05
                     go to sel-500-sdb-250.
       sel-500-sdb-150.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso 01 : Richieste di paga-   *
      *                  * mento, oppure Ri.Ba.                        *
      *                  *---------------------------------------------*
       sel-500-sdb-152.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-abi-app       not  = zero
                     go to sel-500-sdb-154.
           move      51                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice ABI per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-154.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio istituti di credito *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           if        w-let-arc-axi-flg    =    spaces
                     go to sel-500-sdb-156.
           move      52                   to   w-sel-sdb-out-flg      .
           move      "Codice ABI non presente in archivio istituti di cr
      -              "edito          "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-156.
      *                      *-----------------------------------------*
      *                      * Test che il codice CAB sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-cab-app       not  = zero
                     go to sel-500-sdb-158.
           move      53                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice CAB per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-158.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio sportelli istituti  *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           if        w-let-arc-axs-flg    =    spaces
                     go to sel-500-sdb-160.
           move      54                   to   w-sel-sdb-out-flg      .
           move      "Codice CAB non presente in archivio sportelli isti
      -              "tuti di credito"    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-160.
      *                      *-----------------------------------------*
      *                      * Selezioni superate                      *
      *                      *-----------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-175.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso 02 : Richieste di paga-   *
      *                  * mento, oppure M.Av.                         *
      *                  *---------------------------------------------*
       sel-500-sdb-177.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-abi-app       not  = zero
                     go to sel-500-sdb-179.
           move      51                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice ABI per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-179.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio istituti di credito *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           if        w-let-arc-axi-flg    =    spaces
                     go to sel-500-sdb-181.
           move      52                   to   w-sel-sdb-out-flg      .
           move      "Codice ABI non presente in archivio istituti di cr
      -              "edito          "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-181.
      *                      *-----------------------------------------*
      *                      * Test che il codice CAB sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-cab-app       not  = zero
                     go to sel-500-sdb-183.
           move      53                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice CAB per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-183.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio sportelli istituti  *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           if        w-let-arc-axs-flg    =    spaces
                     go to sel-500-sdb-185.
           move      54                   to   w-sel-sdb-out-flg      .
           move      "Codice CAB non presente in archivio sportelli isti
      -              "tuti di credito"    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-185.
      *                      *-----------------------------------------*
      *                      * Selezioni superate                      *
      *                      *-----------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-200.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso 03 : Solo Ri.Ba.          *
      *                  *---------------------------------------------*
       sel-500-sdb-202.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-abi-app       not  = zero
                     go to sel-500-sdb-204.
           move      51                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice ABI per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-204.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio istituti di credito *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           if        w-let-arc-axi-flg    =    spaces
                     go to sel-500-sdb-206.
           move      52                   to   w-sel-sdb-out-flg      .
           move      "Codice ABI non presente in archivio istituti di cr
      -              "edito          "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-206.
      *                      *-----------------------------------------*
      *                      * Test che il codice CAB sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-cab-app       not  = zero
                     go to sel-500-sdb-208.
           move      53                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice CAB per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-208.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio sportelli istituti  *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           if        w-let-arc-axs-flg    =    spaces
                     go to sel-500-sdb-210.
           move      54                   to   w-sel-sdb-out-flg      .
           move      "Codice CAB non presente in archivio sportelli isti
      -              "tuti di credito"    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-210.
      *                      *-----------------------------------------*
      *                      * Selezioni superate                      *
      *                      *-----------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-225.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso 04 : Solo M.Av.           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selezioni superate                      *
      *                      *-----------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-250.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso 05 : Solo R.I.D.          *
      *                  *---------------------------------------------*
       sel-500-sdb-252.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-abi-app       not  = zero
                     go to sel-500-sdb-254.
           move      51                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice ABI per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-254.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio istituti di credito *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           if        w-let-arc-axi-flg    =    spaces
                     go to sel-500-sdb-256.
           move      52                   to   w-sel-sdb-out-flg      .
           move      "Codice ABI non presente in archivio istituti di cr
      -              "edito          "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-256.
      *                      *-----------------------------------------*
      *                      * Test che il codice CAB sia presente     *
      *                      *-----------------------------------------*
           if        rf-sdb-cab-app       not  = zero
                     go to sel-500-sdb-258.
           move      53                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice CAB per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-258.
      *                      *-----------------------------------------*
      *                      * Test che il codice ABI trovi corrispon- *
      *                      * denza nell'archivio sportelli istituti  *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           if        w-let-arc-axs-flg    =    spaces
                     go to sel-500-sdb-260.
           move      54                   to   w-sel-sdb-out-flg      .
           move      "Codice CAB non presente in archivio sportelli isti
      -              "tuti di credito"    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-260.
      *                      *-----------------------------------------*
      *                      * Test che il codice c/c per la presenta- *
      *                      * zione sia presente                      *
      *                      *-----------------------------------------*
           if        rf-sdb-ccc-app       not  = spaces
                     go to sel-500-sdb-262.
           move      55                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice c/c per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-262.
      *                      *-----------------------------------------*
      *                      * Selezioni superate                      *
      *                      *-----------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-300.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 02 : Effetti                *
      *              *-------------------------------------------------*
       sel-500-sdb-310.
      *                  *---------------------------------------------*
      *                  * Test che il codice ABI sia presente         *
      *                  *---------------------------------------------*
           if        rf-sdb-abi-app       not  = zero
                     go to sel-500-sdb-320.
           move      51                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice ABI per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-320.
      *                  *---------------------------------------------*
      *                  * Test che il codice ABI trovi corrispondenza *
      *                  * nell'archivio istituti di credito           *
      *                  *---------------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           if        w-let-arc-axi-flg    =    spaces
                     go to sel-500-sdb-330.
           move      52                   to   w-sel-sdb-out-flg      .
           move      "Codice ABI non presente in archivio istituti di cr
      -              "edito          "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-330.
      *                  *---------------------------------------------*
      *                  * Test che il codice CAB sia presente         *
      *                  *---------------------------------------------*
           if        rf-sdb-cab-app       not  = zero
                     go to sel-500-sdb-340.
           move      53                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice CAB per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-340.
      *                  *---------------------------------------------*
      *                  * Test che il codice CAB trovi corrispondenza *
      *                  * nell'archivio sportelli istituti di credito *
      *                  *---------------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           if        w-let-arc-axs-flg    =    spaces
                     go to sel-500-sdb-350.
           move      54                   to   w-sel-sdb-out-flg      .
           move      "Codice CAB non presente in archivio sportelli isti
      -              "tuti di credito"    to   w-sel-sdb-out-err      .
           go to     sel-500-sdb-999.
       sel-500-sdb-350.
      *                  *---------------------------------------------*
      *                  * Selezioni superate                          *
      *                  *---------------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-500.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 03 : Paghero' cambiari      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni superate                          *
      *                  *---------------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-700.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 04 : Cessioni               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni superate                          *
      *                  *---------------------------------------------*
           go to     sel-500-sdb-900.
       sel-500-sdb-900.
      *              *-------------------------------------------------*
      *              * Selezione superata                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-sel-sdb-out-flg      .
           move      spaces               to   w-sel-sdb-out-err      .
       sel-500-sdb-999.
           exit.

      *    *===========================================================*
      *    * Selezione scadenza letta per inclusione in distinta, nel- *
      *    * presentazione. Selezione di tipo 700.                     *
      *    *                                                           *
      *    * - Sui circuiti interbancari, se necessario                *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - Area parametri w-sel-sdb-inp preparata                 *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-sel-sdb-out-flg : Flag fondamentale di uscita        *
      *    *    - 00 : Selezione superata                              *
      *    *    - nn : Selezione non superata, con causa codice nn     *
      *    *           - 71 : Manca il codice ABI di presentazione     *
      *    *           - 72 : Il codice ABI di presentazione non trova *
      *    *                  corrispondenza nell'archivio istituti di *
      *    *                  credito                                  *
      *    *           - 73 : Scadenza con ABI di presentazione non    *
      *    *                  appartenente a nessuno dei max 5 cir-    *
      *    *                  cuiti da selezionare                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-700-sdb-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di distinta     *
      *              *-------------------------------------------------*
           if        w-sel-sdb-inp-tdd    =    01
                     go to sel-700-sdb-100
           else if   w-sel-sdb-inp-tdd    =    02
                     go to sel-700-sdb-300
           else if   w-sel-sdb-inp-tdd    =    03
                     go to sel-700-sdb-500
           else if   w-sel-sdb-inp-tdd    =    04
                     go to sel-700-sdb-700.
       sel-700-sdb-100.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 01 : Incassi elettronici    *
      *              *-------------------------------------------------*
       sel-700-sdb-125.
      *                  *---------------------------------------------*
      *                  * Se non esiste una lista di circuiti da se-  *
      *                  * lezionare : selezione superata              *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-cib    =    spaces
                     go to sel-700-sdb-900.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di avviso   *
      *                  * richiesto                                   *
      *                  *---------------------------------------------*
           if        w-sel-sdb-inp-tar    =    01
                     go to sel-700-sdb-150
           else if   w-sel-sdb-inp-tar    =    02
                     go to sel-700-sdb-150
           else if   w-sel-sdb-inp-tar    =    03
                     go to sel-700-sdb-150
           else if   w-sel-sdb-inp-tar    =    04
                     go to sel-700-sdb-175
           else if   w-sel-sdb-inp-tar    =    05
                     go to sel-700-sdb-150.
       sel-700-sdb-150.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso richiesto                 *
      *                  * - 01 : Conferme d'ordine, oppure Ri.Ba.     *
      *                  * - 02 : Conferme d'ordine, oppure M.Av.      *
      *                  * - 03 : Solo Ri.Ba.                          *
      *                  * - 05 : Solo R.I.D.                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se codice ABI di presentazione a zero : *
      *                      * selezione non superata                  *
      *                      *-----------------------------------------*
           if        rf-sdb-abi-app       not  = zero
                     go to sel-700-sdb-152.
           move      71                   to   w-sel-sdb-out-flg      .
           move      "Manca il codice ABI per la presentazione          
      -              "               "    to   w-sel-sdb-out-err      .
           go to     sel-700-sdb-999.
       sel-700-sdb-152.
      *                      *-----------------------------------------*
      *                      * Se il codice ABI non trova corrispon-   *
      *                      * denza nell'archivio istituti di credito *
      *                      * : selezione non superata                *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           if        w-let-arc-axi-flg    =    spaces
                     go to sel-700-sdb-154.
           move      72                   to   w-sel-sdb-out-flg      .
           move      "Codice ABI non presente in archivio istituti di cr
      -              "edito          "    to   w-sel-sdb-out-err      .
           go to     sel-700-sdb-999.
       sel-700-sdb-154.
      *                      *-----------------------------------------*
      *                      * Se il codice ABI non appartiene a nes-  *
      *                      * sun circuito interbancario si presume   *
      *                      * che appartenga a tutti i circuiti in-   *
      *                      * terbancari e : selezione superata       *
      *                      *-----------------------------------------*
           if        w-let-arc-axi-cib    =    spaces
                     go to sel-700-sdb-900.
       sel-700-sdb-156.
      *                      *-----------------------------------------*
      *                      * Se il codice ABI appartiene a qualche   *
      *                      * circuito interbancario si controlla che *
      *                      * almeno uno di questi corrisponda ad un  *
      *                      * elemento della lista dei circuiti in-   *
      *                      * terbancari da selezionare. In caso af-  *
      *                      * fermativo la selezione e' superata, in  *
      *                      * caso contrario non e' superata          *
      *                      *-----------------------------------------*
           move      zero                 to   w-sel-sdb-700-i01      .
       sel-700-sdb-158.
           add       1                    to   w-sel-sdb-700-i01      .
           if        w-sel-sdb-700-i01    >    5
                     go to sel-700-sdb-162.
           if        w-let-arc-axi-cic
                    (w-sel-sdb-700-i01)   =    spaces
                     go to sel-700-sdb-158.
           move      zero                 to   w-sel-sdb-700-i02      .
       sel-700-sdb-160.
           add       1                    to   w-sel-sdb-700-i02      .
           if        w-sel-sdb-700-i02    >    5
                     go to sel-700-sdb-158.
           if        w-let-arc-axi-cic
                    (w-sel-sdb-700-i01)   =    w-sel-sdb-inp-cic
                                              (w-sel-sdb-700-i02)
                     go to sel-700-sdb-900
           else      go to sel-700-sdb-160.
       sel-700-sdb-162.
           move      73                   to   w-sel-sdb-out-flg      .
           move      "Scadenza con ABI di presentazione estraneo ai circ
      -              "uiti bancari   "    to   w-sel-sdb-out-err      .
           go to     sel-700-sdb-999.
       sel-700-sdb-175.
      *                  *---------------------------------------------*
      *                  * Se tipo di avviso richiesto                 *
      *                  * - 04 : Solo M.Av.                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selezione superata                      *
      *                      *-----------------------------------------*
           go to     sel-700-sdb-900.
       sel-700-sdb-300.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 02 : Effetti                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione superata                          *
      *                  *---------------------------------------------*
           go to     sel-700-sdb-900.
       sel-700-sdb-500.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 03 : Paghero' cambiari      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione superata                          *
      *                  *---------------------------------------------*
           go to     sel-700-sdb-900.
       sel-700-sdb-700.
      *              *-------------------------------------------------*
      *              * Se tipo di distinta 04 : Cessioni               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione superata                          *
      *                  *---------------------------------------------*
           go to     sel-700-sdb-900.
       sel-700-sdb-900.
      *              *-------------------------------------------------*
      *              * Selezione superata                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-sel-sdb-out-flg      .
           move      spaces               to   w-sel-sdb-out-err      .
       sel-700-sdb-999.
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
           if        v-res                >    2
                     go to qry-liv-det-100.
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
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
       qry-liv-det-225.
      *                  *---------------------------------------------*
      *                  * Numero scadenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-sdb-num-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-250.
      *                  *---------------------------------------------*
      *                  * Importo scadenza                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      13                   to   v-pos                  .
           move      rf-sdb-imp-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-275.
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
       qry-liv-det-280.
           if        rf-sdb-dts-sdb       =    zero
                     go to qry-liv-det-285
           else      go to qry-liv-det-290.
       qry-liv-det-285.
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      31                   to   v-pos                  .
           move      " A vista"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-295.
       qry-liv-det-290.
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rf-sdb-dts-sdb       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-295.
       qry-liv-det-295.
           go to     qry-liv-det-300.
       qry-liv-det-300.
      *                  *---------------------------------------------*
      *                  * Ragione sociale del debitore                *
      *                  *---------------------------------------------*
       qry-liv-det-305.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo debitore  *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-dbt       >    1
                     go to qry-liv-det-315.
       qry-liv-det-310.
      *                      *-----------------------------------------*
      *                      * Se tipo debitore : cliente              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcc] relativa al  *
      *                          * cliente o alla sua dipendenza       *
      *                          *-------------------------------------*
           if        rf-sdb-dpz-dbt       =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      rf-sdb-cod-dbt       to   w-let-arc-dcc-cod      .
           move      rf-sdb-dpz-dbt       to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Ragione sociale in area per visua-  *
      *                          * lizzazione                          *
      *                          *-------------------------------------*
           move      w-let-arc-dcc-rag    to   v-alf                  .
      *                          *-------------------------------------*
      *                          * A visualizzazione                   *
      *                          *-------------------------------------*
           go to     qry-liv-det-320.
       qry-liv-det-315.
      *                      *-----------------------------------------*
      *                      * Se tipo debitore : diversi              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura ragione sociale del debi-   *
      *                          * tore                                *
      *                          *-------------------------------------*
           move      rf-sdb-tip-dbt       to   w-let-arc-dbt-tip      .
           move      rf-sdb-cod-dbt       to   w-let-arc-dbt-cod      .
           perform   let-arc-dbt-000      thru let-arc-dbt-999        .
      *                          *-------------------------------------*
      *                          * Ragione sociale in area per visua-  *
      *                          * lizzazione                          *
      *                          *-------------------------------------*
           move      w-let-arc-dbt-rag    to   v-alf                  .
      *                          *-------------------------------------*
      *                          * A visualizzazione                   *
      *                          *-------------------------------------*
           go to     qry-liv-det-320.
       qry-liv-det-320.
      *                      *-----------------------------------------*
      *                      * Stampa ragione sociale del debitore     *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "+"                  to   v-edm                  .
           move      rf-sdb-num-sdb       to   w-mpn-num-sdb          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-400.
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
           move      w-des-tit-pgm-ovy    to   v-alf                  .
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
       stp-int-pag-300.
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
           move      "Nr.scadenza      Importo      Scadenza            
      -              "  Cliente debitore            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-int-pag-400.
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
           move      "-----------  ---------------  --------  ----------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-int-pag-500.
      *              *-------------------------------------------------*
      *              * Separazione                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
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
      *    * Routine di lettura archivio debitori                      *
      *    *-----------------------------------------------------------*
       let-arc-dbt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dbt-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo debitore          *
      *              *-------------------------------------------------*
           if        w-let-arc-dbt-tip    =    zero
                     go to let-arc-dbt-100
           else if   w-let-arc-dbt-tip    =    01
                     go to let-arc-dbt-200
           else      go to let-arc-dbt-900.
       let-arc-dbt-100.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : zero                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore    *
      *                  *---------------------------------------------*
           if        w-let-arc-dbt-cod    =    zero
                     go to let-arc-dbt-120
           else      go to let-arc-dbt-140.
       let-arc-dbt-120.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-140.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : diverso da zero        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-dbt-flg      .
           move      all   "."            to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-200.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : 01                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cli]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-dbt-cod    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Riporto risultati da lettura                *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-flg    to   w-let-arc-dbt-flg      .
           move      w-let-arc-cli-rag    to   w-let-arc-dbt-rag      .
           move      w-let-arc-cli-via    to   w-let-arc-dbt-via      .
           move      w-let-arc-cli-loc    to   w-let-arc-dbt-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-900.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : non riconosciuto             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore    *
      *                  *---------------------------------------------*
           if        w-let-arc-dbt-cod    =    zero
                     go to let-arc-dbt-920
           else      go to let-arc-dbt-940.
       let-arc-dbt-920.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-940.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : diverso da zero        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-dbt-flg      .
           move      all   "."            to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-999.
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
      *    * Routine di lettura archivio [axi]                         *
      *    *-----------------------------------------------------------*
       let-arc-axi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axi-cod    =    zero
                     go to let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      w-let-arc-axi-cod    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axi-400.
       let-arc-axi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axi-den-abi       to   w-let-arc-axi-den      .
           move      rf-axi-cir-itb       to   w-let-arc-axi-cib      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axi-999.
       let-arc-axi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axi-flg      .
           move      all   "."            to   w-let-arc-axi-den      .
           go to     let-arc-axi-600.
       let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-den      .
       let-arc-axi-600.
           move      spaces               to   w-let-arc-axi-cib      .
       let-arc-axi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axs]                         *
      *    *-----------------------------------------------------------*
       let-arc-axs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-abi    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Test se codice agenzia a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-cab    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB"             to   f-key                  .
           move      w-let-arc-axs-abi    to   rf-axs-cod-abi         .
           move      w-let-arc-axs-cab    to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axs-400.
       let-arc-axs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axs-den-spt       to   w-let-arc-axs-den      .
           move      rf-axs-via-spt       to   w-let-arc-axs-via      .
           move      rf-axs-loc-spt       to   w-let-arc-axs-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axs-999.
       let-arc-axs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axs-flg      .
           move      all   "."            to   w-let-arc-axs-den      .
           go to     let-arc-axs-600.
       let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-den      .
       let-arc-axs-600.
           move      spaces               to   w-let-arc-axs-via      .
           move      spaces               to   w-let-arc-axs-loc      .
       let-arc-axs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axc]                         *
      *    *-----------------------------------------------------------*
       let-arc-axc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice circuito a spaces                *
      *              *-------------------------------------------------*
           if        w-let-arc-axc-cod    =    spaces
                     go to let-arc-axc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCIB    "         to   f-key                  .
           move      w-let-arc-axc-cod    to   rf-axc-cod-cib         .
           move      "pgm/abi/fls/ioc/obj/iofaxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axc-400.
       let-arc-axc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axc-des-cib       to   w-let-arc-axc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axc-999.
       let-arc-axc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axc-flg      .
           move      all   "."            to   w-let-arc-axc-des      .
           go to     let-arc-axc-999.
       let-arc-axc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axc-des      .
       let-arc-axc-999.
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
      *    * Subroutines per l'accettazione del codice circuito inter- *
      *    * bancario                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acdeaxc0.acs"                   .
