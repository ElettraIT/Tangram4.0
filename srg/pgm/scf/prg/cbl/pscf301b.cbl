       Identification Division.
       Program-Id.                                 pscf301b           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    mov                 *
      *                                   Fase:    scf301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/10/93    *
      *                       Ultima revisione:    Ndk del 03/04/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti di scadenze    *
      *                    fornitori                                   *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Interrogazione su pagamenti fornitori       *
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
                     " INTERROGAZIONE SU PAGAMENTI A FORNITORI"       .

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
      *            * Si/No funzionamento cifntco interrogazione        *
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
      *        * [sfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .

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
      *        * Per modalita' di pagamento                            *
      *        *-------------------------------------------------------*
           05  w-ipc-mod-pgf.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla modalita' *
      *            * di pagamento                                      *
      *            *---------------------------------------------------*
               10  w-ipc-mod-pgf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * modalita' di pagamento                            *
      *            *---------------------------------------------------*
               10  w-ipc-mod-pgf-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per tipo valuta pagamento                             *
      *        *-------------------------------------------------------*
           05  w-ipc-tvl-pgf.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * valuta per il pagamento                           *
      *            *---------------------------------------------------*
               10  w-ipc-tvl-pgf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al ti-  *
      *            * po di valuta per il pagamento                     *
      *            *---------------------------------------------------*
               10  w-ipc-tvl-pgf-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per sigla valuta pagamento                            *
      *        *-------------------------------------------------------*
           05  w-ipc-sgl-vlt.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla sigla     *
      *            * valuta per il pagamento                           *
      *            *---------------------------------------------------*
               10  w-ipc-sgl-vlt-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * sigla valuta per il pagamento                     *
      *            *---------------------------------------------------*
               10  w-ipc-sgl-vlt-val      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per codice ns. cassa, banca o c/c postale             *
      *        *-------------------------------------------------------*
           05  w-ipc-cbp-pgf.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c.                         *
      *            *---------------------------------------------------*
               10  w-ipc-cbp-pgf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c.                  *
      *            *---------------------------------------------------*
               10  w-ipc-cbp-pgf-val      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Per Si/No solo pagamenti da addebitare                *
      *        *-------------------------------------------------------*
           05  w-ipc-cbp-pgf.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c.                         *
      *            *---------------------------------------------------*
               10  w-ipc-snx-sda-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c.                  *
      *            *---------------------------------------------------*
               10  w-ipc-snx-sda-val      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice fornitore                                      *
      *        *-------------------------------------------------------*
           05  rr-cod-fnt                 pic  9(07)                  .
           05  rr-cod-fnt-rag             pic  x(40)                  .
           05  rr-cod-fnt-via             pic  x(40)                  .
           05  rr-cod-fnt-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del fornitore                       *
      *        *-------------------------------------------------------*
           05  rr-dpz-fnt                 pic  x(04)                  .
           05  rr-dpz-fnt-rag             pic  x(40)                  .
           05  rr-dpz-fnt-via             pic  x(40)                  .
           05  rr-dpz-fnt-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipi di pagamento da ricercare                        *
      *        *                                                       *
      *        * - 0000 : Tutti                                        *
      *        * - 1610 : Pagamenti per Contanti                       *
      *        * - 1620 : Pagamenti con Assegno                        *
      *        * - 1630 : Pagamenti C/addebito in C/C Bancario         *
      *        * - 1640 : Pagamenti C/addebito in C/C Postale          *
      *        * - 1730 : Richieste di Bonifico Bancario               *
      *        * - 1731 : Richieste di Bonifico su Estero              *
      *        * - 1740 : Bollettini di C/C Postale                    *
      *        * - 1750 : Incarichi di Pagamento Avvisi                *
      *        *-------------------------------------------------------*
           05  rr-tpg-drc                 pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Sigla valuta, se pagamento da ricercare : Richieste   *
      *        * di Bonifico su Estero                                 *
      *        *-------------------------------------------------------*
           05  rr-sgl-vlt                 pic  x(03)                  .
           05  rr-dec-vlt                 pic  9(01)                  .
           05  rr-tdc-vlt                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice ns. cassa, banca o c/c postale da ricercare    *
      *        *-------------------------------------------------------*
           05  rr-cod-cbp                 pic  x(10)                  .
           05  rr-cod-cbp-trc             pic  9(02)                  .
           05  rr-cod-cbp-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione minima da ricercare                *
      *        *-------------------------------------------------------*
           05  rr-drg-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione massima da ricercare               *
      *        *-------------------------------------------------------*
           05  rr-drg-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Importo da ricercare                                  *
      *        *-------------------------------------------------------*
           05  rr-imp-pgf                 pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Si/No solo pagamenti da addebitare                    *
      *        *-------------------------------------------------------*
           05  rr-snx-sda                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det se presenti dipendenze per il fornitore  *
      *        * commerciale                                           *
      *        *-------------------------------------------------------*
           05  w-det-snd-dcf.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, il fornitore commerciale ha dipendenze  *
      *            * - N : No, il fornitore commerciale non ha dipen-  *
      *            *       denze                                       *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice fornitore commerciale                      *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-fnt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore dipendenze rilevate                     *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza, solo se unica per il fornitore *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-dpz      pic  x(04)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-cge      pic  9(07)                  .
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
               10  w-let-arc-dcf-vlt      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvl]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvl.
               10  w-let-arc-zvl-flg      pic  x(01)                  .
               10  w-let-arc-zvl-cod      pic  x(03)                  .
               10  w-let-arc-zvl-des      pic  x(20)                  .
               10  w-let-arc-zvl-din      pic  x(20)                  .
               10  w-let-arc-zvl-dec      pic  9(01)                  .
               10  w-let-arc-zvl-tdc      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cbp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cbp.
               10  w-let-arc-cbp-flg      pic  x(01)                  .
               10  w-let-arc-cbp-tip      pic  9(02)                  .
               10  w-let-arc-cbp-cod      pic  x(10)                  .
               10  w-let-arc-cbp-des      pic  x(40)                  .
               10  w-let-arc-cbp-csc      pic  9(07)                  .
               10  w-let-arc-cbp-csa      pic  9(07)                  .
               10  w-let-arc-cbp-opc      pic  9(07)                  .
               10  w-let-arc-cbp-abi      pic  9(05)                  .
               10  w-let-arc-cbp-cab      pic  9(05)                  .
               10  w-let-arc-cbp-ccb      pic  x(12)                  .
               10  w-let-arc-cbp-cco      pic  9(07)                  .
               10  w-let-arc-cbp-bba      pic  9(07)                  .
               10  w-let-arc-cbp-bbp      pic  9(07)                  .
               10  w-let-arc-cbp-psm      pic  9(07)                  .
               10  w-let-arc-cbp-psi      pic  9(07)                  .
               10  w-let-arc-cbp-pdi      pic  9(07)                  .
               10  w-let-arc-cbp-psc      pic  9(07)                  .
               10  w-let-arc-cbp-anv      pic  9(07)                  .
               10  w-let-arc-cbp-dfp      pic  9(07)                  .
               10  w-let-arc-cbp-dfa      pic  9(07)                  .
               10  w-let-arc-cbp-onb      pic  9(07)                  .
               10  w-let-arc-cbp-inb      pic  9(07)                  .
               10  w-let-arc-cbp-ccp      pic  x(12)                  .
               10  w-let-arc-cbp-bpa      pic  9(07)                  .
               10  w-let-arc-cbp-bpp      pic  9(07)                  .
               10  w-let-arc-cbp-onp      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice fornitore                                      *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fnt              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del fornitore                       *
      *        *-------------------------------------------------------*
           05  w-sav-dpz-fnt              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tipo pagamento da ricercare                           *
      *        *-------------------------------------------------------*
           05  w-sav-tpg-drc              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Sigla valuta                                          *
      *        *-------------------------------------------------------*
           05  w-sav-sgl-vlt              pic  x(03)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice commerciale fornitore   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza fornitore    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice valuta                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nostra cassa, banca, o  *
      *    * c/c postale                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipi pagamento da ricercare                *
      *        *-------------------------------------------------------*
           05  w-exp-tpg-drc.
               10  w-exp-tpg-drc-num      pic  9(02)       value 09   .
               10  w-exp-tpg-drc-lun      pic  9(02)       value 35   .
               10  w-exp-tpg-drc-tbl.
                   15  filler             pic  x(35) value
                            "Tutti                              "     .
                   15  filler             pic  x(35) value
                            "Pagamenti per Contanti             "     .
                   15  filler             pic  x(35) value
                            "Pagamenti con Assegno              "     .
                   15  filler             pic  x(35) value
                            "Pagamenti C/addebito in C/C Banc.  "     .
                   15  filler             pic  x(35) value
                            "Pagamenti C/addebito in C/C Postale"     .
                   15  filler             pic  x(35) value
                            "Richieste di Bonifico Bancario     "     .
                   15  filler             pic  x(35) value
                            "Richieste di Bonifico su Estero    "     .
                   15  filler             pic  x(35) value
                            "Bollettini di C/C Postale          "     .
                   15  filler             pic  x(35) value
                            "Incarichi di Pagamento Avvisi      "     .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No solo pagamenti da addebitare         *
      *        *-------------------------------------------------------*
           05  w-exp-snx-sda.
               10  w-exp-snx-sda-num      pic  9(02)       value 02   .
               10  w-exp-snx-sda-lun      pic  9(02)       value 02   .
               10  w-exp-snx-sda-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

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
      *    * Work per routine stp-int-pag-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-stp-int-pag.
           05  w-stp-int-pag-x80          pic  x(80)                  .
           05  w-stp-int-pag-pnt          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per totalizzazioni                              *
      *    *-----------------------------------------------------------*
       01  w-tot.
      *        *-------------------------------------------------------*
      *        * Per totali generali                                   *
      *        *-------------------------------------------------------*
           05  w-tot-gen.
                10  w-tot-gen-ctr         pic  9(05)                  .
                10  w-tot-gen-tot         pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
      *        *-------------------------------------------------------*
      *        * Numero pagamento fornitore                            *
      *        *-------------------------------------------------------*
           05  w-mpn-num-pgf              pic  9(11)                  .

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
      *              * la modalita' di pagamento                       *
      *              *-------------------------------------------------*
           perform   ipc-mod-pgf-000      thru ipc-mod-pgf-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo valuta pagamento                        *
      *              *-------------------------------------------------*
           perform   ipc-tvl-pgf-000      thru ipc-tvl-pgf-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la sigla valuta pagamento                       *
      *              *-------------------------------------------------*
           perform   ipc-sgl-vlt-000      thru ipc-sgl-vlt-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * codice ns. cassa, banca o c/c postale           *
      *              *-------------------------------------------------*
           perform   ipc-cbp-pgf-000      thru ipc-cbp-pgf-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * Si/No solo pagamenti da addebitare              *
      *              *-------------------------------------------------*
           perform   ipc-snx-sda-000      thru ipc-snx-sda-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "INTPGF    "         to   w-spg-alf-gat          .
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
           move      "INTPGF    "         to   w-spg-alf-gat          .
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
      *    * Lettura della variabile eventuale di i.p.c. per la moda-  *
      *    * lita' di pagamento                                        *
      *    *-----------------------------------------------------------*
       ipc-mod-pgf-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'mod-pgf' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "mod-pgf"            to   s-var                  .
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
                     go to ipc-mod-pgf-200
           else      go to ipc-mod-pgf-400.
       ipc-mod-pgf-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-mod-pgf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-mod-pgf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-mod-pgf-999.
       ipc-mod-pgf-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-mod-pgf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-mod-pgf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-mod-pgf-999.
       ipc-mod-pgf-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di valuta pagamento                                       *
      *    *-----------------------------------------------------------*
       ipc-tvl-pgf-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tvl-pgf' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tvl-pgf"            to   s-var                  .
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
                     go to ipc-tvl-pgf-200
           else      go to ipc-tvl-pgf-400.
       ipc-tvl-pgf-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tvl-pgf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-tvl-pgf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tvl-pgf-999.
       ipc-tvl-pgf-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tvl-pgf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-tvl-pgf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tvl-pgf-999.
       ipc-tvl-pgf-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per la sigla  *
      *    * valuta pagamento                                          *
      *    *-----------------------------------------------------------*
       ipc-sgl-vlt-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'sgl-vlt' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "sgl-vlt"            to   s-var                  .
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
                     go to ipc-sgl-vlt-200
           else      go to ipc-sgl-vlt-400.
       ipc-sgl-vlt-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-sgl-vlt-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-sgl-vlt-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-sgl-vlt-999.
       ipc-sgl-vlt-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-sgl-vlt-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-sgl-vlt-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-sgl-vlt-999.
       ipc-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il codice *
      *    * ns. cassa, banca o c/c postale per il pagamento           *
      *    *-----------------------------------------------------------*
       ipc-cbp-pgf-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'cbp-pgf' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cbp-pgf"            to   s-var                  .
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
                     go to ipc-cbp-pgf-200
           else      go to ipc-cbp-pgf-400.
       ipc-cbp-pgf-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-cbp-pgf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-cbp-pgf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cbp-pgf-999.
       ipc-cbp-pgf-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-cbp-pgf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-cbp-pgf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cbp-pgf-999.
       ipc-cbp-pgf-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per segnale   *
      *    * Si/No solo pagamenti da addebitare                        *
      *    *-----------------------------------------------------------*
       ipc-snx-sda-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'snx-sda' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-sda"            to   s-var                  .
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
                     go to ipc-snx-sda-200
           else      go to ipc-snx-sda-400.
       ipc-snx-sda-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-snx-sda-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-snx-sda-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-sda-999.
       ipc-snx-sda-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-snx-sda-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-snx-sda-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-sda-999.
       ipc-snx-sda-999.
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
      *              *-------------------------------------------------*
      *              * Open modulo accettazione fornitore commerciale  *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione dipendenza fornitore   *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-opn-000  thru cod-cod-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice valuta          *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-opn-000  thru cod-cod-zvl-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nostra cassa,   *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-opn-000  thru cod-des-cbp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open file [sfp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * Open file [fnt]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * Open file [dcf]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Open file [zvl]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *              *-------------------------------------------------*
      *              * Open file [cbp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione fornitore commerciale *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione dipendenza fornitore  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-cls-000  thru cod-cod-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice valuta         *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-cls-000  thru cod-cod-zvl-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nostra cassa,  *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-cls-000  thru cod-des-cbp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close file [sfp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * Close file [fnt]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * Close file [dcf]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Close file [zvl]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *              *-------------------------------------------------*
      *              * Close file [cbp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del fornitore             *
      *                  *---------------------------------------------*
           perform   acc-dpz-fnt-000      thru acc-dpz-fnt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipi pagamento da ricercare                 *
      *                  *---------------------------------------------*
           perform   acc-tpg-drc-000      thru acc-tpg-drc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Sigla valuta per pagamenti da ricercare     *
      *                  *---------------------------------------------*
           perform   acc-sgl-vlt-000      thru acc-sgl-vlt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice ns. cassa, banca o c/c postale       *
      *                  *---------------------------------------------*
           perform   acc-cod-cbp-000      thru acc-cod-cbp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Data registrazione minima da ricercare      *
      *                  *---------------------------------------------*
           perform   acc-drg-min-000      thru acc-drg-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Data registrazione massima da ricercare     *
      *                  *---------------------------------------------*
           perform   acc-drg-max-000      thru acc-drg-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Importo da ricercare                        *
      *                  *---------------------------------------------*
           perform   acc-imp-pgf-000      thru acc-imp-pgf-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Si/No solo pagamenti da addebitare          *
      *                  *---------------------------------------------*
           perform   acc-snx-sda-000      thru acc-snx-sda-999        .
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
           move      "Conferma esecuzione ricerca pagamenti (S/N/E) ?"
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-fnt-000      thru pmt-cod-fnt-999        .
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
           perform   vis-cod-fnt-via-000  thru vis-cod-fnt-via-999    .
           perform   vis-cod-fnt-loc-000  thru vis-cod-fnt-loc-999    .
      *              *-------------------------------------------------*
      *              * Codice dipendenza fornitore                     *
      *              *-------------------------------------------------*
           perform   pmt-dpz-fnt-000      thru pmt-dpz-fnt-999        .
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
      *              *-------------------------------------------------*
      *              * Tipi pagamenti da ricercare                     *
      *              *-------------------------------------------------*
           perform   pmt-tpg-drc-000      thru pmt-tpg-drc-999        .
           perform   vis-tpg-drc-000      thru vis-tpg-drc-999        .
      *              *-------------------------------------------------*
      *              * Sigla valuta                                    *
      *              *-------------------------------------------------*
           perform   pmt-sgl-vlt-000      thru pmt-sgl-vlt-999        .
           perform   vis-sgl-vlt-000      thru vis-sgl-vlt-999        .
      *              *-------------------------------------------------*
      *              * Codice ns. cassa, banca o c/c postale           *
      *              *-------------------------------------------------*
           perform   pmt-cod-cbp-000      thru pmt-cod-cbp-999        .
           perform   vis-cod-cbp-000      thru vis-cod-cbp-999        .
           perform   vis-cod-cbp-des-000  thru vis-cod-cbp-des-999    .
      *              *-------------------------------------------------*
      *              * Data registrazione minima da ricercare          *
      *              *-------------------------------------------------*
           perform   pmt-drg-min-000      thru pmt-drg-min-999        .
           perform   vis-drg-min-000      thru vis-drg-min-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione massima da ricercare         *
      *              *-------------------------------------------------*
           perform   pmt-drg-max-000      thru pmt-drg-max-999        .
           perform   vis-drg-max-000      thru vis-drg-max-999        .
      *              *-------------------------------------------------*
      *              * Importo pagamento da ricercare                  *
      *              *-------------------------------------------------*
           perform   pmt-imp-pgf-000      thru pmt-imp-pgf-999        .
           perform   vis-imp-pgf-000      thru vis-imp-pgf-999        .
      *              *-------------------------------------------------*
      *              * Si/No solo pagamenti da addebitare              *
      *              *-------------------------------------------------*
           perform   pmt-snx-sda-000      thru pmt-snx-sda-999        .
           perform   vis-snx-sda-000      thru vis-snx-sda-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice fornitore                                 *
      *    *-----------------------------------------------------------*
       pmt-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice fornitore           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice dipendenza del fornitore                  *
      *    *-----------------------------------------------------------*
       pmt-dpz-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza del fornitore   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpz-fnt-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipi pagamento da ricercare                      *
      *    *-----------------------------------------------------------*
       pmt-tpg-drc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipi pagamento da ricercare:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tpg-drc-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Sigla valuta per pagamenti da ricercare          *
      *    *-----------------------------------------------------------*
       pmt-sgl-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      "Valuta :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice ns. cassa, banca o c/c postale            *
      *    *-----------------------------------------------------------*
       pmt-cod-cbp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tpg-drc           =    1610
                     move  "Su codice nostra Cassa     :"
                                          to   v-alf
           else if   rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1731 or
                     rr-tpg-drc           =    1750
                     move  "Su codice nostra Banca     :"
                                          to   v-alf
           else if   rr-tpg-drc           =    1640 or
                     rr-tpg-drc           =    1740
                     move  "Su nostro C/C Postale      :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cbp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data registrazione minima da ricercare           *
      *    *-----------------------------------------------------------*
       pmt-drg-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con data registrazione dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-drg-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data registrazione massima da ricercare          *
      *    *-----------------------------------------------------------*
       pmt-drg-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-drg-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Importo pagamento da ricercare                   *
      *    *-----------------------------------------------------------*
       pmt-imp-pgf-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo pagamento da    *
      *              * ricercare                                       *
      *              *-------------------------------------------------*
           if        rr-tpg-drc           =    1731
                     go to pmt-imp-pgf-100
           else      go to pmt-imp-pgf-200.
       pmt-imp-pgf-100.
      *              *-------------------------------------------------*
      *              * Se tipo pagamento da ricercare : 1731           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con importo pagamento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      rr-sgl-vlt           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-imp-pgf-999.
       pmt-imp-pgf-200.
      *              *-------------------------------------------------*
      *              * Se tipo pagamento da ricercare : Tutti gli al-  *
      *              * tri                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con importo pagamento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-imp-pgf-999.
       pmt-imp-pgf-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Si/No solo pagamenti da addebitare               *
      *    *-----------------------------------------------------------*
       pmt-snx-sda-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tpg-drc           =    0000 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1731 or
                     rr-tpg-drc           =    1740 or
                     rr-tpg-drc           =    1750
                     move  "Solo quelli da addebitare  :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-sda-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice fornitore                     *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      rr-cod-fnt           to   w-sav-cod-fnt          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      rr-cod-fnt           to   w-cod-mne-dcf-cod      .
           move      05                   to   w-cod-mne-dcf-lin      .
           move      30                   to   w-cod-mne-dcf-pos      .
           move      05                   to   w-cod-mne-dcf-rln      .
           move      41                   to   w-cod-mne-dcf-rps      .
           move      06                   to   w-cod-mne-dcf-vln      .
           move      41                   to   w-cod-mne-dcf-vps      .
           move      07                   to   w-cod-mne-dcf-lln      .
           move      41                   to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-fnt-999.
       acc-cod-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-fnt             .
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-fnt-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [fnt]                          *
      *                  *---------------------------------------------*
           move      rr-cod-fnt           to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale, indirizzo,  *
      *                  * localita', e sottoconto associato da ana-   *
      *                  * grafica contabile                           *
      *                  *---------------------------------------------*
           move      w-let-arc-fnt-rag    to   rr-cod-fnt-rag         .
           move      w-let-arc-fnt-via    to   rr-cod-fnt-via         .
           move      w-let-arc-fnt-loc    to   rr-cod-fnt-loc         .
       acc-cod-fnt-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
           perform   vis-cod-fnt-via-000  thru vis-cod-fnt-via-999    .
           perform   vis-cod-fnt-loc-000  thru vis-cod-fnt-loc-999    .
       acc-cod-fnt-430.
      *                  *---------------------------------------------*
      *                  * Se fornitore non esistente                  *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    =    spaces
                     go to acc-cod-fnt-440.
       acc-cod-fnt-432.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del fornitore                  *
      *                      *-----------------------------------------*
           if        rr-dpz-fnt           =    spaces and
                     rr-dpz-fnt-rag       =    spaces and
                     rr-dpz-fnt-via       =    spaces and
                     rr-dpz-fnt-loc       =    spaces
                     go to acc-cod-fnt-434.
           move      spaces               to   rr-dpz-fnt             .
           move      spaces               to   rr-dpz-fnt-rag         .
           move      spaces               to   rr-dpz-fnt-via         .
           move      spaces               to   rr-dpz-fnt-loc         .
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
       acc-cod-fnt-434.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-440.
      *                  *---------------------------------------------*
      *                  * Se codice a zero                            *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           not  = zero
                     go to acc-cod-fnt-450.
       acc-cod-fnt-442.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del fornitore                  *
      *                      *-----------------------------------------*
           if        rr-dpz-fnt           =    spaces and
                     rr-dpz-fnt-rag       =    spaces and
                     rr-dpz-fnt-via       =    spaces and
                     rr-dpz-fnt-loc       =    spaces
                     go to acc-cod-fnt-444.
           move      spaces               to   rr-dpz-fnt             .
           move      spaces               to   rr-dpz-fnt-rag         .
           move      spaces               to   rr-dpz-fnt-via         .
           move      spaces               to   rr-dpz-fnt-loc         .
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
       acc-cod-fnt-444.
      *                      *-----------------------------------------*
      *                      * Reimpostazione, a meno che non si sia   *
      *                      * in tasto Up                             *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-fnt-600
           else      go to acc-cod-fnt-100.
       acc-cod-fnt-450.
      *                  *---------------------------------------------*
      *                  * Se codice diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del confronto con  *
      *                      * il valore precedente                    *
      *                      *-----------------------------------------*
           if        rr-cod-fnt           =    w-sav-cod-fnt
                     go to acc-cod-fnt-455
           else      go to acc-cod-fnt-480.
       acc-cod-fnt-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato pari al valore pre- *
      *                      * cedente                                 *
      *                      *-----------------------------------------*
       acc-cod-fnt-457.
      *                          *-------------------------------------*
      *                          * Lettura anagrafica commerciale del  *
      *                          * fornitore principale                *
      *                          *-------------------------------------*
           move      "C"                  to   w-let-arc-dcf-tle      .
           move      rr-cod-fnt           to   w-let-arc-dcf-cod      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dell'esito     *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-fnt-467.
       acc-cod-fnt-459.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del for-  *
      *                          * nitore principale non esistente     *
      *                          *-------------------------------------*
       acc-cod-fnt-461.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        rr-dpz-fnt           =    spaces and
                     rr-dpz-fnt-rag       =    spaces and
                     rr-dpz-fnt-via       =    spaces and
                     rr-dpz-fnt-loc       =    spaces
                     go to acc-cod-fnt-463.
           move      spaces               to   rr-dpz-fnt             .
           move      spaces               to   rr-dpz-fnt-rag         .
           move      spaces               to   rr-dpz-fnt-via         .
           move      spaces               to   rr-dpz-fnt-loc         .
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
       acc-cod-fnt-463.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Manca l'anagrafica commerciale per il fornitore   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-cod-fnt-465.
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-467.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del for-  *
      *                          * nitore principale esistente         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Memorizzazione valori letti da  *
      *                              * anagrafica commerciale fornito- *
      *                              * re principale in dati per la    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           move      w-let-arc-dcf-rag    to   rr-dpz-fnt-rag         .
           move      w-let-arc-dcf-via    to   rr-dpz-fnt-via         .
           move      w-let-arc-dcf-loc    to   rr-dpz-fnt-loc         .
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice dipendenza e' a spaces op- *
      *                              * pure diverso da spaces          *
      *                              *---------------------------------*
           if        rr-dpz-fnt           =    spaces
                     go to acc-cod-fnt-469
           else      go to acc-cod-fnt-471.
       acc-cod-fnt-469.
      *                              *---------------------------------*
      *                              * Se codice dipendenza a spaces   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione valori re-  *
      *                                  * lativi alla dipendenza      *
      *                                  *-----------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
      *                                  *-----------------------------*
      *                                  * A dipendenze dall'imposta-  *
      *                                  * zione                       *
      *                                  *-----------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-471.
      *                              *---------------------------------*
      *                              * Se codice dipendenza diverso da *
      *                              * spaces                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura anagrafica commer-  *
      *                                  * ciale della dipendenza      *
      *                                  *-----------------------------*
           move      "D"                  to   w-let-arc-dcf-tle      .
           move      rr-cod-fnt           to   w-let-arc-dcf-cod      .
           move      rr-dpz-fnt           to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del-   *
      *                                  * l'esito della lettura       *
      *                                  *-----------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-fnt-473
           else      go to acc-cod-fnt-475.
       acc-cod-fnt-473.
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
           move      w-let-arc-dcf-rag    to   rr-dpz-fnt-rag         .
           move      w-let-arc-dcf-via    to   rr-dpz-fnt-via         .
           move      w-let-arc-dcf-loc    to   rr-dpz-fnt-loc         .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
      *                                      *-------------------------*
      *                                      * A dipendenze dall'impo- *
      *                                      * stazione                *
      *                                      *-------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-475.
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
           move      w-let-arc-dcf-rag    to   rr-dpz-fnt-rag         .
           move      w-let-arc-dcf-via    to   rr-dpz-fnt-via         .
           move      w-let-arc-dcf-loc    to   rr-dpz-fnt-loc         .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
      *                                      *-------------------------*
      *                                      * Messaggio di errore     *
      *                                      *-------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     rr-dpz-fnt       
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                      *-------------------------*
      *                                      * A reimpostazione        *
      *                                      *-------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-480.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso dal valore  *
      *                      * precedente                              *
      *                      *-----------------------------------------*
       acc-cod-fnt-482.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        rr-dpz-fnt           =    spaces and
                     rr-dpz-fnt-rag       =    spaces and
                     rr-dpz-fnt-via       =    spaces and
                     rr-dpz-fnt-loc       =    spaces
                     go to acc-cod-fnt-484.
           move      spaces               to   rr-dpz-fnt             .
           move      spaces               to   rr-dpz-fnt-rag         .
           move      spaces               to   rr-dpz-fnt-via         .
           move      spaces               to   rr-dpz-fnt-loc         .
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
       acc-cod-fnt-484.
      *                              *---------------------------------*
      *                              * Riaggancio                      *
      *                              *---------------------------------*
           go to     acc-cod-fnt-457.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * il fornitore in corso di trattamento        *
      *                  *---------------------------------------------*
           move      rr-cod-fnt           to   w-det-snd-dcf-fnt      .
           perform   det-snd-dcf-000      thru det-snd-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           =    w-sav-cod-fnt
                     go to acc-cod-fnt-800.
      *                      *-----------------------------------------*
      *                      * Trattamento sigla valuta                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da eseguire                 *
      *                          *-------------------------------------*
           if        rr-sgl-vlt           not  = spaces
                     go to acc-cod-fnt-800.
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcf]              *
      *                          *-------------------------------------*
           move      "C"                  to   w-let-arc-dcf-tle      .
           move      rr-cod-fnt           to   w-let-arc-dcf-cod      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                          *-------------------------------------*
      *                          * Lettura archivio [zvl]              *
      *                          *-------------------------------------*
           move      w-let-arc-dcf-vlt    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           if        w-let-arc-zvl-flg    not  = spaces
                     go to acc-cod-fnt-800.
      *                          *-------------------------------------*
      *                          * Memorizzazione e visualizzazione    *
      *                          *-------------------------------------*
           move      w-let-arc-dcf-vlt    to   rr-sgl-vlt             .
           move      w-let-arc-zvl-dec    to   rr-dec-vlt             .
           move      w-let-arc-zvl-tdc    to   rr-tdc-vlt             .
           perform   vis-sgl-vlt-000      thru vis-sgl-vlt-999        .
       acc-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fnt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fnt-100.
       acc-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore                        *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-fnt           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore, ragione sociale       *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-fnt-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore, indirizzo             *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-via-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-fnt-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore, localita'             *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-loc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-fnt-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice dipendenza fornitore          *
      *    *-----------------------------------------------------------*
       acc-dpz-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dpz-fnt-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           =    zero
                     go to acc-dpz-fnt-999.
           if        w-det-snd-dcf-snx    =    "N"
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "A*"                 to   w-cod-cod-dcf-ope      .
           move      rr-cod-fnt           to   w-cod-cod-dcf-fnt      .
           move      rr-dpz-fnt           to   w-cod-cod-dcf-cod      .
           move      09                   to   w-cod-cod-dcf-lin      .
           move      30                   to   w-cod-cod-dcf-pos      .
           move      09                   to   w-cod-cod-dcf-rln      .
           move      41                   to   w-cod-cod-dcf-rps      .
           move      10                   to   w-cod-cod-dcf-vln      .
           move      41                   to   w-cod-cod-dcf-vps      .
           move      11                   to   w-cod-cod-dcf-lln      .
           move      41                   to   w-cod-cod-dcf-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcf-cll-000  thru cod-cod-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcf-foi-000  thru cod-cod-dcf-foi-999    .
       acc-dpz-fnt-110.
           perform   cod-cod-dcf-cll-000  thru cod-cod-dcf-cll-999    .
           if        w-cod-cod-dcf-ope    =    "F+"
                     go to acc-dpz-fnt-115.
           if        w-cod-cod-dcf-ope    =    "AC"
                     go to acc-dpz-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-fnt-115.
           perform   cod-cod-dcf-foi-000  thru cod-cod-dcf-foi-999    .
           go to     acc-dpz-fnt-110.
       acc-dpz-fnt-120.
           move      w-cod-cod-dcf-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-dpz-fnt             .
       acc-dpz-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dpz-fnt-410.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se codice dipendenza   *
      *                  * a spaces oppure no                          *
      *                  *---------------------------------------------*
           if        rr-dpz-fnt           =    spaces
                     go to acc-dpz-fnt-420
           else      go to acc-dpz-fnt-440.
       acc-dpz-fnt-420.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza a spaces               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale del for- *
      *                      * nitore principale                       *
      *                      *-----------------------------------------*
           move      "C"                  to   w-let-arc-dcf-tle      .
           move      rr-cod-fnt           to   w-let-arc-dcf-cod      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale fornitore principale   *
      *                      * in dati per la dipendenza               *
      *                      *-----------------------------------------*
           move      w-let-arc-dcf-rag    to   rr-dpz-fnt-rag         .
           move      w-let-arc-dcf-via    to   rr-dpz-fnt-via         .
           move      w-let-arc-dcf-loc    to   rr-dpz-fnt-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-dpz-fnt-425
           else      go to acc-dpz-fnt-430.
       acc-dpz-fnt-425.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del fornitore *
      *                      * principale esistente                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-fnt-600.
       acc-dpz-fnt-430.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del fornitore *
      *                      * principale non esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il fornitore   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-dpz-fnt-100.
       acc-dpz-fnt-440.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza diverso da spaces      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale per la   *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           move      "D"                  to   w-let-arc-dcf-tle      .
           move      rr-cod-fnt           to   w-let-arc-dcf-cod      .
           move      rr-dpz-fnt           to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale della dipendenza in    *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcf-rag    to   rr-dpz-fnt-rag         .
           move      w-let-arc-dcf-via    to   rr-dpz-fnt-via         .
           move      w-let-arc-dcf-loc    to   rr-dpz-fnt-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
           perform   vis-dpz-fnt-via-000  thru vis-dpz-fnt-via-999    .
           perform   vis-dpz-fnt-loc-000  thru vis-dpz-fnt-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-dpz-fnt-445
           else      go to acc-dpz-fnt-450.
       acc-dpz-fnt-445.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza esistente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-fnt-600.
       acc-dpz-fnt-450.
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
                     rr-dpz-fnt       
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-dpz-fnt-100.
       acc-dpz-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dpz-fnt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dpz-fnt-100.
       acc-dpz-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza del fornitore         *
      *    *-----------------------------------------------------------*
       vis-dpz-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dpz-fnt           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza fornitore, ragione    *
      *    *                   sociale                                 *
      *    *-----------------------------------------------------------*
       vis-dpz-fnt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-fnt-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-fnt-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza fornitore, indirizzo  *
      *    *-----------------------------------------------------------*
       vis-dpz-fnt-via-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-fnt-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-fnt-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza fornitore, localita'  *
      *    *-----------------------------------------------------------*
       vis-dpz-fnt-loc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-fnt-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-fnt-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipi pagamento da ricercare                *
      *    *-----------------------------------------------------------*
       acc-tpg-drc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tpg-drc           to   w-sav-tpg-drc          .
       acc-tpg-drc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpg-drc-lun    to   v-car                  .
           move      w-exp-tpg-drc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tpg-drc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-tpg-drc           =    0000
                     move  01             to   v-num
           else if   rr-tpg-drc           =    1610
                     move  02             to   v-num
           else if   rr-tpg-drc           =    1620
                     move  03             to   v-num
           else if   rr-tpg-drc           =    1630
                     move  04             to   v-num
           else if   rr-tpg-drc           =    1640
                     move  05             to   v-num
           else if   rr-tpg-drc           =    1730
                     move  06             to   v-num
           else if   rr-tpg-drc           =    1731
                     move  07             to   v-num
           else if   rr-tpg-drc           =    1740
                     move  08             to   v-num
           else if   rr-tpg-drc           =    1750
                     move  09             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tpg-drc-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tpg-drc-999.
       acc-tpg-drc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  0000           to   rr-tpg-drc
           else if   v-num                =    02
                     move  1610           to   rr-tpg-drc
           else if   v-num                =    03
                     move  1620           to   rr-tpg-drc
           else if   v-num                =    04
                     move  1630           to   rr-tpg-drc
           else if   v-num                =    05
                     move  1640           to   rr-tpg-drc
           else if   v-num                =    06
                     move  1730           to   rr-tpg-drc
           else if   v-num                =    07
                     move  1731           to   rr-tpg-drc
           else if   v-num                =    08
                     move  1740           to   rr-tpg-drc
           else if   v-num                =    09
                     move  1750           to   rr-tpg-drc
           else      move  zero           to   rr-tpg-drc             .
       acc-tpg-drc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        v-num                >    w-exp-tpg-drc-num
                     go to acc-tpg-drc-100.
       acc-tpg-drc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           =    w-sav-tpg-drc
                     go to acc-tpg-drc-800.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione prompts                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta                            *
      *                      *-----------------------------------------*
           perform   pmt-sgl-vlt-000      thru pmt-sgl-vlt-999        .
      *                      *-----------------------------------------*
      *                      * Codice ns. cassa, banca o c/c postale   *
      *                      *-----------------------------------------*
           perform   pmt-cod-cbp-000      thru pmt-cod-cbp-999        .
      *                      *-----------------------------------------*
      *                      * Importo pagamento da ricercare          *
      *                      *-----------------------------------------*
           perform   pmt-imp-pgf-000      thru pmt-imp-pgf-999        .
      *                      *-----------------------------------------*
      *                      * Si/No solo pagamenti da addebitare      *
      *                      *-----------------------------------------*
           perform   pmt-snx-sda-000      thru pmt-snx-sda-999        .
      *                  *---------------------------------------------*
      *                  * Se valore precedente : 1731                 *
      *                  *---------------------------------------------*
           if        w-sav-tpg-drc        not  = 1731
                     go to acc-tpg-drc-620.
      *                      *-----------------------------------------*
      *                      * Trattamento sigla valuta                *
      *                      *-----------------------------------------*
           if        rr-sgl-vlt           not  = spaces
                     move  spaces         to   rr-sgl-vlt
                     move  zero           to   rr-dec-vlt
                     move  spaces         to   rr-tdc-vlt
                     perform vis-sgl-vlt-000
                                          thru vis-sgl-vlt-999        .
      *                      *-----------------------------------------*
      *                      * Trattamento importo pagamento           *
      *                      *-----------------------------------------*
           if        rr-imp-pgf           not  = zero
                     move  zero           to   rr-imp-pgf
                     perform vis-imp-pgf-000
                                          thru vis-imp-pgf-999        .
           go to     acc-tpg-drc-650.
       acc-tpg-drc-620.
      *                  *---------------------------------------------*
      *                  * Se valore attuale : 1731                    *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           not  = 1731
                     go to acc-tpg-drc-650.
      *                      *-----------------------------------------*
      *                      * Preparazione sigla valuta               *
      *                      *-----------------------------------------*
           if        rr-sgl-vlt           not  = spaces
                     go to acc-tpg-drc-650.
           move      "C"                  to   w-let-arc-dcf-tle      .
           move      rr-cod-fnt           to   w-let-arc-dcf-cod      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
           move      w-let-arc-dcf-vlt    to   rr-sgl-vlt             .
           perform   vis-sgl-vlt-000      thru vis-sgl-vlt-999        .
       acc-tpg-drc-650.
      *                  *---------------------------------------------*
      *                  * Trattamento codice ns. cassa, banca o c/c   *
      *                  * postale                                     *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           =    0000
                     go to acc-tpg-drc-652.
           if       (rr-tpg-drc           =    1610  ) and
                    (rr-cod-cbp-trc       =    01    )
                     go to acc-tpg-drc-660.
           if       (rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1731 or
                     rr-tpg-drc           =    1750  ) and
                    (rr-cod-cbp-trc       =    02    )
                     go to acc-tpg-drc-660.
           if       (rr-tpg-drc           =    1640 or
                     rr-tpg-drc           =    1740  ) and
                    (rr-cod-cbp-trc       =    03    )
                     go to acc-tpg-drc-660.
       acc-tpg-drc-652.
           if        rr-cod-cbp           not  = spaces
                     move  spaces         to   rr-cod-cbp
                     move  spaces         to   rr-cod-cbp-des
                     perform vis-cod-cbp-000
                                          thru vis-cod-cbp-999
                     perform vis-cod-cbp-des-000
                                          thru vis-cod-cbp-des-999    .
       acc-tpg-drc-660.
      *                  *---------------------------------------------*
      *                  * Trattamento Si/No solo pagamenti da addebi- *
      *                  * tare                                        *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640
                     move  spaces         to   rr-snx-sda
                     perform vis-snx-sda-000
                                          thru vis-snx-sda-999        .
       acc-tpg-drc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tpg-drc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tpg-drc-100.
       acc-tpg-drc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipi pagamento da ricercare             *
      *    *-----------------------------------------------------------*
       vis-tpg-drc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpg-drc-lun    to   v-car                  .
           move      w-exp-tpg-drc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tpg-drc-tbl    to   v-txt                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-tpg-drc           =    0000
                     move  01             to   v-num
           else if   rr-tpg-drc           =    1610
                     move  02             to   v-num
           else if   rr-tpg-drc           =    1620
                     move  03             to   v-num
           else if   rr-tpg-drc           =    1630
                     move  04             to   v-num
           else if   rr-tpg-drc           =    1640
                     move  05             to   v-num
           else if   rr-tpg-drc           =    1730
                     move  06             to   v-num
           else if   rr-tpg-drc           =    1731
                     move  07             to   v-num
           else if   rr-tpg-drc           =    1740
                     move  08             to   v-num
           else if   rr-tpg-drc           =    1750
                     move  09             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tpg-drc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sigla valuta pagamenti da ricercare        *
      *    *-----------------------------------------------------------*
       acc-sgl-vlt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-sgl-vlt           to   w-sav-sgl-vlt       .
       acc-sgl-vlt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zvl-ope      .
           move      rr-sgl-vlt           to   w-cod-cod-zvl-cod      .
           move      13                   to   w-cod-cod-zvl-lin      .
           move      78                   to   w-cod-cod-zvl-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO"                 to   v-pfk (05)             .
           perform   cod-cod-zvl-cll-000  thru cod-cod-zvl-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zvl-foi-000  thru cod-cod-zvl-foi-999    .
       acc-sgl-vlt-110.
           perform   cod-cod-zvl-cll-000  thru cod-cod-zvl-cll-999    .
           if        w-cod-cod-zvl-ope    =    "F+"
                     go to acc-sgl-vlt-115.
           if        w-cod-cod-zvl-ope    =    "AC"
                     go to acc-sgl-vlt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgl-vlt-115.
           perform   cod-cod-zvl-foi-000  thru cod-cod-zvl-foi-999    .
           go to     acc-sgl-vlt-110.
       acc-sgl-vlt-120.
           move      w-cod-cod-zvl-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sgl-vlt-999.
       acc-sgl-vlt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sgl-vlt             .
       acc-sgl-vlt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [zvl] e memorizzazione dati    *
      *                  * associati alla valuta                       *
      *                  *---------------------------------------------*
           move      rr-sgl-vlt           to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-dec    to   rr-dec-vlt             .
           move      w-let-arc-zvl-tdc    to   rr-tdc-vlt             .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : reimpostazione    *
      *                  *---------------------------------------------*
           if        w-let-arc-zvl-flg    not  = spaces
                     go to acc-sgl-vlt-100.
      *                  *---------------------------------------------*
      *                  * Valore a Spaces non ammesso, a meno che non *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-sgl-vlt           not  = spaces
                     go to acc-sgl-vlt-600.
           if        v-key                =    "UP  "
                     go to acc-sgl-vlt-600
           else      go to acc-sgl-vlt-100.
       acc-sgl-vlt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-sgl-vlt           =    w-sav-sgl-vlt
                     go to acc-sgl-vlt-800.
      *                      *-----------------------------------------*
      *                      * Trattamento importo da ricercare        *
      *                      *-----------------------------------------*
           perform   pmt-imp-pgf-000      thru pmt-imp-pgf-999        .
           perform   vis-imp-pgf-000      thru vis-imp-pgf-999        .
       acc-sgl-vlt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sgl-vlt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sgl-vlt-100.
       acc-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Sigla valuta pagamenti da ricercare     *
      *    *-----------------------------------------------------------*
       vis-sgl-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      rr-sgl-vlt           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice ns. cassa, banca o c/c postale      *
      *    *-----------------------------------------------------------*
       acc-cod-cbp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           =    0000
                     go to acc-cod-cbp-999.
      *                  *---------------------------------------------*
      *                  * Determinazione tipo record [cbp]            *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           =    1610
                     move  01             to   rr-cod-cbp-trc
           else if   rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1731 or
                     rr-tpg-drc           =    1750
                     move  02             to   rr-cod-cbp-trc
           else if   rr-tpg-drc           =    1640 or
                     rr-tpg-drc           =    1740
                     move  03             to   rr-cod-cbp-trc         .
       acc-cod-cbp-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           if        rr-cod-cbp-trc       =    01
                     move  "Digitare un codice nostra Cassa oppure Retur
      -                    "n per selezionarle tutte"
                                          to   v-not
           else if   rr-cod-cbp-trc       =    02
                     move  "Digitare un codice nostra Banca oppure Retur
      -                    "n per selezionarle tutte"
                                          to   v-not
           else if   rr-cod-cbp-trc       =    03
                     move  "Digitare un codice nostro C/C Postale oppure
      -                    " Return per selezionarli tutti"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-cbp-ope      .
           move      rr-cod-cbp-trc       to   w-cod-des-cbp-tip      .
           move      rr-cod-cbp           to   w-cod-des-cbp-cod      .
           move      15                   to   w-cod-des-cbp-lin      .
           move      30                   to   w-cod-des-cbp-pos      .
           move      15                   to   w-cod-des-cbp-dln      .
           move      41                   to   w-cod-des-cbp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-cbp-cll-000  thru cod-des-cbp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-cbp-foi-000  thru cod-des-cbp-foi-999    .
       acc-cod-cbp-110.
           perform   cod-des-cbp-cll-000  thru cod-des-cbp-cll-999    .
           if        w-cod-des-cbp-ope    =    "F+"
                     go to acc-cod-cbp-115.
           if        w-cod-des-cbp-ope    =    "AC"
                     go to acc-cod-cbp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cbp-115.
           perform   cod-des-cbp-foi-000  thru cod-des-cbp-foi-999    .
           go to     acc-cod-cbp-110.
       acc-cod-cbp-120.
           move      w-cod-des-cbp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cod-cbp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cbp-999.
       acc-cod-cbp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-cbp             .
       acc-cod-cbp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cbp]                      *
      *                  *---------------------------------------------*
           move      rr-cod-cbp-trc       to   w-let-arc-cbp-tip      .
           move      rr-cod-cbp           to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-cbp-des    to   rr-cod-cbp-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-cbp-des-000  thru vis-cod-cbp-des-999    .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : reimpostazione    *
      *                  *---------------------------------------------*
           if        w-let-arc-cbp-flg    not  = spaces
                     go to acc-cod-cbp-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : visualizzazione lite-  *
      *                  * ral                                         *
      *                  *---------------------------------------------*
           if        rr-cod-cbp           not  = spaces
                     go to acc-cod-cbp-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti     "         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cod-cbp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cbp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cbp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cbp-100.
       acc-cod-cbp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice ns. cassa, banca o c/c postale   *
      *    *-----------------------------------------------------------*
       vis-cod-cbp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-cbp           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cod-cbp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione ns. cassa, banca o c/c po-  *
      *    * stale                                                     *
      *    *-----------------------------------------------------------*
       vis-cod-cbp-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cbp-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cod-cbp-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data registrazione minima da ricercare     *
      *    *-----------------------------------------------------------*
       acc-drg-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-drg-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-drg-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-drg-min-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-drg-min-999.
       acc-drg-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-drg-min             .
       acc-drg-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-drg-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-drg-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-drg-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-drg-min-100.
       acc-drg-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data registrazione minima da ricercare  *
      *    *-----------------------------------------------------------*
       vis-drg-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-drg-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-drg-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data registrazione massima da ricercare    *
      *    *-----------------------------------------------------------*
       acc-drg-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-drg-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-drg-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-drg-max-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-drg-max-999.
       acc-drg-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-drg-max             .
       acc-drg-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-drg-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-drg-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-drg-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-drg-max-100.
       acc-drg-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data registrazione massima da ricercare *
      *    *-----------------------------------------------------------*
       vis-drg-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-drg-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-drg-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Importo pagamento da ricercare             *
      *    *-----------------------------------------------------------*
       acc-imp-pgf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-imp-pgf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rr-dec-vlt           to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-imp-pgf           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-imp-pgf-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-imp-pgf-999.
       acc-imp-pgf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-imp-pgf             .
       acc-imp-pgf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-imp-pgf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-imp-pgf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-imp-pgf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-imp-pgf-100.
       acc-imp-pgf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Importo pagamento da ricercare          *
      *    *-----------------------------------------------------------*
       vis-imp-pgf-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rr-dec-vlt           to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-imp-pgf           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-imp-pgf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No solo pagamenti da addebitare         *
      *    *-----------------------------------------------------------*
       acc-snx-sda-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640
                     go to acc-snx-sda-999.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        rr-snx-sda           =    spaces
                     move  "S"            to   rr-snx-sda             .
       acc-snx-sda-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-sda-lun    to   v-car                  .
           move      w-exp-snx-sda-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-sda-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-snx-sda           =    "S"
                     move  01             to   v-num
           else if   rr-snx-sda           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-snx-sda-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-sda-999.
       acc-snx-sda-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-snx-sda
           else if   v-num                =    02
                     move  "N"            to   rr-snx-sda
           else      move  spaces         to   rr-snx-sda             .
       acc-snx-sda-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a spaces non ammesso a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-snx-sda           not  = spaces
                     go to acc-snx-sda-500.
           if        v-key                =    "UP  "
                     go to acc-snx-sda-600
           else      go to acc-snx-sda-100.
       acc-snx-sda-500.
      *                  *---------------------------------------------*
      *                  * Test se valore ammesso                      *
      *                  *---------------------------------------------*
           if        rr-snx-sda           not  = "S" and
                     rr-snx-sda           not  = "N"
                     go to acc-snx-sda-100.
       acc-snx-sda-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-sda-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-sda-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-sda-100.
       acc-snx-sda-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No solo pagamenti da addebitare      *
      *    *-----------------------------------------------------------*
       vis-snx-sda-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-sda-lun    to   v-car                  .
           move      w-exp-snx-sda-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-sda-tbl    to   v-txt                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-snx-sda           =    "S"
                     move  01             to   v-num
           else if   rr-snx-sda           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-snx-sda-999.
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
      *              * Controllo su codice fornitore                   *
      *              *-------------------------------------------------*
           if        rr-cod-fnt           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca il codice fornitore                         
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su tipo pagamento da ricercare        *
      *              *-------------------------------------------------*
           if        rr-tpg-drc           =    0000 or
                     rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1731 or
                     rr-tpg-drc           =    1740 or
                     rr-tpg-drc           =    1750
                     go to tdo-ric-sel-250.
           move      "Tipi pagamento da ricercare errato                
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-250.
      *              *-------------------------------------------------*
      *              * Controllo su sigla valuta                       *
      *              *-------------------------------------------------*
           if        rr-sgl-vlt           not  = spaces
                     go to tdo-ric-sel-300.
           move      "Manca la sigla valuta per i pagamenti da ricercare
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su data registrazione minima e massi- *
      *              * ma                                              *
      *              *-------------------------------------------------*
           if        rr-drg-min           =    zero or
                     rr-drg-max           =    zero
                     go to tdo-ric-sel-400.
           if        rr-drg-max           not  < rr-drg-min
                     go to tdo-ric-sel-400.
           move      "Data registrazione massima inferiore a quella mini
      -              "ma             "    to   w-err-box-err-msg      .
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
      *              * Si/No solo pagamenti da addebitare              *
      *              *-------------------------------------------------*
           if        rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640
                     go to reg-ric-sel-200.
           if        rr-snx-sda           =    spaces
                     move  "S"            to   rr-snx-sda             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Dati valuta                                     *
      *              *-------------------------------------------------*
           if        rr-sgl-vlt           =    spaces
                     move  c-sgl          to   rr-sgl-vlt
                     move  c-dec          to   rr-dec-vlt
                     move  c-tdc          to   rr-tdc-vlt             .
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
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-fnt             .
           move      spaces               to   rr-cod-fnt-rag         .
           move      spaces               to   rr-cod-fnt-via         .
           move      spaces               to   rr-cod-fnt-loc         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del fornitore             *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dpz-fnt             .
           move      spaces               to   rr-dpz-fnt-rag         .
           move      spaces               to   rr-dpz-fnt-via         .
           move      spaces               to   rr-dpz-fnt-loc         .
      *                  *---------------------------------------------*
      *                  * Tipi di pagamenti da ricercare              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tpg-drc             .
      *                  *---------------------------------------------*
      *                  * Sigla valuta per pagamenti da ricercare     *
      *                  *---------------------------------------------*
           move      spaces               to   rr-sgl-vlt             .
           move      zero                 to   rr-dec-vlt             .
           move      spaces               to   rr-tdc-vlt             .
      *                  *---------------------------------------------*
      *                  * Codice ns. cassa, banca o c/c postale       *
      *                  *---------------------------------------------*
           move      spaces               to   rr-cod-cbp             .
           move      zero                 to   rr-cod-cbp-trc         .
           move      spaces               to   rr-cod-cbp-des         .
      *                  *---------------------------------------------*
      *                  * Data registrazione minima da ricercare      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-drg-min             .
      *                  *---------------------------------------------*
      *                  * Data registrazione massima da ricercare     *
      *                  *---------------------------------------------*
           move      zero                 to   rr-drg-max             .
      *                  *---------------------------------------------*
      *                  * Importo da ricercare                        *
      *                  *---------------------------------------------*
           move      zero                 to   rr-imp-pgf             .
      *                  *---------------------------------------------*
      *                  * Si/No solo pagamenti da addebitare          *
      *                  *---------------------------------------------*
           move      spaces               to   rr-snx-sda             .
       nor-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Preparazione defaults                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipi di pagamento da ricercare              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa alla modalita' di pagamento :  *
      *                      * no preparazione default                 *
      *                      *-----------------------------------------*
           if        w-ipc-mod-pgf-snx    not  = "S"
                     go to nor-ric-sel-500.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-mod-pgf-val    not  = 01 and
                     w-ipc-mod-pgf-val    not  = 02 and
                     w-ipc-mod-pgf-val    not  = 03 and 
                     w-ipc-mod-pgf-val    not  = 04 and 
                     w-ipc-mod-pgf-val    not  = 13 and 
                     w-ipc-mod-pgf-val    not  = 14 and 
                     w-ipc-mod-pgf-val    not  = 15 
                     go to nor-ric-sel-500.
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa al tipo valuta pagamento : no  *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-ipc-tvl-pgf-snx    not  = "S"
                     go to nor-ric-sel-500.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-tvl-pgf-val    <    01 or
                     w-ipc-tvl-pgf-val    >    02
                     go to nor-ric-sel-500.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           if        w-ipc-mod-pgf-val    =    01
                     move  1610           to   rr-tpg-drc
           else if   w-ipc-mod-pgf-val    =    02
                     move  1620           to   rr-tpg-drc
           else if   w-ipc-mod-pgf-val    =    03
                     move  1630           to   rr-tpg-drc
           else if   w-ipc-mod-pgf-val    =    04
                     move  1640           to   rr-tpg-drc
           else if   w-ipc-mod-pgf-val    =    13
                     if    w-ipc-tvl-pgf-val
                                          =    01
                           move  1730     to   rr-tpg-drc
                     else  move  1731     to   rr-tpg-drc
           else if   w-ipc-mod-pgf-val    =    14
                     move  1740           to   rr-tpg-drc
           else if   w-ipc-mod-pgf-val    =    15
                     move  1750           to   rr-tpg-drc             .
       nor-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice ns. cassa, banca o c/c postale       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione tipo record [cbp]        *
      *                      *-----------------------------------------*
           if        rr-tpg-drc           =    1610
                     move  01             to   rr-cod-cbp-trc
           else if   rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1731 or
                     rr-tpg-drc           =    1750
                     move  02             to   rr-cod-cbp-trc
           else if   rr-tpg-drc           =    1640 or
                     rr-tpg-drc           =    1740
                     move  03             to   rr-cod-cbp-trc
           else      move  zero           to   rr-cod-cbp-trc         .
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa al codice ns. cassa, banca o   *
      *                      * c/c postale : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-cbp-pgf-snx    not  = "S"
                     go to nor-ric-sel-520.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha valore a   *
      *                      * spazi : no preparazione default         *
      *                      *-----------------------------------------*
           if        w-ipc-cbp-pgf-val    =    spaces
                     go to nor-ric-sel-520.
      *                      *-----------------------------------------*
      *                      * Lettura archivio [cbp]                  *
      *                      *-----------------------------------------*
           move      rr-cod-cbp-trc       to   w-let-arc-cbp-tip      .
           move      w-ipc-cbp-pgf-val    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           if        w-let-arc-cbp-flg    not  = spaces
                     go to nor-ric-sel-520.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-cbp-pgf-val    to   rr-cod-cbp             .
           move      w-let-arc-cbp-des    to   rr-cod-cbp-des         .
       nor-ric-sel-520.
      *                  *---------------------------------------------*
      *                  * Sigla valuta per il pagamento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa alla sigla valuta : no prepa-  *
      *                      * razione default                         *
      *                      *-----------------------------------------*
           if        w-ipc-sgl-vlt-snx    not  = "S"
                     go to nor-ric-sel-540.
      *                      *-----------------------------------------*
      *                      * Se tipo valuta per pagamento non ammet- *
      *                      * te sigla valuta : no preparazione defa- *
      *                      * ult                                     *
      *                      *-----------------------------------------*
           if        w-ipc-tvl-pgf-snx    not  = 02
                     go to nor-ric-sel-540.
      *                      *-----------------------------------------*
      *                      * Lettura archivio [zvl]                  *
      *                      *-----------------------------------------*
           move      w-ipc-sgl-vlt-val    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           if        w-let-arc-zvl-flg    not  = spaces
                     go to nor-ric-sel-540.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-sgl-vlt-val    to   rr-sgl-vlt             .
           move      w-let-arc-zvl-dec    to   rr-dec-vlt             .
           move      w-let-arc-zvl-tdc    to   rr-tdc-vlt             .
       nor-ric-sel-540.
      *                  *---------------------------------------------*
      *                  * Si/No solo pagamenti da addebitare          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa : no preparazione default      *
      *                      *-----------------------------------------*
           if        w-ipc-snx-sda-snx    not  = "S"
                     go to nor-ric-sel-560.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-snx-sda-val    not  = "S" and
                     w-ipc-snx-sda-val    not  = "N"
                     go to nor-ric-sel-560.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-snx-sda-val    to   rr-snx-sda             .
       nor-ric-sel-560.
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
      *              * Start su [sfp] per fornitore                    *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "FNTPGF    "         to   f-key                  .
           move      rr-cod-fnt           to   rf-sfp-cod-fnt         .
           move      zero                 to   rf-sfp-num-pgf         .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della start     *
      *              *-------------------------------------------------*
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
           move      "Nessun pagamento per il fornitore entro i limiti a
      -              "ssegnati       "    to   w-err-box-err-msg      .
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
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Next su [sfp] per fornitore                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
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
      *              * Se fine fornitore : superamento                 *
      *              *-------------------------------------------------*
           if        rf-sfp-cod-fnt       not  = rr-cod-fnt
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-tst-max-999.
       qry-tst-max-200.
      *              *-------------------------------------------------*
      *              * Fine test max                                   *
      *              *-------------------------------------------------*
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
       qry-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza del fornitore    *
      *              *-------------------------------------------------*
           if        rr-dpz-fnt           =    "*   "
                     go to qry-sel-rec-200.
           if        rf-sfp-dpz-fnt       =    rr-dpz-fnt
                     go to qry-sel-rec-200
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Selezione su modalita' di pagamento             *
      *              *-------------------------------------------------*
           if        rr-tpg-drc           =    0000
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1610 and
                     rf-sfp-mod-pgf       =    01
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1620 and
                     rf-sfp-mod-pgf       =    02
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1630 and
                     rf-sfp-mod-pgf       =    03
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1640 and
                     rf-sfp-mod-pgf       =    04
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1730 and
                     rf-sfp-mod-pgf       =    13
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1731 and
                     rf-sfp-mod-pgf       =    13
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1740 and
                     rf-sfp-mod-pgf       =    14
                     go to qry-sel-rec-300
           else if   rr-tpg-drc           =    1750 and
                     rf-sfp-mod-pgf       =    15
                     go to qry-sel-rec-300
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-300.
      *              *-------------------------------------------------*
      *              * Selezione su tipo valuta per pagamento          *
      *              *-------------------------------------------------*
           if       (rr-tpg-drc           =    0000 or
                     rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640 or
                     rr-tpg-drc           =    1730 or
                     rr-tpg-drc           =    1740 or
                     rr-tpg-drc           =    1750  ) and
                     rf-sfp-tvl-pgf       =    01
                     go to qry-sel-rec-325
           else if   rr-tpg-drc           =    1731    and
                     rf-sfp-tvl-pgf       =    02
                     go to qry-sel-rec-325
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-325.
      *              *-------------------------------------------------*
      *              * Selezione su sigla valuta                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test di selezione                           *
      *                  *---------------------------------------------*
           if        rf-sfp-sgl-vlt       not  = rr-sgl-vlt or
                     rf-sfp-dec-vlt       not  = rr-dec-vlt or
                     rf-sfp-tdc-vlt       not  = rr-tdc-vlt
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-350.
      *              *-------------------------------------------------*
      *              * Selezione su codice ns. cassa, banca o c/c po-  *
      *              * stale                                           *
      *              *-------------------------------------------------*
           if        rr-cod-cbp           =    spaces
                     go to qry-sel-rec-375.
           if        rf-sfp-cbp-pgf       not  = rr-cod-cbp
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-375.
      *              *-------------------------------------------------*
      *              * Selezione su data registrazione minima          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se data registrazione minima da ricercare   *
      *                  * a zero : nessuna selezione                  *
      *                  *---------------------------------------------*
           if        rr-drg-min           =    zero
                     go to qry-sel-rec-400.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-sfp-dtr-pgf       not  < rr-drg-min
                     go to qry-sel-rec-400
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-400.
      *              *-------------------------------------------------*
      *              * Selezione su data registrazione massima         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che la data di regi-   *
      *                  * strazione massima sia a zero o no           *
      *                  *---------------------------------------------*
           if        rr-drg-max           not  = zero
                     go to qry-sel-rec-420
           else      go to qry-sel-rec-440.
       qry-sel-rec-420.
      *                  *---------------------------------------------*
      *                  * Se data registrazione massima da ricercare  *
      *                  * non a zero                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rf-sfp-dtr-pgf       not  > rr-drg-max
                     go to qry-sel-rec-500
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-440.
      *                  *---------------------------------------------*
      *                  * Se data registrazione massima da ricercare  *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che la data di re- *
      *                      * gistrazione minima sia a zero o no      *
      *                      *-----------------------------------------*
           if        rr-drg-min           =    zero
                     go to qry-sel-rec-442
           else      go to qry-sel-rec-444.
       qry-sel-rec-442.
      *                      *-----------------------------------------*
      *                      * Se data di registrazione minima : zero  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Nessuna selezione                   *
      *                          *-------------------------------------*
           go to     qry-sel-rec-500.
       qry-sel-rec-444.
      *                      *-----------------------------------------*
      *                      * Se data di registrazione minima : non   *
      *                      * zero                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        rf-sfp-dtr-pgf       not  > rr-drg-min
                     go to qry-sel-rec-500
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-500.
      *              *-------------------------------------------------*
      *              * Selezione su importo da ricercare               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se importo da ricercare pari a zero : nes-  *
      *                  * suna selezione sull'importo                 *
      *                  *---------------------------------------------*
           if        rr-imp-pgf           =    zero
                     go to qry-sel-rec-600.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-sfp-imp-pgf       =    rr-imp-pgf
                     go to qry-sel-rec-600
           else      move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-600.
      *              *-------------------------------------------------*
      *              * Selezione su Si/No solo pagamenti da addebitare *
      *              *-------------------------------------------------*
           if        rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640
                     go to qry-sel-rec-700.
           if        rr-snx-sda           not  = "S"
                     go to qry-sel-rec-700.
           if        rf-sfp-num-adp       not  = zero
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-700.
      *              *-------------------------------------------------*
      *              * Fine selezioni                                  *
      *              *-------------------------------------------------*
           go to     qry-sel-rec-999.
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
      *              * Normalizzazione totali generali                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-gen-ctr          .
           move      zero                 to   w-tot-gen-tot          .
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
      *              *-------------------------------------------------*
      *              * Stampa totale generale                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-gen-ctr        not  > 1
                     go to qry-fin-cic-999.
      *                  *---------------------------------------------*
      *                  * Test se numero linee residue sufficienti    *
      *                  *---------------------------------------------*
           if        v-res                >    2
                     go to qry-fin-cic-100.
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-fin-cic-999.
      *                      *-----------------------------------------*
      *                      * Se intestato pagina: si saltano i trat- *
      *                      * tini per totale                         *
      *                      *-----------------------------------------*
           go to     qry-fin-cic-200.
       qry-fin-cic-100.
      *                  *---------------------------------------------*
      *                  * Stampa trattini                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      58                   to   v-pos                  .
           move      "----------------"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-fin-cic-200.
      *                  *---------------------------------------------*
      *                  * Stampa valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda che si tratti  *
      *                          * di una Richiesta di Bonifico su E-  *
      *                          * stero o no                          *
      *                          *-------------------------------------*
           if        rr-tpg-drc           =    1731
                     go to qry-fin-cic-210
           else      go to qry-fin-cic-220.
       qry-fin-cic-210.
      *                          *-------------------------------------*
      *                          * Se Richiesta di Bonifico su Estero  *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      45                   to   v-pos                  .
           move      "Totale     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      52                   to   v-pos                  .
           move      rr-sgl-vlt           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-fin-cic-250.
       qry-fin-cic-220.
      *                          *-------------------------------------*
      *                          * Se altro tipo pagamento             *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      45                   to   v-pos                  .
           move      "Totale     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-fin-cic-250.
       qry-fin-cic-250.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rr-dec-vlt           to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      58                   to   v-pos                  .
           move      w-tot-gen-tot        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
       qry-liv-det-010.
      *              *-------------------------------------------------*
      *              * Aggiornamento totale generale                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore elementi                  *
      *                  *---------------------------------------------*
           if        w-tot-gen-ctr        <    99999
                     add   1              to   w-tot-gen-ctr          .
      *                  *---------------------------------------------*
      *                  * Test su sigla valuta                        *
      *                  *---------------------------------------------*
           if        rf-sfp-sgl-vlt       not  = c-sgl
                     add   rf-sfp-iiv-pgf to   w-tot-gen-tot
           else      add   rf-sfp-imp-pgf to   w-tot-gen-tot          .
       qry-liv-det-050.
      *              *-------------------------------------------------*
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        v-res                >    1
                     go to qry-liv-det-100.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
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
      *                  * Numero pagamento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-sfp-num-pgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      13                   to   v-pos                  .
           move      rf-sfp-dtr-pgf       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo pagamento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "+"                  to   v-edm                  .
           move      rf-sfp-num-pgf       to   w-mpn-num-pgf          .
           move      w-mpn                to   v-cnt                  .
           if        rf-sfp-mod-pgf       =    01
                     move  "Pagamento per Contanti             "
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    02
                     move  "Pagamento con Assegno              "
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    03
                     move  "Pagamento C/addebito in C/C Banc.  "
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    04
                     move  "Pagamento C/addebito in C/C Postale"
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    13 and
                     rf-sfp-tvl-pgf       =    01
                     move  "Richiesta di Bonifico Bancario     "
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    13 and
                     rf-sfp-tvl-pgf       =    02
                     move  "Richiesta di Bonifico su Estero    "
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    14
                     move  "Bollettino di C/C Postale          "
                                          to   v-alf
           else if   rf-sfp-mod-pgf       =    15
                     move  "Incarico di Pagamento Avvisi       "
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-300.
      *                  *---------------------------------------------*
      *                  * Importo pagamento                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione della valuta     *
      *                      *-----------------------------------------*
           if        rf-sfp-sgl-vlt       not  = c-sgl
                     go to qry-liv-det-340.
       qry-liv-det-320.
      *                      *-----------------------------------------*
      *                      * Importo in valuta base                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      58                   to   v-pos                  .
           move      rf-sfp-imp-pgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     qry-liv-det-400.
       qry-liv-det-340.
      *                      *-----------------------------------------*
      *                      * Importo in valuta                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-sfp-dec-vlt       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      58                   to   v-pos                  .
           move      rf-sfp-iiv-pgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     qry-liv-det-400.
       qry-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Status del pagamento                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da visualizzare                 *
      *                      *-----------------------------------------*
           if        rr-tpg-drc           =    1610 or
                     rr-tpg-drc           =    1620 or
                     rr-tpg-drc           =    1630 or
                     rr-tpg-drc           =    1640
                     go to qry-liv-det-999.
      *                      *-----------------------------------------*
      *                      * Eventuale visualizzazione               *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      75                   to   v-pos                  .
           if        rr-snx-sda           not  = "S"  and
                     rf-sfp-num-adp       not  = zero
                     move  "Add.to"       to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      "          PAGAMENTI A FORNITORE         "
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
      *              * Sub-intestazione per il fornitore               *
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
           string    "Fornitore : "
                                delimited by   size
                                          into w-stp-int-pag-x80
                                  with pointer w-stp-int-pag-pnt      .
      *                      *-----------------------------------------*
      *                      * Codice fornitore                        *
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
           move      rr-cod-fnt           to   v-num                  .
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
           if        rr-dpz-fnt           =    spaces
                     go to stp-int-pag-220.
           string    "-"
                                delimited by   size
                     rr-dpz-fnt
                                delimited by   spaces
                                          into w-stp-int-pag-x80
                                  with pointer w-stp-int-pag-pnt      .
       stp-int-pag-220.
      *                      *-----------------------------------------*
      *                      * Ragione sociale fornitore               *
      *                      *-----------------------------------------*
           string    " "
                                delimited by   size
                     rr-cod-fnt-rag
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
           move      "   Numero   Data Reg.           Tipo pagamento    
      -              "            Importo      Stato"
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
           move      "----------- -------- -----------------------------
      -              "------ ---------------- ------"
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
      *                  * Se programma di espansione pagamenti gia'   *
      *                  * attivo : no function key EXPD               *
      *                  *---------------------------------------------*
           move      "ESPPGF    "         to   w-spg-alf-gat          .
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
      *                  * Scrittura della variabile 'num-pgf' per il  *
      *                  * livello di profondita' precedente o per lo  *
      *                  * stesso livello di profondita' applicativa a *
      *                  * seconda se il sottoprogramma e' stato ri-   *
      *                  * chiamato dal main oppure da un sottopro-    *
      *                  * gramma dello stesso livello                 *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-pgf"            to   s-var                  .
           if        w-ipc-tdc-mos-snx    =    "S" and
                     w-ipc-tdc-mos-val    =    "M"
                     move  "-"            to   s-dop
           else      move  "="            to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-num-pgf        to   s-num                  .
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
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Se programma di espansione pagamenti gia'   *
      *                  * attivo : uscita                             *
      *                  *---------------------------------------------*
           move      "ESPPGF    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     go to qry-trt-fun-999.
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
                    (w-tin-ele-inx)       not  = "ESPPGF    "
                     go to qry-trt-fun-320.
           if        w-tin-ovy-tin
                    (w-tin-ele-inx)       =    spaces
                     go to qry-trt-fun-999.
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-ovy-exe-pos          .
       qry-trt-fun-325.
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'num-pgf' per *
      *                  * lo stesso livello di profondita' applicati- *
      *                  * va per il numero pagamento da espandere     *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-pgf"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-num-pgf        to   s-num                  .
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
      *    * Determinazione se presenti dipendenze per il fornitore    *
      *    * commerciale                                               *
      *    *-----------------------------------------------------------*
       det-snd-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-snd-dcf-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione codice dipendenza unica         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-snd-dcf-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snd-dcf-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dcf]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-det-snd-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita con flag a 'no'        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcf-999.
       det-snd-dcf-100.
      *              *-------------------------------------------------*
      *              * Next su [dcf]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a test finale                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcf-800.
      *              *-------------------------------------------------*
      *              * Max su [dcf], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-dcf-cod-fnt       not  = w-det-snd-dcf-fnt
                     go to det-snd-dcf-800.
       det-snd-dcf-200.
      *              *-------------------------------------------------*
      *              * Test sul codice dipendenza                      *
      *              *-------------------------------------------------*
           if        rf-dcf-dpz-fnt       =    spaces
                     go to det-snd-dcf-100.
       det-snd-dcf-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snd-dcf-ctr      .
       det-snd-dcf-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione del primo codice dipendenza     *
      *              *-------------------------------------------------*
           if        w-det-snd-dcf-ctr    >    1
                     go to det-snd-dcf-500.
           move      rf-dcf-dpz-fnt       to   w-det-snd-dcf-dpz      .
       det-snd-dcf-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [dcf] successivo               *
      *              *-------------------------------------------------*
           go to     det-snd-dcf-100.
       det-snd-dcf-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * numero di dipendenze superiore a zero si    *
      *                  * esce con il flag di presenza a 'S'          *
      *                  *---------------------------------------------*
           if        w-det-snd-dcf-ctr    >    zero
                     go to det-snd-dcf-900
           else      go to det-snd-dcf-999.
       det-snd-dcf-900.
      *              *-------------------------------------------------*
      *              * Uscita per dipendenze trovate                   *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-snd-dcf-snx      .
       det-snd-dcf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
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
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-cod-cge       to   w-let-arc-fnt-cge      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-600.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-600.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-cge      .
       let-arc-fnt-999.
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
      *              * Test se codice fntente a zero                   *
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
           move      rf-dcf-cod-vlt       to   w-let-arc-dcf-vlt      .
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
           move      spaces               to   w-let-arc-dcf-vlt      .
       let-arc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zvl]                             *
      *    *-----------------------------------------------------------*
       let-arc-zvl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvl-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della sigla va- *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zvl-cod    =    c-sgl
                     go to let-arc-zvl-200
           else if   w-let-arc-zvl-cod    =    spaces
                     go to let-arc-zvl-400
           else      go to let-arc-zvl-600.
       let-arc-zvl-200.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      c-des                to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      c-din                to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      c-dec                to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      c-tdc                to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-400.
      *              *-------------------------------------------------*
      *              * Se sigla valuta a Spaces                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-600.
      *              *-------------------------------------------------*
      *              * Se sigla valuta diversa dalla valuta base ed    *
      *              * anche diversa da Spaces                         *
      *              *-------------------------------------------------*
       let-arc-zvl-650.
      *                  *---------------------------------------------*
      *                  * Lettura per sigla valuta                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVLT    "         to   f-key                  .
           move      w-let-arc-zvl-cod    to   rf-zvl-sgl-vlt         .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zvl-750.
       let-arc-zvl-700.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      rf-zvl-des-ita       to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      rf-zvl-des-int       to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      rf-zvl-num-dec       to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      rf-zvl-def-tdc       to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-750.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zvl-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cbp]                         *
      *    *-----------------------------------------------------------*
       let-arc-cbp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-cbp-cod    =    spaces
                     go to let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP    "         to   f-key                  .
           move      w-let-arc-cbp-tip    to   rf-cbp-tip-cbp         .
           move      w-let-arc-cbp-cod    to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cbp-400.
       let-arc-cbp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cbp-des-cbp       to   w-let-arc-cbp-des      .
           move      rf-cbp-stc-csc       to   w-let-arc-cbp-csc      .
           move      rf-cbp-stc-csa       to   w-let-arc-cbp-csa      .
           move      rf-cbp-stc-opc       to   w-let-arc-cbp-opc      .
           move      rf-cbp-abi-ban       to   w-let-arc-cbp-abi      .
           move      rf-cbp-cab-ban       to   w-let-arc-cbp-cab      .
           move      rf-cbp-sgl-ccb       to   w-let-arc-cbp-ccb      .
           move      rf-cbp-stc-cco       to   w-let-arc-cbp-cco      .
           move      rf-cbp-stc-bba       to   w-let-arc-cbp-bba      .
           move      rf-cbp-stc-bbp       to   w-let-arc-cbp-bbp      .
           move      rf-cbp-stc-psm       to   w-let-arc-cbp-psm      .
           move      rf-cbp-stc-psi       to   w-let-arc-cbp-psi      .
           move      rf-cbp-stc-pdi       to   w-let-arc-cbp-pdi      .
           move      rf-cbp-stc-psc       to   w-let-arc-cbp-psc      .
           move      rf-cbp-stc-anv       to   w-let-arc-cbp-anv      .
           move      rf-cbp-stc-dfp       to   w-let-arc-cbp-dfp      .
           move      rf-cbp-stc-dfa       to   w-let-arc-cbp-dfa      .
           move      rf-cbp-stc-onb       to   w-let-arc-cbp-onb      .
           move      rf-cbp-stc-inb       to   w-let-arc-cbp-inb      .
           move      rf-cbp-sgl-ccp       to   w-let-arc-cbp-ccp      .
           move      rf-cbp-stc-bpa       to   w-let-arc-cbp-bpa      .
           move      rf-cbp-stc-bpp       to   w-let-arc-cbp-bpp      .
           move      rf-cbp-stc-onp       to   w-let-arc-cbp-onp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cbp-999.
       let-arc-cbp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cbp-flg      .
           move      all   "."            to   w-let-arc-cbp-des      .
           go to     let-arc-cbp-600.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-600.
           move      zero                 to   w-let-arc-cbp-csc      .
           move      zero                 to   w-let-arc-cbp-csa      .
           move      zero                 to   w-let-arc-cbp-opc      .
           move      zero                 to   w-let-arc-cbp-abi      .
           move      zero                 to   w-let-arc-cbp-cab      .
           move      spaces               to   w-let-arc-cbp-ccb      .
           move      zero                 to   w-let-arc-cbp-cco      .
           move      zero                 to   w-let-arc-cbp-bba      .
           move      zero                 to   w-let-arc-cbp-bbp      .
           move      zero                 to   w-let-arc-cbp-psm      .
           move      zero                 to   w-let-arc-cbp-psi      .
           move      zero                 to   w-let-arc-cbp-pdi      .
           move      zero                 to   w-let-arc-cbp-psc      .
           move      zero                 to   w-let-arc-cbp-anv      .
           move      zero                 to   w-let-arc-cbp-dfp      .
           move      zero                 to   w-let-arc-cbp-dfa      .
           move      zero                 to   w-let-arc-cbp-onb      .
           move      zero                 to   w-let-arc-cbp-inb      .
           move      spaces               to   w-let-arc-cbp-ccp      .
           move      zero                 to   w-let-arc-cbp-bpa      .
           move      zero                 to   w-let-arc-cbp-bpp      .
           move      zero                 to   w-let-arc-cbp-onp      .
       let-arc-cbp-999.
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
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice dipendenza fornitore  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice valuta          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nostra cassa, o *
      *    * nostra banca, o nostro c/c postale                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acs"                   .

