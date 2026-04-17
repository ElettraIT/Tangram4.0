       Identification Division.
       Program-Id.                                 pscf301w           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    mov                 *
      *                                   Fase:    scf301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/12/95    *
      *                       Ultima revisione:    Ndk del 02/09/16    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti per gestione   *
      *                    scadenze fornitori                          *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Espansione movimenti effettuati su di una   *
      *                    scadenza                                    *
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
                     "ESPANSIONE MOVIMENTI SU DI UNA SCADENZA "       .

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
      *        * [sff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsff"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [obp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfobp"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [bef]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfbef"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .

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
      *        * Per numero scadenza fornitore                         *
      *        *-------------------------------------------------------*
           05  w-ipc-num-scf.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al numero di   *
      *            * scadenza                                          *
      *            *---------------------------------------------------*
               10  w-ipc-num-scf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al nu-  *
      *            * mero di scadenza                                  *
      *            *---------------------------------------------------*
               10  w-ipc-num-scf-val      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Numero scadenza fornitore                             *
      *        *-------------------------------------------------------*
           05  rr-num-scf                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Status visualizzazione dati scadenza                  *
      *        * - N : Non visualizzati                                *
      *        * - V : Visualizzati                                    *
      *        *-------------------------------------------------------*
           05  rr-svd-scf                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [sfs]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-sfs.
               10  w-fnd-arc-sfs-sns      pic  x(01)                  .
               10  w-fnd-arc-sfs-fnt      pic  9(07)                  .
               10  w-fnd-arc-sfs-dpf      pic  x(04)                  .
               10  w-fnd-arc-sfs-tip      pic  9(02)                  .
               10  w-fnd-arc-sfs-trs      pic  9(02)                  .
               10  w-fnd-arc-sfs-tvl      pic  9(02)                  .
               10  w-fnd-arc-sfs-vlt      pic  x(03)                  .
               10  w-fnd-arc-sfs-sdp      pic  x(01)                  .
               10  w-fnd-arc-sfs-sel      pic  x(01)                  .
               10  w-fnd-arc-sfs-num      pic  9(11)                  .
               
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
               10  w-let-arc-dcf-ccc      pic  x(12)                  .
               10  w-let-arc-dcf-ccp      pic  x(12)                  .
               10  w-let-arc-dcf-ban      pic  x(10)                  .
               10  w-let-arc-dcf-vlt      pic  x(03)                  .
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
      *        * Work per Let su archivio [bef]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-bef.
               10  w-let-arc-bef-flg      pic  x(01)                  .
               10  w-let-arc-bef-fnt      pic  9(07)                  .
               10  w-let-arc-bef-dpz      pic  x(04)                  .
               10  w-let-arc-bef-cod      pic  9(05)                  .
               10  w-let-arc-bef-des      pic  x(40)                  .
               10  w-let-arc-bef-fil      pic  x(40)                  .
               10  w-let-arc-bef-ccc      pic  x(40)                  .
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
      *        * Work per Let su archivio [obp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-obp.
               10  w-let-arc-obp-flg      pic  x(01)                  .
               10  w-let-arc-obp-tip      pic  9(02)                  .
               10  w-let-arc-obp-num      pic  9(11)                  .
               10  w-let-arc-obp-rag      pic  x(40)                  .
               10  w-let-arc-obp-via      pic  x(40)                  .
               10  w-let-arc-obp-loc      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per test se programma gia' attivo               *
      *    *-----------------------------------------------------------*
       01  w-pga.
      *        *-------------------------------------------------------*
      *        * Work per programma di interrogazione scadenze in por- *
      *        * tafoglio                                              *
      *        * - S : Si, gia' attivo                                 *
      *        * - N : No, non attivo                                  *
      *        *-------------------------------------------------------*
           05  w-pga-scf-301-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo scadenza                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-scf.
               10  w-exp-tip-scf-num      pic  9(02)       value 11   .
               10  w-exp-tip-scf-lun      pic  9(02)       value 20   .
               10  w-exp-tip-scf-tbl.
                   15  filler             pic  x(20) value
                            "Rimessa diretta     "                    .
                   15  filler             pic  x(20) value
                            "Incasso elettronico "                    .
                   15  filler             pic  x(20) value
                            "Ri.Ba.              "                    .
                   15  filler             pic  x(20) value
                            "C.d.O.              "                    .
                   15  filler             pic  x(20) value
                            "M.Av.               "                    .
                   15  filler             pic  x(20) value
                            "R.I.D.              "                    .
                   15  filler             pic  x(20) value
                            "Bonifico bancario   "                    .
                   15  filler             pic  x(20) value
                            "C/C postale         "                    .
                   15  filler             pic  x(20) value
                            "Ricevuta bancaria   "                    .
                   15  filler             pic  x(20) value
                            "Tratta              "                    .
                   15  filler             pic  x(20) value
                            "Paghero' cambiario  "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Inoltro scadenza al fornitore              *
      *        *-------------------------------------------------------*
           05  w-exp-inl-scf.
               10  w-exp-inl-scf-num      pic  9(02)       value 03   .
               10  w-exp-inl-scf-lun      pic  9(02)       value 20   .
               10  w-exp-inl-scf-tbl.
                   15  filler             pic  x(20) value
                            "Alla sede           "                    .
                   15  filler             pic  x(20) value
                            "Alla sede legale    "                    .
                   15  filler             pic  x(20) value
                            "Alla dipendenza     "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo documento di riferimento              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ddr.
               10  w-exp-tip-ddr-num      pic  9(02)       value 05   .
               10  w-exp-tip-ddr-lun      pic  9(02)       value 15   .
               10  w-exp-tip-ddr-tbl.
                   15  filler             pic  x(15) value
                            "Fattura        "                         .
                   15  filler             pic  x(15) value
                            "Nota addebito  "                         .
                   15  filler             pic  x(15) value
                            "Nota accredito "                         .
                   15  filler             pic  x(15) value
                            "Preaffiso/prof."                         .
                   15  filler             pic  x(15) value
                            "               "                         .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No pagamento bloccato                   *
      *        *-------------------------------------------------------*
           05  w-exp-snx-pbl.
               10  w-exp-snx-pbl-num      pic  9(02)       value 02   .
               10  w-exp-snx-pbl-lun      pic  9(02)       value 02   .
               10  w-exp-snx-pbl-tbl.
                   15  filler             pic  x(02)       value "No" .
                   15  filler             pic  x(02)       value "Si" .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo registrazione scadenza                *
      *        *-------------------------------------------------------*
           05  w-exp-trs-scf.
               10  w-exp-trs-scf-num      pic  9(02)       value 02   .
               10  w-exp-trs-scf-lun      pic  9(02)       value 40   .
               10  w-exp-trs-scf-tbl.
                   15  filler             pic  x(40) value
                            "Registrazione interna                   ".
                   15  filler             pic  x(40) value
                            "Cessione a fornitore                    ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/No                                      *
      *        *-------------------------------------------------------*
           05  w-exp-six-nox.
               10  w-exp-six-nox-num      pic  9(02)       value 02   .
               10  w-exp-six-nox-lun      pic  9(02)       value 02   .
               10  w-exp-six-nox-tbl.
                   15  filler             pic  x(02) value
                            "Si"                                      .
                   15  filler             pic  x(02) value
                            "No"                                      .
      *        *-------------------------------------------------------*
      *        * Work per : Modalita' di pagamento                     *
      *        *-------------------------------------------------------*
           05  w-exp-mod-pgf.
               10  w-exp-mod-pgf-num      pic  9(02)       value 07   .
               10  w-exp-mod-pgf-lun      pic  9(02)       value 40   .
               10  w-exp-mod-pgf-tbl.
                   15  filler             pic  x(40) value
                            "Per contanti                            ".
                   15  filler             pic  x(40) value
                            "Con assegno                             ".
                   15  filler             pic  x(40) value
                            "Contro addebito in C/C bancario         ".
                   15  filler             pic  x(40) value
                            "Contro addebito in C/C postale          ".
                   15  filler             pic  x(40) value
                            "Con ordine di bonifico bancario         ".
                   15  filler             pic  x(40) value
                            "Con bollettino di C/C postale           ".
                   15  filler             pic  x(40) value
                            "Incarico alla banca di pagamento avviso ".

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
      *    * Work area per interrogazione scadenza                     *
      *    *-----------------------------------------------------------*
       01  w-int-sca.
      *        *-------------------------------------------------------*
      *        * Flag per controllo ciclo interrogazione standard      *
      *        *-------------------------------------------------------*
           05  w-int-sca-flg-uno          pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per stampa valori scadenza                      *
      *    *-----------------------------------------------------------*
       01  w-stp-sfs.
      *        *-------------------------------------------------------*
      *        * Titolo per testatina per tipo operazione              *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-tst-ope          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Prompt per la stampa                                  *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-pmt-pls          pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Data per la stampa                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-dat-pls          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo s.aa.nnnnnnnn per la stampa        *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-p08-pls          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo s.aa.nnnnnn per la stampa          *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-p06-pls          pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di scadenza per la stampa                        *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-tds-pls          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 10 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-a10-pls          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 40 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-a40-pls          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 50 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-a50-pls          pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione di 40 caratteri per la stampa             *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-d40-pls          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 1 carattere per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-n01-pls          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 2 caratteri per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-n02-pls          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 5 caratteri per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-n05-pls          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 5 caratteri per la stampa con zeri  *
      *        * in testa                                              *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-z05-pls          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 7 caratteri per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-n07-pls          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico per la stampa                          *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-v11-pls          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Decimali per campo numerico                           *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-v11-dec          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione per contabilita'                   *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-drc-ope          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo per contabilita'                    *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-npc-ope          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 1                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-edt-001          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 2                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-edt-002          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 3                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-edt-003          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing di 3 caratteri fissi                 *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-edt-ax3          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Area per composizione contatore numero stampe         *
      *        *-------------------------------------------------------*
           05  w-stp-sfs-flc.
               10  w-stp-sfs-flc-snx      pic  x(02)                  .
               10  w-stp-sfs-flc-vrg      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-stp-sfs-flc-ctr      pic  z(02)                  .
               10  filler                 pic  x(01)                  .
               10  w-stp-sfs-flc-vlt      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per determinazioni varie                             *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per : Determinazioni relative alle operazioni di *
      *        *            pagamento                                  *
      *        *-------------------------------------------------------*
           05  w-det-sfp.
      *            *---------------------------------------------------*
      *            * Numero pagamento                                  *
      *            *---------------------------------------------------*
               10  w-det-sfp-num-pgf      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Numero scadenze fornitori coinvolte nell'opera-   *
      *            * zione di pagamento                                *
      *            *---------------------------------------------------*
               10  w-det-sfp-nrs-iop      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Buffer di salvataggio per il record attualmente   *
      *            * gestito in area [scf]                             *
      *            *---------------------------------------------------*
               10  w-det-sfp-buf-sfs.
                   15  filler  occurs 2048
                                          pic  x(01)                  .

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
      *              * il numero scadenza                              *
      *              *-------------------------------------------------*
           perform   ipc-num-scf-000      thru ipc-num-scf-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "ESPSCF    "         to   w-spg-alf-gat          .
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
           move      "ESPSCF    "         to   w-spg-alf-gat          .
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
      *    * Lettura della variabile eventuale di i.p.c. per il nume-  *
      *    * ro di scadenza                                            *
      *    *-----------------------------------------------------------*
       ipc-num-scf-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'num-scf' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-scf"            to   s-var                  .
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
                     go to ipc-num-scf-200
           else      go to ipc-num-scf-400.
       ipc-num-scf-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-num-scf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-num-scf-val      .
      *                  *---------------------------------------------*
      *                  * Se valore della variabile a zero : come per *
      *                  * variabile non esistente                     *
      *                  *---------------------------------------------*
           if        w-ipc-num-scf-val    =    zero
                     go to ipc-num-scf-400.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di accettazione      *
      *                  *---------------------------------------------*
           move      w-ipc-num-scf-val    to   rr-num-scf             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-scf-999.
       ipc-num-scf-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-num-scf-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-num-scf-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-scf-999.
       ipc-num-scf-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste un numero scadenza passato dal    *
      *                  * chiamante non si eseguono le richieste,     *
      *                  * altrimenti le si eseguono                   *
      *                  *---------------------------------------------*
           if        w-ipc-num-scf-snx    =    "S"   and
                     w-ipc-num-scf-val    not  = zero
                     move  "N"            to   w-cnt-fun-snx-ric
           else      move  "S"            to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste un numero scadenza passato dal    *
      *                  * chiamante il funzionamento non e' ciclico,  *
      *                  * altrimenti e' ciclico                       *
      *                  *---------------------------------------------*
           if        w-ipc-num-scf-snx    =    "S"   and
                     w-ipc-num-scf-val    not  = zero
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
      *              * Open file [sfs]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
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
      *              * Open file [sfa]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfa                 .
      *              *-------------------------------------------------*
      *              * Open file [sff]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
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
      *              * Open file [obp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
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
      *              * Open file [bef]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofbef"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close file [sfs]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
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
      *              * Close file [sfa]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfa                 .
      *              *-------------------------------------------------*
      *              * Close file [sff]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
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
      *              * Close file [obp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
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
      *              * Close file [bef]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofbef"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
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
      *                  * Numero scadenza fornitore                   *
      *                  *---------------------------------------------*
           perform   acc-num-scf-000      thru acc-num-scf-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
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
      *              *-------------------------------------------------*
      *              * Numero scadenza fornitore                       *
      *              *-------------------------------------------------*
           perform   pmt-num-scf-000      thru pmt-num-scf-999        .
      *              *-------------------------------------------------*
      *              * Trattini di chiusura                            *
      *              *-------------------------------------------------*
           perform   pmt-trt-chs-000      thru pmt-trt-chs-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt per numero scadenza fornitore                      *
      *    *-----------------------------------------------------------*
       pmt-num-scf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero scadenza            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-scf-999.
           exit.

      *    *===========================================================*
      *    * Prompt per trattini di chiusura                           *
      *    *-----------------------------------------------------------*
       pmt-trt-chs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-trt-chs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero scadenza fornitore            *
      *    *-----------------------------------------------------------*
       acc-num-scf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-scf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se programma per interrogazione sca-   *
      *                  * denze in portafoglio gia' attivo per abi-   *
      *                  * litazione tasto Find                        *
      *                  *---------------------------------------------*
           move      "INTSCF    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     move  "N"            to   w-pga-scf-301-snx
           else      move  "S"            to   w-pga-scf-301-snx      .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-pga-scf-301-snx    =    "S"
                     move  "FIND"         to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-scf           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-num-scf-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-scf-999.
       acc-num-scf-250.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-scf             .
       acc-num-scf-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-scf-400.
      *                  *---------------------------------------------*
      *                  * Find su scadenze fornitori                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/No ammissibilita' tasto Slct         *
      *                      *-----------------------------------------*
           move      "S"                  to   w-fnd-arc-sfs-sns      .
      *                      *-----------------------------------------*
      *                      * Codice fornitore                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-arc-sfs-fnt      .
      *                      *-----------------------------------------*
      *                      * Dipendenza del fornitore                *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-sfs-dpf      .
      *                      *-----------------------------------------*
      *                      * Tipo di scadenza                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-arc-sfs-tip      .
      *                      *-----------------------------------------*
      *                      * Tipo registrazione scadenza             *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-arc-sfs-trs      .
      *                      *-----------------------------------------*
      *                      * Tipo valuta per la scadenza             *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-arc-sfs-tvl      .
      *                      *-----------------------------------------*
      *                      * Sigla valuta per la scadenza            *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-sfs-vlt      .
      *                      *-----------------------------------------*
      *                      * Si/No solo scadenze da pagare           *
      *                      *-----------------------------------------*
           move      "N"                  to   w-fnd-arc-sfs-sdp      .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           perform   fnd-arc-sfs-000      thru fnd-arc-sfs-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata : a reimposta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-fnd-arc-sfs-sel    not  = spaces
                     go to acc-num-scf-100.
      *                  *---------------------------------------------*
      *                  * Ripresa numero scadenza selezionata         *
      *                  *---------------------------------------------*
           move      w-fnd-arc-sfs-num    to   rr-num-scf             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero scadenza selezionata *
      *                  *---------------------------------------------*
           perform   vis-num-scf-000      thru vis-num-scf-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura simulata del tasto Return         *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-num-scf-400.
       acc-num-scf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-scf-425.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore           *
      *                  *---------------------------------------------*
           if        rr-num-scf           =    zero
                     go to acc-num-scf-450
           else      go to acc-num-scf-475.
       acc-num-scf-450.
      *                  *---------------------------------------------*
      *                  * Se valore impostato a zero                  *
      *                  *---------------------------------------------*
       acc-num-scf-452.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dello status di    *
      *                      * visualizzazione dati scadenza           *
      *                      *-----------------------------------------*
           if        rr-svd-scf           =    "N"
                     go to acc-num-scf-454
           else      go to acc-num-scf-456.
       acc-num-scf-454.
      *                      *-----------------------------------------*
      *                      * Se dati scadenza attualmente non visua- *
      *                      * lizzati                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione             *
      *                          *-------------------------------------*
           go to     acc-num-scf-100.
       acc-num-scf-456.
      *                      *-----------------------------------------*
      *                      * Se dati scadenza attualmente visualiz-  *
      *                      * zati                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Cancellazione dati scadenza         *
      *                          *-------------------------------------*
           perform   cnc-dat-sfs-000      thru cnc-dat-sfs-999        .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione             *
      *                          *-------------------------------------*
           go to     acc-num-scf-100.
       acc-num-scf-475.
      *                  *---------------------------------------------*
      *                  * Se valore impostato diverso da zero         *
      *                  *---------------------------------------------*
       acc-num-scf-480.
      *                      *-----------------------------------------*
      *                      * Lettura record scadenza da [sfs]        *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSCF    "         to   f-key                  .
           move      rr-num-scf           to   rf-sfs-num-scf         .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
       acc-num-scf-485.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-num-scf-500
           else      go to acc-num-scf-490.
       acc-num-scf-490.
      *                      *-----------------------------------------*
      *                      * Se record scadenza non esistente        *
      *                      *-----------------------------------------*
       acc-num-scf-492.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dello status   *
      *                          * di visualizzazione dati scadenza    *
      *                          *-------------------------------------*
           if        rr-svd-scf           =    "N"
                     go to acc-num-scf-494
           else      go to acc-num-scf-496.
       acc-num-scf-494.
      *                          *-------------------------------------*
      *                          * Se dati scadenza attualmente non    *
      *                          * visualizzati                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Numero scadenza non esistente in archivio scadenze
      -              " !             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione         *
      *                              *---------------------------------*
           go to     acc-num-scf-100.
       acc-num-scf-496.
      *                          *-------------------------------------*
      *                          * Se dati scadenza attualmente visua- *
      *                          * lizzati                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Cancellazione dati scadenza     *
      *                              *---------------------------------*
           perform   cnc-dat-sfs-000      thru cnc-dat-sfs-999        .
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Numero scadenza non esistente in archivio scadenze
      -              " !             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione         *
      *                              *---------------------------------*
           go to     acc-num-scf-100.
       acc-num-scf-500.
      *                      *-----------------------------------------*
      *                      * Se record scadenza esistente            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione dati scadenza       *
      *                          *-------------------------------------*
           perform   vis-dat-sfs-000      thru vis-dat-sfs-999        .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-num-scf-600.
       acc-num-scf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-scf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-scf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-scf-100.
       acc-num-scf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero scadenza                   *
      *    *-----------------------------------------------------------*
       vis-num-scf-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-scf           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-scf-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione dati relativi alla scadenza                 *
      *    *-----------------------------------------------------------*
       cnc-dat-sfs-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dai dati relativi alla    *
      *              * scadenza                                        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       cnc-dat-sfs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione dati relativi alla scadenza               *
      *    *-----------------------------------------------------------*
       vis-dat-sfs-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dai dati relativi alla    *
      *              * scadenza                                        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione vera e propria                  *
      *              *-------------------------------------------------*
       vis-dat-sfs-125.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo scadenza              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scf-lun    to   v-car                  .
           move      w-exp-tip-scf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-scf-tbl    to   v-txt                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-tip-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-150.
      *                  *---------------------------------------------*
      *                  * Data di registrazione della scadenza        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      rf-sfs-dtr-rgs       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-sfs-175.
      *                  *---------------------------------------------*
      *                  * Codice del fornitore                        *
      *                  *---------------------------------------------*
       vis-dat-sfs-177.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice fornitore           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-179.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-cod-fnt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-181.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica del fornitore    *
      *                          *-------------------------------------*
           move      rf-sfs-cod-fnt       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione ragione sociale del *
      *                          * fornitore                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-fnt-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-200.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del fornitore             *
      *                  *---------------------------------------------*
       vis-dat-sfs-202.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza del fornitore   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-204.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-dpz-fnt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-206.
      *                      *-----------------------------------------*
      *                      * Ragione sociale, indirizzo, localita'   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica dipendenza del   *
      *                          * fornitore                           *
      *                          *-------------------------------------*
           if        rf-sfs-dpz-fnt       =    spaces
                     move  "C"            to   w-let-arc-dcf-tle
           else      move  "D"            to   w-let-arc-dcf-tle      .
           move      rf-sfs-cod-fnt       to   w-let-arc-dcf-cod      .
           move      rf-sfs-dpz-fnt       to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione ragione sociale per *
      *                          * la dipendenza del fornitore         *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-dcf-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione indirizzo per la    *
      *                          * dipendenza del fornitore            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-dcf-via    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione localita' per la    *
      *                          * dipendenza del fornitore            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-dcf-loc    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-225.
      *                  *---------------------------------------------*
      *                  * Codice nostra banca d'appoggio              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di scadenza non e' tra i se- *
      *                      * guenti:                                 *
      *                      *  - 02 : Incasso Elettronico             *
      *                      *  - 03 : Ri.Ba.                          *
      *                      *  - 04 : C.d.O.                          *
      *                      *  - 05 : M.Av.                           *
      *                      *  - 06 : R.I.D.                          *
      *                      *  - 09 : Ricevuta Bancaria               *
      *                      *  - 10 : Tratta                          *
      *                      *  - 11 : Paghero' Cambiario              *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       not  = 02 and
                     rf-sfs-tip-scf       not  = 03 and
                     rf-sfs-tip-scf       not  = 04 and
                     rf-sfs-tip-scf       not  = 05 and
                     rf-sfs-tip-scf       not  = 06 and
                     rf-sfs-tip-scf       not  = 09 and
                     rf-sfs-tip-scf       not  = 10 and
                     rf-sfs-tip-scf       not  = 11
                     go to vis-dat-sfs-250.
      *                      *-----------------------------------------*
      *                      * Se si tratta di un paghero' cambiario,  *
      *                      * ma ceduto : non si esegue nessuna vi-   *
      *                      * sualizzazione                           *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       =    11 and
                     rf-sfs-trs-scf       =    02
                     go to vis-dat-sfs-250.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rf-sfs-tip-scf       =    11
                     move  "Nostra banca domiciliazione:"
                                          to   v-alf
           else      move  "Nostra banca d'appoggio    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-cbp-nsb       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica nostra banca     *
      *                          *-------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-sfs-cbp-nsb       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione denominazione banca *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-cbp-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-250.
      *                  *---------------------------------------------*
      *                  * Codice ABI banca del fornitore              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di scadenza non e' tra i se- *
      *                      * guenti:                                 *
      *                      *  - 07 : Bonifico Bancario               *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to vis-dat-sfs-275.
      *                      *-----------------------------------------*
      *                      * Se il tipo validita' per la scadenza e' *
      *                      * tra i seguenti:                         *
      *                      *  - 02 : Scadenza Estero                 *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tvl-scf       =    02
                     go to vis-dat-sfs-275.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice ABI banca fornitore :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-abi-fnt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica istituto di cre- *
      *                          * dito                                *
      *                          *-------------------------------------*
           move      rf-sfs-abi-fnt       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione denominazione       *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-axi-den    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-275.
      *                  *---------------------------------------------*
      *                  * Codice CAB per l'appoggio in presentazione  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di scadenza non e' tra i se- *
      *                      * guenti:                                 *
      *                      *  - 07 : Bonifico Bancario               *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to vis-dat-sfs-300.
      *                      *-----------------------------------------*
      *                      * Se il tipo valuta per la scadenza e'    *
      *                      * tra i seguenti:                         *
      *                      *  - 02 : Scadenza Estero                 *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tvl-scf       =    02
                     go to vis-dat-sfs-300.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice CAB banca fornitore :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-cab-fnt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica sportelli isti-  *
      *                          * tuti di credito                     *
      *                          *-------------------------------------*
           move      rf-sfs-abi-fnt       to   w-let-arc-axs-abi      .
           move      rf-sfs-cab-fnt       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione denominazione       *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-axs-den    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-300.
      *                  *---------------------------------------------*
      *                  * Codice c/c bancario o postale per il forni- *
      *                  * tore                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di scadenza non e' tra i se- *
      *                      * guenti:                                 *
      *                      *  - 07 : Bonifico Bancario               *
      *                      *  - 08 : C/C Postale                     *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       not  = 07 and
                     rf-sfs-tip-scf       not  = 08
                     go to vis-dat-sfs-325.
      *                      *-----------------------------------------*
      *                      * Se il tipo validita' per la scadenza e' *
      *                      * tra i seguenti:                         *
      *                      *  - 02 : Scadenza Estero                 *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tvl-scf       =    02
                     go to vis-dat-sfs-325.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        rf-sfs-tip-scf       =    07
                     move  15             to   v-lin
           else      move  13             to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rf-sfs-tip-scf       =    07
                     move  "Conto corrente fornitore   :"
                                          to   v-alf
           else      move  "C/C postale del fornitore  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           if        rf-sfs-tip-scf       =    07
                     move  15             to   v-lin
           else      move  13             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-ccc-fnt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-325.
      *                  *---------------------------------------------*
      *                  * Codice banca estera                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di scadenza non e' tra i se- *
      *                      * guenti:                                 *
      *                      *  - 07 : Bonifico Bancario               *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to vis-dat-sfs-350.
      *                      *-----------------------------------------*
      *                      * Se il tipo validita' per la scadenza    *
      *                      * non e' tra i seguenti:                  *
      *                      *  - 02 : Scadenza Estero                 *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-tvl-scf       not  = 02
                     go to vis-dat-sfs-350.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Banca d'appoggio estera    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-cod-bef       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione, filiale e  *
      *                      * conto corrente                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio banche estere per  *
      *                          * i fornitori [bef], per il codice    *
      *                          * banca estera del fornitore          *
      *                          *-------------------------------------*
           move      rf-sfs-cod-fnt       to   w-let-arc-bef-fnt      .
           move      rf-sfs-dpz-fnt       to   w-let-arc-bef-dpz      .
           move      rf-sfs-cod-bef       to   w-let-arc-bef-cod      .
           perform   let-arc-bef-000      thru let-arc-bef-999        .
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-bef-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Filiale                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-bef-fil    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Conto corrente                      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-bef-ccc    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-350.
      *                  *---------------------------------------------*
      *                  * Inoltro pagamento al fornitore              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se si tratta di una cessione : nessuna  *
      *                      * visualizzazione                         *
      *                      *-----------------------------------------*
           if        rf-sfs-trs-scf       =    02
                     go to vis-dat-sfs-375.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Inoltro pagamento          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-inl-scf-lun    to   v-car                  .
           move      w-exp-inl-scf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-inl-scf-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-inl-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-375.
      *                  *---------------------------------------------*
      *                  * Data di scadenza                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data di scadenza           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfs-dts-scf       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Test se a vista                     *
      *                          *-------------------------------------*
           if        v-edt                =    spaces
                     move  "A vista "     to   v-edt                  .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-400.
      *                  *---------------------------------------------*
      *                  * Sigla valuta e cambio                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la valuta della scadenza e' pari a   *
      *                      * quella base :                           *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-sgl-vlt       =    c-sgl
                     go to vis-dat-sfs-425.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sigla valuta e cambio      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione sigla valuta            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-sgl-vlt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione coefficiente di cambio  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<GBD"               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           if        rf-sfs-sgl-vlt       =    spaces or
                     rf-sfs-sgl-vlt       =    c-sgl
                     move  zero           to   v-num
           else      move  rf-sfs-cdc-scf to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-425.
      *                  *---------------------------------------------*
      *                  * Importo scadenza in valuta                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la valuta della scadenza e' pari a   *
      *                      * quella base :                           *
      *                      * non si esegue nessuna visualizzazione   *
      *                      *-----------------------------------------*
           if        rf-sfs-sgl-vlt       =    c-sgl
                     go to vis-dat-sfs-450.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo in valuta          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-sfs-dec-vlt       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-iiv-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-450.
      *                  *---------------------------------------------*
      *                  * Importo scadenza in valuta base             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        rf-sfs-sgl-vlt       =    c-sgl
                     move  19             to   v-lin
           else      move  21             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo scadenza           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sigla valuta                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           if        rf-sfs-sgl-vlt       =    c-sgl
                     move  19             to   v-lin
           else      move  21             to   v-lin                  .
           move      24                   to   v-pos                  .
           move      rf-sfs-sgl-vlt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           if        rf-sfs-sgl-vlt       =    c-sgl
                     move  19             to   v-lin
           else      move  21             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sfs-imp-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-500.
      *                  *---------------------------------------------*
      *                  * Data documento di riferimento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "--Riferimenti documento---"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Data    :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-sfs-dat-ddr       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-sfs-525.
      *                  *---------------------------------------------*
      *                  * Numero documento di riferimento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Numero  :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-sfs-num-ddr       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-550.
      *                  *---------------------------------------------*
      *                  * Tipo documento di riferimento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Tipo    :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddr-lun    to   v-car                  .
           move      w-exp-tip-ddr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddr-tbl    to   v-txt                  .
           move      19                   to   v-lin                  .
           move      65                   to   v-pos                  .
           if        rf-sfs-tip-ddr       =    01
                     move  01             to   v-num
           else if   rf-sfs-tip-ddr       =    02
                     move  02             to   v-num
           else if   rf-sfs-tip-ddr       =    11
                     move  03             to   v-num
           else if   rf-sfs-tip-ddr       =    51
                     move  04             to   v-num
           else if   rf-sfs-tip-ddr       =    99
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-575.
      *                  *---------------------------------------------*
      *                  * Importo documento di riferimento            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Importo :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-sfs-dec-vlt       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-sfs-imp-ddr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-600.
      *                  *---------------------------------------------*
      *                  * Numero rata documento di riferimento        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Rata    :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      21                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-sfs-nra-ddr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-625.
      *                  *---------------------------------------------*
      *                  * Fine visualizzazione dati                   *
      *                  *---------------------------------------------*
           go to     vis-dat-sfs-900.
       vis-dat-sfs-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sfs-999.
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
      *              *-------------------------------------------------*
      *              * Numero scadenza                                 *
      *              *-------------------------------------------------*
           move      zero                 to   rr-num-scf             .
      *              *-------------------------------------------------*
      *              * Status visualizzazione dati scadenza            *
      *              *-------------------------------------------------*
           move      "N"                  to   rr-svd-scf             .
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
      *              * Lettura record scadenza da [sfs]                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSCF    "         to   f-key                  .
           move      rr-num-scf           to   rf-sfs-num-scf         .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to qry-str-ini-300
           else      go to qry-str-ini-200.
       qry-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se record scadenza non esistente                *
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
      *              * Se record scadenza esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per controllo ciclo    *
      *                  * interrogazione standard                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-int-sca-flg-uno      .
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
           move      "Numero scadenza non esistente in archivio scadenze
      -              " !             "    to   w-err-box-err-msg      .
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
           if        w-int-sca-flg-uno    =    spaces
                     move  "#"            to   w-int-sca-flg-uno
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
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa dati per storno                          *
      *              *-------------------------------------------------*
           perform   stp-dat-sto-000      thru stp-dat-sto-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-300.
      *              *-------------------------------------------------*
      *              * Stampa dati per pagamento                       *
      *              *-------------------------------------------------*
           perform   stp-dat-pgf-000      thru stp-dat-pgf-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-400.
      *              *-------------------------------------------------*
      *              * Stampa dati per addebito                        *
      *              *-------------------------------------------------*
           perform   stp-dat-adp-000      thru stp-dat-adp-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-500.
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
      *                  *---------------------------------------------*
      *                  * Editing numero scadenza                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      rf-sfs-num-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titolo con numero scadenza editato          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "   MOVIMENTI SU SCADENZA NR. "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into v-alf                  .
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
      *    * Stampa dati per registrazione                             *
      *    *-----------------------------------------------------------*
       stp-dat-rgs-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per registrazione              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         Registrazione scadenza         "
                                          to   w-stp-sfs-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-025.
      *              *-------------------------------------------------*
      *              * Data registrazione movimento di registrazione   *
      *              * scadenza                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data registrazione         :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-dtr-rgs       to   w-stp-sfs-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-050.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo di con-  *
      *              * tabilita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sfs-drc-rgs       to   w-stp-sfs-drc-ope      .
           move      rf-sfs-npc-rgs       to   w-stp-sfs-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-075.
      *              *-------------------------------------------------*
      *              * Tipo scadenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo scadenza              :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-tip-scf       to   w-stp-sfs-tds-pls      .
           perform   stp-tds-pls-000      thru stp-tds-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-100.
      *              *-------------------------------------------------*
      *              * Tipo registrazione scadenza                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' un paghero' : no stampa           *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 11
                     go to stp-dat-rgs-125.
      *                  *---------------------------------------------*
      *                  * Se non e' una cessione : no stampa          *
      *                  *---------------------------------------------*
           if        rf-sfs-trs-scf       not  = 02
                     go to stp-dat-rgs-125.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo registrazione scadenza:"
                                          to   w-stp-sfs-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-trs-scf-lun    to   v-car                  .
           move      w-exp-trs-scf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-trs-scf-tbl    to   v-txt                  .
           move      rf-sfs-trs-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-125.
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
       stp-dat-rgs-130.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice fornitore           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-cod-fnt       to   w-stp-sfs-n07-pls      .
           perform   stp-n07-pls-000      thru stp-n07-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-135.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-rgs-137.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica contabile del forni- *
      *                      * tore                                    *
      *                      *-----------------------------------------*
           move      rf-sfs-cod-fnt       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
       stp-dat-rgs-139.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-fnt-rag    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
       stp-dat-rgs-141.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-fnt-via    =    spaces
                     go to stp-dat-rgs-143.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-fnt-via    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
       stp-dat-rgs-143.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-fnt-loc    =    spaces
                     go to stp-dat-rgs-145.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-fnt-loc    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
       stp-dat-rgs-145.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-rgs-150.
       stp-dat-rgs-150.
      *              *-------------------------------------------------*
      *              * Codice dipendenza                               *
      *              *-------------------------------------------------*
       stp-dat-rgs-160.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Dipendenza del fornitore   :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-dpz-fnt       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-165.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-rgs-166.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale della    *
      *                      * dipendenza del fornitore                *
      *                      *-----------------------------------------*
           if        rf-sfs-dpz-fnt       =    spaces
                     move  "C"            to   w-let-arc-dcf-tle
           else      move  "D"            to   w-let-arc-dcf-tle      .
           move      rf-sfs-cod-fnt       to   w-let-arc-dcf-cod      .
           move      rf-sfs-dpz-fnt       to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
       stp-dat-rgs-167.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-dcf-rag    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-168.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-dcf-via    =    spaces
                     go to stp-dat-rgs-169.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-dcf-via    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-169.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-dcf-loc    =    spaces
                     go to stp-dat-rgs-170.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-dcf-loc    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-170.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-rgs-175.
       stp-dat-rgs-175.
      *              *-------------------------------------------------*
      *              * Inoltro pagamento al fornitore                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se scadenza ceduta a terzi : no stampa      *
      *                  *---------------------------------------------*
           if        rf-sfs-trs-scf       =    02
                     go to stp-dat-rgs-200.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Inoltro scadenza           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-inl-scf-lun    to   v-car                  .
           move      w-exp-inl-scf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-inl-scf-tbl    to   v-txt                  .
           move      rf-sfs-inl-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-200.
      *              *-------------------------------------------------*
      *              * Data di scadenza                                *
      *              *-------------------------------------------------*
       stp-dat-rgs-205.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore             *
      *                  *---------------------------------------------*
           if        rf-sfs-dts-scf       =    zero
                     go to stp-dat-rgs-210
           else      go to stp-dat-rgs-215.
       stp-dat-rgs-210.
      *                  *---------------------------------------------*
      *                  * Se valore a zero                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Data di scadenza           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      "A vista"            to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-rgs-225.
       stp-dat-rgs-215.
      *                  *---------------------------------------------*
      *                  * Se valore diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Data di scadenza           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-dts-scf       to   w-stp-sfs-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-rgs-225.
       stp-dat-rgs-225.
      *              *-------------------------------------------------*
      *              * Importo scadenza in valuta                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la scadenza e' nella valuta base : no    *
      *                  * visualizzazione                             *
      *                  *---------------------------------------------*
           if        rf-sfs-sgl-vlt       =    c-sgl
                     go to stp-dat-rgs-235.
      *                  *---------------------------------------------*
      *                  * Editing sigla valuta                        *
      *                  *---------------------------------------------*
           move      rf-sfs-sgl-vlt       to   w-stp-sfs-edt-001      .
           if        w-stp-sfs-edt-001    =    spaces
                     move  "???"          to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing importo in valuta                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-sfs-dec-vlt       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      rf-sfs-iiv-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Editing coefficiente di cambio              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<GDB"               to   v-edm                  .
           if        rf-sfs-sgl-vlt       =    spaces or
                     rf-sfs-sgl-vlt       =    c-sgl
                     move  zero           to   v-num
           else      move  rf-sfs-cdc-scf to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   w-stp-sfs-edt-003      .
           if        v-edt                not  = spaces
                     string "(Cambio: "
                                delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-stp-sfs-edt-003      .
      *                  *---------------------------------------------*
      *                  * Composizione stringa unica composta da:     *
      *                  *  - Sigla valuta                             *
      *                  *  - Importo in valuta                        *
      *                  *  - Coefficiente di cambio, tra parentesi    *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   spaces
                     "   "
                                delimited by   size
                     w-stp-sfs-edt-003
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo scadenza in valuta :"
                                          to   w-stp-sfs-pmt-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-235.
      *              *-------------------------------------------------*
      *              * Importo scadenza                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo scadenza           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-imp-scf       to   w-stp-sfs-v11-pls      .
           move      c-dec                to   w-stp-sfs-v11-dec      .
           perform   stp-v11-pls-000      thru stp-v11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-250.
      *              *-------------------------------------------------*
      *              * Riferimenti al documento                        *
      *              *-------------------------------------------------*
       stp-dat-rgs-255.
      *                  *---------------------------------------------*
      *                  * Se data documento di riferimento a zero :   *
      *                  * no stampa                                   *
      *                  *---------------------------------------------*
           if        rf-sfs-dat-ddr       =    zero
                     go to stp-dat-rgs-275.
       stp-dat-rgs-260.
      *                  *---------------------------------------------*
      *                  * Data documento di riferimento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Data documento di rifer.   :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-dat-ddr       to   w-stp-sfs-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-262.
      *                  *---------------------------------------------*
      *                  * Numero documento di riferimento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Numero documento di rif.   :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-num-ddr       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-264.
      *                  *---------------------------------------------*
      *                  * Tipo documento di riferimento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Tipo documento di rifer.   :"
                                          to   w-stp-sfs-pmt-pls      .


           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddr-lun    to   v-car                  .
           move      w-exp-tip-ddr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddr-tbl    to   v-txt                  .
           if        rf-sfs-tip-ddr       =    01
                     move  01             to   v-num
           else if   rf-sfs-tip-ddr       =    02
                     move  02             to   v-num
           else if   rf-sfs-tip-ddr       =    11
                     move  03             to   v-num
           else if   rf-sfs-tip-ddr       =    51
                     move  04             to   v-num
           else if   rf-sfs-tip-ddr       =    99
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-266.
      *                  *---------------------------------------------*
      *                  * Importo documento di riferimento            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo docum. di rifer.   :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-imp-ddr       to   w-stp-sfs-v11-pls      .
           move      rf-sfs-dec-vlt       to   w-stp-sfs-v11-dec      .
           perform   stp-v11-pls-000      thru stp-v11-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-268.
      *                  *---------------------------------------------*
      *                  * Numero rata documento di riferimento        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Nr. rata doc. di rifer.    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-nra-ddr       to   w-stp-sfs-n02-pls      .
           perform   stp-n02-pls-000      thru stp-n02-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-275.
      *              *-------------------------------------------------*
      *              * Codice ABI per banca fornitore per pagamento a  *
      *              * mezzo bonifico non in valuta                    *
      *              *-------------------------------------------------*
       stp-dat-rgs-280.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un bonifico bancario in *
      *                  * valuta base : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to stp-dat-rgs-300.
           if        rf-sfs-tvl-scf       =    02
                     go to stp-dat-rgs-300.
       stp-dat-rgs-285.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice ABI banca fornitore :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-abi-fnt       to   w-stp-sfs-z05-pls      .
           perform   stp-z05-pls-000      thru stp-z05-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-290.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica istituto di credito  *
      *                      *-----------------------------------------*
           move      rf-sfs-abi-fnt       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-axi-den    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-300.
      *              *-------------------------------------------------*
      *              * Codice ABI per banca fornitore per pagamento a  *
      *              * mezzo bonifico non in valuta                    *
      *              *-------------------------------------------------*
       stp-dat-rgs-302.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un bonifico bancario in *
      *                  * valuta base : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to stp-dat-rgs-310.
           if        rf-sfs-tvl-scf       =    02
                     go to stp-dat-rgs-310.
       stp-dat-rgs-304.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice CAB banca fornitore :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-cab-fnt       to   w-stp-sfs-z05-pls      .
           perform   stp-z05-pls-000      thru stp-z05-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-306.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica sportelli istituto   *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sfs-abi-fnt       to   w-let-arc-axs-abi      .
           move      rf-sfs-cab-fnt       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-axs-den    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-310.
      *              *-------------------------------------------------*
      *              * Codice banca estera del fornitore per pagamento *
      *              * a mezzo bonifico in valuta                      *
      *              *-------------------------------------------------*
       stp-dat-rgs-311.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un bonifico bancario su *
      *                  * estero : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to stp-dat-rgs-325.
           if        rf-sfs-tvl-scf       not  = 02
                     go to stp-dat-rgs-325.
       stp-dat-rgs-312.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Banca d'appoggio estera    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-cod-bef       to   w-stp-sfs-n05-pls      .
           perform   stp-n05-pls-000      thru stp-n05-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-313.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-rgs-314.
      *                      *-----------------------------------------*
      *                      * Lettura archivio banche estere per i    *
      *                      * fornitori [bef], per il codice banca    *
      *                      * estera del fornitore                    *
      *                      *-----------------------------------------*
           move      rf-sfs-cod-fnt       to   w-let-arc-bef-fnt      .
           move      rf-sfs-dpz-fnt       to   w-let-arc-bef-dpz      .
           move      rf-sfs-cod-bef       to   w-let-arc-bef-cod      .
           perform   let-arc-bef-000      thru let-arc-bef-999        .
       stp-dat-rgs-315.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-bef-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-316.
      *                      *-----------------------------------------*
      *                      * Filiale                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-bef-fil    =    spaces
                     go to stp-dat-rgs-317.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-bef-fil    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-317.
      *                      *-----------------------------------------*
      *                      * Conto corrente                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-bef-ccc    =    spaces
                     go to stp-dat-rgs-318.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-bef-ccc    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-318.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-rgs-325.
       stp-dat-rgs-325.
      *              *-------------------------------------------------*
      *              * Codice c/c bancario o postale per il fornitore  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un bonifico bancario in *
      *                  * valuta base, ne' di un bonifico postale :   *
      *                  * no stampa                                   *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 07 and
                     rf-sfs-tip-scf       not  = 08
                     go to stp-dat-rgs-350.
           if        rf-sfs-tvl-scf       =    02
                     go to stp-dat-rgs-350.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       =    07
                     move  "Conto corrente fornitore   :"
                                          to   w-stp-sfs-pmt-pls
           else      move  "C/C postale del fornitore  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-ccc-fnt       to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-350.
      *              *-------------------------------------------------*
      *              * Codice nostra banca di appoggio per pagamenti   *
      *              * di tipo :                                       *
      *              *  - Incasso elettronico                          *
      *              *  - Ri.Ba.                                       *
      *              *  - C.d.O.                                       *
      *              *  - M.Av.                                        *
      *              *  - R.I.D.                                       *
      *              *  - Ricevuta Bancaria                            *
      *              *  - Tratta                                       *
      *              *                                                 *
      *              * Oppure nostra banca di domiciliazione per paga- *
      *              * menti di tipo :                                 *
      *              *  - Paghero' cambiario                           *
      *              *-------------------------------------------------*
       stp-dat-rgs-355.
      *                  *---------------------------------------------*
      *                  * Test su tipo scadenza                       *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 02 and
                     rf-sfs-tip-scf       not  = 03 and
                     rf-sfs-tip-scf       not  = 04 and
                     rf-sfs-tip-scf       not  = 05 and
                     rf-sfs-tip-scf       not  = 06 and
                     rf-sfs-tip-scf       not  = 09 and
                     rf-sfs-tip-scf       not  = 10 and
                     rf-sfs-tip-scf       not  = 11
                     go to stp-dat-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se si tratta di un paghero' cambiario, ma   *
      *                  * ceduto : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       =    11 and
                     rf-sfs-trs-scf       =    02
                     go to stp-dat-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se si tratta di un paghero' cambiario, ma   *
      *                  * manca la ns. banca di domiciliazione : no   *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       =    11 and
                     rf-sfs-cbp-nsb       =    spaces
                     go to stp-dat-rgs-400.
       stp-dat-rgs-360.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           if        rf-sfs-tip-scf       =    11
                     move  "Nostra banca domiciliazione:"
                                          to   w-stp-sfs-pmt-pls
           else      move  "Nostra banca d'appoggio    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-cbp-nsb       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-365.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica nostre banche        *
      *                      *-----------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-sfs-cbp-nsb       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-400.
      *              *-------------------------------------------------*
      *              * Obbligato principale                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' un paghero' : no      *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 11
                     go to stp-dat-rgs-425.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' stata ceduta al for-  *
      *                  * nitore : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sfs-trs-scf       not  = 02
                     go to stp-dat-rgs-425.
       stp-dat-rgs-405.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica obbligato principale     *
      *                  *---------------------------------------------*
           move      51                   to   w-let-arc-obp-tip      .
           move      rf-sfs-num-scf       to   w-let-arc-obp-num      .
           perform   let-arc-obp-000      thru let-arc-obp-999        .
       stp-dat-rgs-410.
      *                  *---------------------------------------------*
      *                  * Stampa anagrafica                           *
      *                  *---------------------------------------------*
       stp-dat-rgs-412.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Obbligato principale       :"
                                          to   w-stp-sfs-pmt-pls      .
           move      w-let-arc-obp-rag    to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-414.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se indirizzo a spaces : oltre       *
      *                          *-------------------------------------*
           if        w-let-arc-obp-via    =    spaces
                     go to stp-dat-rgs-416.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           move      w-let-arc-obp-via    to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-416.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se localita' a spaces : oltre       *
      *                          *-------------------------------------*
           if        w-let-arc-obp-loc    =    spaces
                     go to stp-dat-rgs-418.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           move      w-let-arc-obp-loc    to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-418.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-rgs-425.
       stp-dat-rgs-425.
      *              *-------------------------------------------------*
      *              * Si/No pagamento bloccato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Pagamento scadenza bloccato:"
                                          to   w-stp-sfs-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-pbl-lun    to   v-car                  .
           move      w-exp-snx-pbl-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-pbl-tbl    to   v-txt                  .
           if        rf-sfs-snx-pbl       =    "N"
                     move  01             to   v-num
           else if   rf-sfs-snx-pbl       =    "S"
                     move  02             to   v-num
           else      move  01             to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-430.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per emissione                  *
      *              *-------------------------------------------------*
           go to     stp-dat-rgs-999.
       stp-dat-rgs-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per storno                                    *
      *    *-----------------------------------------------------------*
       stp-dat-sto-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sfs-dtr-sto       =    zero
                     go to stp-dat-sto-999.
       stp-dat-sto-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per storno                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "                 Storno                 "
                                          to   w-stp-sfs-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-050.
      *              *-------------------------------------------------*
      *              * Data registrazione movimento di storno          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data storno                :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-dtr-sto       to   w-stp-sfs-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo di con-  *
      *              * tabilita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sfs-drc-sto       to   w-stp-sfs-drc-ope      .
           move      rf-sfs-npc-sto       to   w-stp-sfs-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-100.
      *              *-------------------------------------------------*
      *              * Si/No emissione nuova scadenza a fronte dello   *
      *              * storno                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Nuova scadenza a fronte    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-six-nox-lun    to   v-car                  .
           move      w-exp-six-nox-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-six-nox-tbl    to   v-txt                  .
           move      rf-sfs-rns-sto       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-125.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte dello storno     *
      *              *-------------------------------------------------*
       stp-dat-sto-130.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sfs-rns-sto       not  = 01
                     go to stp-dat-sto-150.
       stp-dat-sto-135.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-nns-sto       to   w-stp-sfs-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-150.
      *              *-------------------------------------------------*
      *              * Tipo nuova scadenza a fronte dello storno       *
      *              *-------------------------------------------------*
       stp-dat-sto-155.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sfs-rns-sto       not  = 01
                     go to stp-dat-sto-175.
       stp-dat-sto-160.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo nuova scadenza        :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-tns-sto       to   w-stp-sfs-tds-pls      .
           perform   stp-tds-pls-000      thru stp-tds-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-175.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per storno                     *
      *              *-------------------------------------------------*
           go to     stp-dat-sto-999.
       stp-dat-sto-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per pagamento                                 *
      *    *-----------------------------------------------------------*
       stp-dat-pgf-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sfs-num-pgf       =    zero
                     go to stp-dat-pgf-999.
       stp-dat-pgf-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per pagamento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "               Pagamento                "
                                          to   w-stp-sfs-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-050.
      *              *-------------------------------------------------*
      *              * Lettura record pagamento da [sfp]               *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPGF    "         to   f-key                  .
           move      rf-sfs-num-pgf       to   rf-sfp-num-pgf         .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * Se pagamento non esistente : si pone a zero il  *
      *              * numero pagamento nel record, su cui in seguito  *
      *              * sara' eseguito il test di esistenza             *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-sfp-num-pgf         .
       stp-dat-pgf-075.
      *              *-------------------------------------------------*
      *              * Numero pagamento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing numero pagamento                    *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      rf-sfs-num-pgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing segnale di record non esistente     *
      *                  *---------------------------------------------*
           if        rf-sfp-num-pgf       =    zero
                     move  "NON PIU' ESISTENTE IN ARCHIVIO     "
                                          to   w-stp-sfs-edt-002
           else      move  spaces         to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero pagamento           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     "     "    delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-100.
      *              *-------------------------------------------------*
      *              * Se record pagamento in [sfp] non esistente : u- *
      *              * scita senza alcuna ulteriore azione             *
      *              *-------------------------------------------------*
           if        rf-sfp-num-pgf       =    zero
                     go to stp-dat-pgf-999.
       stp-dat-pgf-200.
      *              *-------------------------------------------------*
      *              * Data registrazione movimento di pagamento, in-  *
      *              * sieme a:                                        *
      *              *  - Data di sistema ultimo inserimento/modifica  *
      *              *    relativa al pagamento                        *
      *              *  - Utente                                       *
      *              *  - Fase                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfp-dtr-pgf       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing data di sistema ultimo inserimento  *
      *                  * o modifica per il pagamento                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfp-ide-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data registrazione         :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     "  "       delimited by   size
                     "["        delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   spaces
                     "] ["       delimited by   size
                     rf-sfp-ide-ute
                                delimited by   spaces
                     "] ["       delimited by   size
                     rf-sfp-ide-fas
                                delimited by   spaces
                     "]"        delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-225.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo di con-  *
      *              * tabilita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sfp-drc-pgf       to   w-stp-sfs-drc-ope      .
           move      rf-sfp-npc-pgf       to   w-stp-sfs-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-250.
      *              *-------------------------------------------------*
      *              * Modalita' di pagamento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing modalita' di pagamento              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mod-pgf-lun    to   v-car                  .
           move      w-exp-mod-pgf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-mod-pgf-tbl    to   v-txt                  .
           if        rf-sfp-mod-pgf       =    01
                     move  01             to   v-num
           else if   rf-sfp-mod-pgf       =    02
                     move  02             to   v-num
           else if   rf-sfp-mod-pgf       =    03
                     move  03             to   v-num
           else if   rf-sfp-mod-pgf       =    04
                     move  04             to   v-num
           else if   rf-sfp-mod-pgf       =    13
                     move  05             to   v-num
           else if   rf-sfp-mod-pgf       =    14
                     move  06             to   v-num
           else if   rf-sfp-mod-pgf       =    15
                     move  07             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Modalita' di pagamento     :"
                                          to   w-stp-sfs-pmt-pls      .
           move      v-edt                to   w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-275.
      *              *-------------------------------------------------*
      *              * Nostra cassa, o nostra banca, o nostro conto    *
      *              * corrente postale, a seconda della modalita' di  *
      *              * pagamento                                       *
      *              *-------------------------------------------------*
       stp-dat-pgf-277.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della modalita' di pa- *
      *                  * gamento                                     *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       =    01
                     go to stp-dat-pgf-280
           else if   rf-sfp-mod-pgf       =    02 or
                     rf-sfp-mod-pgf       =    03 or
                     rf-sfp-mod-pgf       =    13 or
                     rf-sfp-mod-pgf       =    14 or
                     rf-sfp-mod-pgf       =    15
                     go to stp-dat-pgf-285
           else if   rf-sfp-mod-pgf       =    04 or
                     rf-sfp-mod-pgf       =    14
                     go to stp-dat-pgf-290
           else      go to stp-dat-pgf-300.
       stp-dat-pgf-280.
      *                  *---------------------------------------------*
      *                  * Nostra cassa                                *
      *                  *---------------------------------------------*
       stp-dat-pgf-281.
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Codice nostra cassa        :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-cbp-pgf       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-282.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica nostre casse     *
      *                          *-------------------------------------*
           move      01                   to   w-let-arc-cbp-tip      .
           move      rf-sfp-cbp-pgf       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-283.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-pgf-300.
       stp-dat-pgf-285.
      *                  *---------------------------------------------*
      *                  * Nostra banca                                *
      *                  *---------------------------------------------*
       stp-dat-pgf-286.
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Codice nostra banca        :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-cbp-pgf       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-287.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica nostre banche    *
      *                          *-------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-sfp-cbp-pgf       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-288.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-pgf-300.
       stp-dat-pgf-290.
      *                  *---------------------------------------------*
      *                  * Nostro c/c postale                          *
      *                  *---------------------------------------------*
       stp-dat-pgf-291.
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Codice nostro C/C postale  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-cbp-pgf       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-292.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica nostri c/c po-   *
      *                          * stali                               *
      *                          *-------------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      rf-sfp-cbp-pgf       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-293.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-pgf-300.
       stp-dat-pgf-300.
      *              *-------------------------------------------------*
      *              * Codice ABI per banca fornitore                  *
      *              *-------------------------------------------------*
       stp-dat-pgf-305.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un ordine di bonifico   *
      *                  * bancario in valuta base : no stampa         *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 13
                     go to stp-dat-pgf-325.
           if        rf-sfp-tvl-pgf       =    02
                     go to stp-dat-pgf-325.
       stp-dat-pgf-310.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice ABI banca fornitore :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-abi-pgf       to   w-stp-sfs-z05-pls      .
           perform   stp-z05-pls-000      thru stp-z05-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-315.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica istituto di credito  *
      *                      *-----------------------------------------*
           move      rf-sfp-abi-pgf       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-axi-den    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-325.
      *              *-------------------------------------------------*
      *              * Codice ABI per banca fornitore                  *
      *              *-------------------------------------------------*
       stp-dat-pgf-327.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un ordine di bonifico   *
      *                  * bancario in valuta base : no stampa         *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 13
                     go to stp-dat-pgf-335.
           if        rf-sfp-tvl-pgf       =    02
                     go to stp-dat-pgf-335.
       stp-dat-pgf-329.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice CAB banca fornitore :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-cab-pgf       to   w-stp-sfs-z05-pls      .
           perform   stp-z05-pls-000      thru stp-z05-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-331.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica sportelli istituto   *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sfp-abi-pgf       to   w-let-arc-axs-abi      .
           move      rf-sfp-cab-pgf       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-axs-den    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-335.
      *              *-------------------------------------------------*
      *              * Codice banca estera del fornitore               *
      *              *-------------------------------------------------*
       stp-dat-pgf-336.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un ordine di bonifico   *
      *                  * bancario in valuta : no stampa              *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 13
                     go to stp-dat-pgf-350.
           if        rf-sfp-tvl-pgf       not  = 02
                     go to stp-dat-pgf-350.
       stp-dat-pgf-337.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Banca d'appoggio estera    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-bef-pgf       to   w-stp-sfs-n05-pls      .
           perform   stp-n05-pls-000      thru stp-n05-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-338.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-pgf-339.
      *                      *-----------------------------------------*
      *                      * Lettura archivio banche estere per i    *
      *                      * fornitori [bef], per il codice banca    *
      *                      * estera del fornitore                    *
      *                      *-----------------------------------------*
           move      rf-sfp-cod-fnt       to   w-let-arc-bef-fnt      .
           move      rf-sfp-dpz-fnt       to   w-let-arc-bef-dpz      .
           move      rf-sfp-bef-pgf       to   w-let-arc-bef-cod      .
           perform   let-arc-bef-000      thru let-arc-bef-999        .
       stp-dat-pgf-340.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-bef-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-341.
      *                      *-----------------------------------------*
      *                      * Filiale                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-bef-fil    =    spaces
                     go to stp-dat-pgf-342.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-bef-fil    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-342.
      *                      *-----------------------------------------*
      *                      * Conto corrente                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-bef-ccc    =    spaces
                     go to stp-dat-pgf-343.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-bef-ccc    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-343.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-pgf-350.
       stp-dat-pgf-350.
      *              *-------------------------------------------------*
      *              * Codice c/c bancario o postale per il fornitore  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un ordine di bonifico   *
      *                  * bancario in valuta base, ne' di un bollet-  *
      *                  * tino in c/c postale : no stampa             *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 13 and
                     rf-sfp-mod-pgf       not  = 14
                     go to stp-dat-pgf-375.
           if        rf-sfp-tvl-pgf       =    02
                     go to stp-dat-pgf-375.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       =    13
                     move  "Conto corrente fornitore   :"
                                          to   w-stp-sfs-pmt-pls
           else      move  "C/C postale del fornitore  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-ccc-pgf       to   w-stp-sfs-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-375.
      *              *-------------------------------------------------*
      *              * Determinazione del numero di scadenze coinvolte *
      *              * nell'operazione di pagamento, per uso successi- *
      *              * vo, in 'w-det-sfp-nrs-iop'                      *
      *              *-------------------------------------------------*
           move      rf-sfp-num-pgf       to   w-det-sfp-num-pgf      .
           perform   det-sfp-nrs-000      thru det-sfp-nrs-999        .
       stp-dat-pgf-400.
      *              *-------------------------------------------------*
      *              * Importo pagamento, sempre in valuta base        *
      *              *-------------------------------------------------*
       stp-dat-pgf-402.
      *                  *---------------------------------------------*
      *                  * Editing sigla valuta                        *
      *                  *---------------------------------------------*
           move      c-sgl                to   w-stp-sfs-edt-ax3      .
      *                  *---------------------------------------------*
      *                  * Editing importo, in valuta base             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      rf-sfp-imp-pgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing literal per piu' scadenze coinvolte *
      *                  * nell'operazione di pagamento                *
      *                  *---------------------------------------------*
           if        w-det-sfp-nrs-iop    >    1
                     move  " (N.B.: Include anche altre scadenze)"
                                          to   w-stp-sfs-edt-002
           else      move  spaces         to   w-stp-sfs-edt-002      .
       stp-dat-pgf-404.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           string    "Importo pagamento      "
                                delimited by   size
                     w-stp-sfs-edt-ax3
                                delimited by   size
                     " :"       delimited by   size
                                          into w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     w-stp-sfs-edt-002
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
       stp-dat-pgf-406.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-425.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate dalla banca o dalla    *
      *              * Posta                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su modalita' di pagamento              *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 03 and
                     rf-sfp-mod-pgf       not  = 04
                     go to stp-dat-pgf-450.
      *                  *---------------------------------------------*
      *                  * Test su importo spese                       *
      *                  *---------------------------------------------*
           if        rf-sfp-spe-pgf       =    zero
                     go to stp-dat-pgf-450.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-spe-pgf       to   w-stp-sfs-v11-pls      .
           move      c-dec                to   w-stp-sfs-v11-dec      .
           perform   stp-v11-pls-000      thru stp-v11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-450.
      *              *-------------------------------------------------*
      *              * Data e numero documento                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su modalita' di pagamento              *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 01 and
                     rf-sfp-mod-pgf       not  = 02 and
                     rf-sfp-mod-pgf       not  = 03 and
                     rf-sfp-mod-pgf       not  = 04
                     go to stp-dat-pgf-475.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-sfp-ddo-pgf       =    zero
                     go to stp-dat-pgf-475.
      *                  *---------------------------------------------*
      *                  * Editing data documento                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfp-ddo-pgf       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing numero documento                    *
      *                  *---------------------------------------------*
           move      rf-sfp-ndo-pgf       to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data e numero documento    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     "  "       delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-475.
      *              *-------------------------------------------------*
      *              * Data valuta fissa per il beneficiario           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su modalita' di pagamento              *
      *                  *---------------------------------------------*
           if        rf-sfp-mod-pgf       not  = 13
                     go to stp-dat-pgf-500.
      *                  *---------------------------------------------*
      *                  * Test su data valuta fissa                   *
      *                  *---------------------------------------------*
           if        rf-sfp-dvf-bfc       =    zero
                     go to stp-dat-pgf-500.
      *                  *---------------------------------------------*
      *                  * Editing data valuta fissa                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfp-dvf-bfc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data valuta fissa          :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-500.
      *              *-------------------------------------------------*
      *              * Segnale di acconto/saldo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "In conto o a saldo         :"
                                          to   w-stp-sfs-pmt-pls      .
           if        rf-sfp-aos-pgf       =    02
                     move  "Pagamento in conto"
                                          to   w-stp-sfs-a50-pls
           else      move  "Pagamento a saldo "
                                          to   w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-600.
      *              *-------------------------------------------------*
      *              * Si/No emissione nuova scadenza a fronte del re- *
      *              * siduo da pagare                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il pagamento non e' In conto : no stampa *
      *                  *---------------------------------------------*
           if        rf-sfp-aos-pgf       not  = 02
                     go to stp-dat-pgf-625.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Nuova scadenza per residuo :"
                                          to   w-stp-sfs-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-six-nox-lun    to   v-car                  .
           move      w-exp-six-nox-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-six-nox-tbl    to   v-txt                  .
           move      rf-sfp-rns-pgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-625.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte del residuo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il pagamento non e' In conto : no stampa *
      *                  *---------------------------------------------*
           if        rf-sfp-aos-pgf       not  = 02
                     go to stp-dat-pgf-650.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte del residuo : no stampa              *
      *                  *---------------------------------------------*
           if        rf-sfp-rns-pgf       not  = 01
                     go to stp-dat-pgf-650.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-nns-pgf       to   w-stp-sfs-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-650.
      *              *-------------------------------------------------*
      *              * Tipo nuova scadenza a fronte del residuo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il pagamento non e' In conto : no stampa *
      *                  *---------------------------------------------*
           if        rf-sfp-aos-pgf       not  = 02
                     go to stp-dat-pgf-675.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte del residuo : no stampa              *
      *                  *---------------------------------------------*
           if        rf-sfp-rns-pgf       not  = 01
                     go to stp-dat-pgf-675.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo nuova scadenza        :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfp-tns-pgf       to   w-stp-sfs-tds-pls      .
           perform   stp-tds-pls-000      thru stp-tds-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pgf-999.
       stp-dat-pgf-675.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per pagamento                  *
      *              *-------------------------------------------------*
           go to     stp-dat-pgf-999.
       stp-dat-pgf-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per addebito                                  *
      *    *-----------------------------------------------------------*
       stp-dat-adp-000.
      *              *-------------------------------------------------*
      *              * Se operazione di pagamento non presente nel     *
      *              * record della scadenza : uscita                  *
      *              *-------------------------------------------------*
           if        rf-sfs-num-pgf       =    zero
                     go to stp-dat-adp-999.
      *              *-------------------------------------------------*
      *              * Se record di pagamento non esistente : uscita   *
      *              *-------------------------------------------------*
           if        rf-sfp-num-pgf       =    zero
                     go to stp-dat-adp-999.
      *              *-------------------------------------------------*
      *              * Se operazione di addebito non presente nel      *
      *              * record del pagamento : uscita                   *
      *              *-------------------------------------------------*
           if        rf-sfp-num-adp       =    zero
                     go to stp-dat-adp-999.
       stp-dat-adp-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per addebito                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "                Addebito                "
                                          to   w-stp-sfs-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-050.
      *              *-------------------------------------------------*
      *              * Lettura record addebito da [sfa]                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMADP    "         to   f-key                  .
           move      rf-sfp-num-adp       to   rf-sfa-num-adp         .
           move      "pgm/scf/fls/ioc/obj/iofsfa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfa                 .
      *              *-------------------------------------------------*
      *              * Se addebito non esistente : si pone a zero il   *
      *              * numero addebito nel record, su cui in seguito   *
      *              * sara' eseguito il test di esistenza             *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-sfa-num-adp         .
       stp-dat-adp-075.
      *              *-------------------------------------------------*
      *              * Numero addebito                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing numero addebito                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      rf-sfp-num-adp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing segnale di record non esistente     *
      *                  *---------------------------------------------*
           if        rf-sfa-num-adp       =    zero
                     move  "NON PIU' ESISTENTE IN ARCHIVIO     "
                                          to   w-stp-sfs-edt-002
           else      move  spaces         to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero addebito            :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     "     "    delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-100.
      *              *-------------------------------------------------*
      *              * Se record addebito in [sfa] non esistente : u-  *
      *              * scita senza alcuna ulteriore azione             *
      *              *-------------------------------------------------*
           if        rf-sfa-num-adp       =    zero
                     go to stp-dat-adp-999.
       stp-dat-adp-200.
      *              *-------------------------------------------------*
      *              * Data registrazione movimento di addebito, in-   *
      *              * sieme a:                                        *
      *              *  - Data di sistema ultimo inserimento/modifica  *
      *              *    relativa all'addebito                        *
      *              *  - Utente                                       *
      *              *  - Fase                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfa-dtr-adp       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing data di sistema ultimo inserimento  *
      *                  * o modifica per l'addebito                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfa-ide-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data registrazione         :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     "  "       delimited by   size
                     "["        delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   spaces
                     "] ["       delimited by   size
                     rf-sfa-ide-ute
                                delimited by   spaces
                     "] ["       delimited by   size
                     rf-sfa-ide-fas
                                delimited by   spaces
                     "]"        delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-225.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo di con-  *
      *              * tabilita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sfa-drc-adp       to   w-stp-sfs-drc-ope      .
           move      rf-sfa-npc-adp       to   w-stp-sfs-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-275.
      *              *-------------------------------------------------*
      *              * Nostra banca, o nostro conto corrente postale,  *
      *              * a seconda della modalita' di addebito           *
      *              *-------------------------------------------------*
       stp-dat-adp-277.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della modalita' di ad- *
      *                  * debito                                      *
      *                  *---------------------------------------------*
           if        rf-sfa-mod-adp       =    13 or
                     rf-sfa-mod-adp       =    15
                     go to stp-dat-adp-285
           else if   rf-sfa-mod-adp       =    14
                     go to stp-dat-adp-290
           else      go to stp-dat-adp-350.
       stp-dat-adp-285.
      *                  *---------------------------------------------*
      *                  * Nostra banca                                *
      *                  *---------------------------------------------*
       stp-dat-adp-286.
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Codice nostra banca        :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfa-cbp-adp       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-287.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica nostre banche    *
      *                          *-------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-sfa-cbp-adp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-288.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-adp-350.
       stp-dat-adp-290.
      *                  *---------------------------------------------*
      *                  * Nostro c/c postale                          *
      *                  *---------------------------------------------*
       stp-dat-adp-291.
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Codice nostro C/C postale  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfa-cbp-adp       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-292.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica nostri c/c po-   *
      *                          * stali                               *
      *                          *-------------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      rf-sfa-cbp-adp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sfs-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-293.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-adp-350.
       stp-dat-adp-350.
      *              *-------------------------------------------------*
      *              * Importo addebito  in valuta                     *
      *              *-------------------------------------------------*
       stp-dat-adp-352.
      *                  *---------------------------------------------*
      *                  * Se sigla valuta pari alla valuta base : no  *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-sfa-sgl-vlt       =    spaces or
                     rf-sfa-sgl-vlt       =    c-sgl
                     go to stp-dat-adp-400.
       stp-dat-adp-354.
      *                  *---------------------------------------------*
      *                  * Editing sigla valuta                        *
      *                  *---------------------------------------------*
           move      rf-sfa-sgl-vlt       to   w-stp-sfs-edt-ax3      .
      *                  *---------------------------------------------*
      *                  * Editing importo addebito in valuta          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           if        rf-sfa-sgl-vlt       =    spaces or
                     rf-sfa-sgl-vlt       =    c-sgl
                     move  c-dec          to   v-dec
           else      move  rf-sfa-dec-vlt to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      rf-sfa-iiv-adp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing coefficiente di cambio              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<GDB"               to   v-edm                  .
           if        rf-sfa-sgl-vlt       =    spaces or
                     rf-sfa-sgl-vlt       =    c-sgl
                     move  zero           to   v-num
           else      move  rf-sfa-cdc-adp to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   w-stp-sfs-edt-002      .
           if        v-edt                not  = spaces
                     string "(Cambio: "
                                delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-stp-sfs-edt-002      .
       stp-dat-adp-356.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-sfs-pmt-pls      .
           string    "Importo in valuta      "
                                delimited by   size
                     w-stp-sfs-edt-ax3
                                delimited by   size
                     " :"       delimited by   size
                                          into w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     w-stp-sfs-edt-002
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
       stp-dat-adp-358.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-400.
      *              *-------------------------------------------------*
      *              * Importo addebito                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo addebito           :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfa-imp-adp       to   w-stp-sfs-v11-pls      .
           move      c-dec                to   w-stp-sfs-v11-dec      .
           perform   stp-v11-pls-000      thru stp-v11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-425.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate dalla banca o dalla    *
      *              * Posta                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su modalita' di addebito               *
      *                  *---------------------------------------------*
           if        rf-sfa-mod-adp       not  = 13 and
                     rf-sfa-mod-adp       not  = 14 and
                     rf-sfa-mod-adp       not  = 15
                     go to stp-dat-adp-450.
      *                  *---------------------------------------------*
      *                  * Test su importo spese                       *
      *                  *---------------------------------------------*
           if        rf-sfa-spe-adp       =    zero
                     go to stp-dat-adp-450.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfa-spe-adp       to   w-stp-sfs-v11-pls      .
           move      c-dec                to   w-stp-sfs-v11-dec      .
           perform   stp-v11-pls-000      thru stp-v11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-450.
      *              *-------------------------------------------------*
      *              * Data e numero documento                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-sfa-ddo-adp       =    zero
                     go to stp-dat-adp-475.
      *                  *---------------------------------------------*
      *                  * Editing data documento                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sfa-ddo-adp       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *                  *---------------------------------------------*
      *                  * Editing numero documento                    *
      *                  *---------------------------------------------*
           move      rf-sfa-ndo-adp       to   w-stp-sfs-edt-002      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data e numero documento    :"
                                          to   w-stp-sfs-pmt-pls      .
           move      spaces               to   w-stp-sfs-a50-pls      .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     "  "       delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   size
                                          into w-stp-sfs-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-adp-999.
       stp-dat-adp-475.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per addebito                   *
      *              *-------------------------------------------------*
           go to     stp-dat-adp-999.
       stp-dat-adp-999.
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
                                          to   w-stp-sfs-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-600.
      *              *-------------------------------------------------*
      *              * Flags di elaborazione bloccanti                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-sfs-flg-blo       =    spaces
                     go to stp-dat-ain-625.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags bloccanti            :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-flg-blo       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-625.
      *              *-------------------------------------------------*
      *              * Flags di elaborazione non bloccanti             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-sfs-flg-nbl       =    spaces
                     go to stp-dat-ain-650.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags non bloccanti        :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-flg-nbl       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-650.
      *              *-------------------------------------------------*
      *              * Flags di sottoponibilita' a pulizia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-sfs-flg-pul       =    spaces
                     go to stp-dat-ain-800.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags per pulizia          :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-flg-pul       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-800.
      *              *-------------------------------------------------*
      *              * Data ultimo aggiornamento                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data ultimo aggiornamento  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-ide-dat       to   w-stp-sfs-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-825.
      *              *-------------------------------------------------*
      *              * Utente ultimo aggiornamento                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "     da parte dell'utente  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-ide-ute       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-850.
      *              *-------------------------------------------------*
      *              * Programma ultimo aggiornamento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         con il programma  :"
                                          to   w-stp-sfs-pmt-pls      .
           move      rf-sfs-ide-fas       to   w-stp-sfs-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-900.
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
           move      w-stp-sfs-tst-ope    to   v-alf                  .
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa data                                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-dat-pls    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-dat-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un numero progressivo nel formato s.aa.nnnnnnnn *
      *    *-----------------------------------------------------------*
       stp-p08-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-p08-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa numero progressivo                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-p08-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-p08-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
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
           move      w-stp-sfs-p06-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-p06-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un tipo scadenza                                *
      *    *-----------------------------------------------------------*
       stp-tds-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tds-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa tipo di scadenza                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scf-lun    to   v-car                  .
           move      w-exp-tip-scf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-scf-tbl    to   v-txt                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-tds-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tds-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo alfanumerico di 40 caratteri           *
      *    *-----------------------------------------------------------*
       stp-a10-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-a10-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-a10-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a10-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo alfanumerico di 40 caratteri           *
      *    *-----------------------------------------------------------*
       stp-a40-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-a40-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-a40-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a40-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
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
           move      w-stp-sfs-a50-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a50-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa descrizione di 40 caratteri                        *
      *    *-----------------------------------------------------------*
       stp-d40-pls-000.
      *              *-------------------------------------------------*
      *              * Stampa descrizione                              *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-stp-sfs-d40-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-d40-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico di 1 carattere                *
      *    *-----------------------------------------------------------*
       stp-n01-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-n01-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-n01-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-n01-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico di 2 caratteri                *
      *    *-----------------------------------------------------------*
       stp-n02-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-n02-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-n02-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-n02-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico di 5 caratteri                *
      *    *-----------------------------------------------------------*
       stp-n05-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-n05-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-n05-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-n05-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico di 5 caratteri con zeri in    *
      *    * testa                                                     *
      *    *-----------------------------------------------------------*
       stp-z05-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-z05-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-z05-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-z05-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico di 7 caratteri                *
      *    *-----------------------------------------------------------*
       stp-n07-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-n07-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-n07-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-n07-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo 'valore' con segno e decimali          *
      *    *-----------------------------------------------------------*
       stp-v11-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-v11-pls-999.
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
           move      w-stp-sfs-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo numerico                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      w-stp-sfs-v11-dec    to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sfs-v11-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-v11-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa data registrazione e numero protocollo di contabi- *
      *    * lita' generale                                            *
      *    *-----------------------------------------------------------*
       stp-drc-npc-000.
      *              *-------------------------------------------------*
      *              * Se i valori sono entrambi a zero : uscita       *
      *              *-------------------------------------------------*
           if        w-stp-sfs-drc-ope    =    zero and
                     w-stp-sfs-npc-ope    =    zero
                     go to stp-drc-npc-999.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-drc-npc-999.
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
           move      "Protocollo di contabilita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Editing numero protocollo                       *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-stp-sfs-npc-ope    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-001      .
      *              *-------------------------------------------------*
      *              * Editing data di registrazione                   *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-stp-sfs-drc-ope    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sfs-edt-002      .
      *              *-------------------------------------------------*
      *              * Valore per numero protocollo e data di regi-    *
      *              * strazione stringati assieme                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    w-stp-sfs-edt-001
                                delimited by   spaces
                     " del "
                                delimited by   size
                     w-stp-sfs-edt-002
                                delimited by   spaces
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-drc-npc-999.
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
           move      rf-dcf-ccc-app       to   w-let-arc-dcf-ccc      .
           move      rf-dcf-ccp-app       to   w-let-arc-dcf-ccp      .
           if        w-let-arc-dcf-tle    =    "D"
                     move  spaces         to   w-let-arc-dcf-ban
           else      move  rf-dcf-nos-ban to   w-let-arc-dcf-ban
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
           move      zero                 to   w-let-arc-dcf-ccc      .
           move      spaces               to   w-let-arc-dcf-ccp      .
           move      spaces               to   w-let-arc-dcf-ban      .
           move      spaces               to   w-let-arc-dcf-vlt      .
       let-arc-dcf-999.
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
       let-arc-cbp-999.
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
      *    * Routine di lettura archivio [bef]                         *
      *    *-----------------------------------------------------------*
       let-arc-bef-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bef-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-bef-fnt    =    zero
                     go to let-arc-bef-500.
      *              *-------------------------------------------------*
      *              * Test se progressivo a zero                      *
      *              *-------------------------------------------------*
           if        w-let-arc-bef-cod    =    zero
                     go to let-arc-bef-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODBEF"             to   f-key                  .
           move      w-let-arc-bef-fnt    to   rf-bef-cod-dcf         .
           move      w-let-arc-bef-dpz    to   rf-bef-dpz-dcf         .
           move      w-let-arc-bef-cod    to   rf-bef-cod-bef         .
           move      "pgm/scf/fls/ioc/obj/iofbef"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-bef-400.
       let-arc-bef-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-bef-des-bef       to   w-let-arc-bef-des      .
           move      rf-bef-fil-bef       to   w-let-arc-bef-fil      .
           move      rf-bef-ccc-bef       to   w-let-arc-bef-ccc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-bef-999.
       let-arc-bef-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-bef-flg      .
           move      all   "."            to   w-let-arc-bef-des      .
           go to     let-arc-bef-600.
       let-arc-bef-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bef-des      .
       let-arc-bef-600.
           move      spaces               to   w-let-arc-bef-fil      .
           move      spaces               to   w-let-arc-bef-ccc      .
       let-arc-bef-999.
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
      *    * Routine di lettura archivio [obp]                         *
      *    *-----------------------------------------------------------*
       let-arc-obp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-obp-flg      .
      *              *-------------------------------------------------*
      *              * Test se tipo record a zero                      *
      *              *-------------------------------------------------*
           if        w-let-arc-obp-tip    =    zero
                     go to let-arc-obp-500.
      *              *-------------------------------------------------*
      *              * Test se numero scadenza a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-obp-num    =    zero
                     go to let-arc-obp-500.
      *              *-------------------------------------------------*
      *              * Lettura per numero scadenza                     *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMOBP    "         to   f-key                  .
           move      w-let-arc-obp-tip    to   rf-obp-tip-obp         .
           move      w-let-arc-obp-num    to   rf-obp-num-obp         .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-obp-400.
       let-arc-obp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-obp-rag-obp       to   w-let-arc-obp-rag      .
           move      rf-obp-via-obp       to   w-let-arc-obp-via      .
           move      rf-obp-loc-obp       to   w-let-arc-obp-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-obp-999.
       let-arc-obp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-obp-flg      .
           move      all   "."            to   w-let-arc-obp-rag      .
           go to     let-arc-obp-600.
       let-arc-obp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-obp-rag      .
       let-arc-obp-600.
           move      spaces               to   w-let-arc-obp-via      .
           move      spaces               to   w-let-arc-obp-loc      .
       let-arc-obp-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [sfs]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-sfs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      "N"                  to   w-fnd-arc-sfs-sel      .
       fnd-arc-sfs-050.
      *              *-------------------------------------------------*
      *              * Se programma di interrogazione gia' attivo :    *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           move      "INTSCF    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     go to fnd-arc-sfs-999.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per lo  *
      *              * stesso livello per l'ammissibilita' del tasto   *
      *              * Slct                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non ammesso : no scrittura               *
      *                  *---------------------------------------------*
           if        w-fnd-arc-sfs-sns    not  = "S"
                     go to fnd-arc-sfs-300.
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-sfs-300.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname completo per l'esecu- *
      *                  * zione del sottoprogramma da richiamare, ri- *
      *                  * cercando nella tabella dei tipi interroga-  *
      *                  * zione; se non trovato : uscita              *
      *                  *---------------------------------------------*
           move      zero                 to   w-tin-ele-inx          .
       fnd-arc-sfs-320.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-num
                     go to fnd-arc-sfs-999.
           if        w-tin-alf-tin
                    (w-tin-ele-inx)       not  = "INTSCF    "
                     go to fnd-arc-sfs-320.
           if        w-tin-ovy-tin
                    (w-tin-ele-inx)       =    spaces
                     go to fnd-arc-sfs-999.
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-ovy-exe-pos          .
       fnd-arc-sfs-325.
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
       fnd-arc-sfs-335.
      *                  *---------------------------------------------*
      *                  * Cancellazione del sottoprogramma            *
      *                  *---------------------------------------------*
           move      w-ovy-exe-spv
                    (w-ovy-exe-inx)       to   w-ovy-exe-pos          .
           cancel    w-ovy-exe-pat                                    .
           subtract  1                    from w-ovy-exe-inx          .
       fnd-arc-sfs-400.
      *              *-------------------------------------------------*
      *              * Determinazione selezione avvenuta               *
      *              *-------------------------------------------------*
       fnd-arc-sfs-405.
      *                  *---------------------------------------------*
      *                  * Se non era ammessa la selezione : uscita    *
      *                  *---------------------------------------------*
           if        w-fnd-arc-sfs-sns    not  = "S"
                     go to fnd-arc-sfs-999.
       fnd-arc-sfs-410.
      *                  *---------------------------------------------*
      *                  * Lettura variabile di i.p.c. 'num-scf' dallo *
      *                  * stesso livello per numero scadenza selezio- *
      *                  * nato                                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-scf"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to fnd-arc-sfs-415
           else      go to fnd-arc-sfs-420.
       fnd-arc-sfs-415.
      *                  *---------------------------------------------*
      *                  * Se variabile esistente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di selezione avvenuta              *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-sfs-sel      .
      *                      *-----------------------------------------*
      *                      * Numero scadenza selezionata             *
      *                      *-----------------------------------------*
           move      s-num                to   w-fnd-arc-sfs-num      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-sfs-999.
       fnd-arc-sfs-420.
      *                  *---------------------------------------------*
      *                  * Se variabile non esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-sfs-999.
       fnd-arc-sfs-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione del numero di scadenze for- *
      *    * nitori coinvolte in una operazione di pagamento           *
      *    *-----------------------------------------------------------*
       det-sfp-nrs-000.
      *              *-------------------------------------------------*
      *              * Salvataggio record attuale di [sfs]             *
      *              *-------------------------------------------------*
           move      rf-sfs               to   w-det-sfp-buf-sfs      .
       det-sfp-nrs-100.
      *              *-------------------------------------------------*
      *              * Numero scadenze fornitori coinvolte nell'opera- *
      *              * zione di pagamento : a zero                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sfp-nrs-iop      .
      *              *-------------------------------------------------*
      *              * Se numero pagamento a zero : ad uscita          *
      *              *-------------------------------------------------*
           if        w-det-sfp-num-pgf    =    zero
                     go to det-sfp-nrs-900.
       det-sfp-nrs-200.
      *              *-------------------------------------------------*
      *              * Start su [sfs] per numero pagamento             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PGFSCF    "         to   f-key                  .
           move      w-det-sfp-num-pgf    to   rf-sfs-num-pgf         .
           move      zero                 to   rf-sfs-num-scf         .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sfp-nrs-900.
       det-sfp-nrs-300.
      *              *-------------------------------------------------*
      *              * Read Next su [sfs] per numero pagamento         *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sfp-nrs-900.
       det-sfp-nrs-400.
      *              *-------------------------------------------------*
      *              * Test Max su [sfs]                               *
      *              *-------------------------------------------------*
           if        rf-sfs-num-pgf       not  = w-det-sfp-num-pgf
                     go to det-sfp-nrs-900.
       det-sfp-nrs-500.
      *              *-------------------------------------------------*
      *              * Incremento numero scadenze fornitori coinvolte  *
      *              * nell'operazione di pagamento                    *
      *              *-------------------------------------------------*
           add       1                    to   w-det-sfp-nrs-iop      .
       det-sfp-nrs-600.
      *              *-------------------------------------------------*
      *              * Riciclo a leggere la scadenza successiva        *
      *              *-------------------------------------------------*
           go to     det-sfp-nrs-300.
       det-sfp-nrs-900.
      *              *-------------------------------------------------*
      *              * Ripristino record di [sfs]                      *
      *              *-------------------------------------------------*
           move      w-det-sfp-buf-sfs    to   rf-sfs                 .
       det-sfp-nrs-999.
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

