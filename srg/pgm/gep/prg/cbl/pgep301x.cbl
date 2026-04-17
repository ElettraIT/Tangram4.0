       Identification Division.
       Program-Id.                                 pgep301x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    mov                 *
      *                                   Fase:    gep301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 06/04/94    *
      *                       Ultima revisione:    NdK del 15/07/09    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti per gestione   *
      *                    portafoglio                                 *
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
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [rsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsd"                          .
      *        *-------------------------------------------------------*
      *        * [obp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfobp"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
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
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
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
      *        * Per numero scadenza                                   *
      *        *-------------------------------------------------------*
           05  w-ipc-num-sdb.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al numero di   *
      *            * scadenza                                          *
      *            *---------------------------------------------------*
               10  w-ipc-num-sdb-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al nu-  *
      *            * mero di scadenza                                  *
      *            *---------------------------------------------------*
               10  w-ipc-num-sdb-val      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Numero scadenza                                       *
      *        *-------------------------------------------------------*
           05  rr-num-sdb                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Status visualizzazione dati scadenza                  *
      *        * - N : Non visualizzati                                *
      *        * - V : Visualizzati                                    *
      *        *-------------------------------------------------------*
           05  rr-svd-sdb                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [sdb]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-sdb.
               10  w-fnd-arc-sdb-sns      pic  x(01)                  .
               10  w-fnd-arc-sdb-sel      pic  x(01)                  .
               10  w-fnd-arc-sdb-cli      pic  9(07)                  .
               10  w-fnd-arc-sdb-dpz      pic  x(04)                  .
               10  w-fnd-arc-sdb-tip      pic  9(02)                  .
               10  w-fnd-arc-sdb-tac      pic  9(02)                  .
               10  w-fnd-arc-sdb-sts      pic  9(02)                  .
               10  w-fnd-arc-sdb-num      pic  9(11)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio obbligati principali         *
      *        *-------------------------------------------------------*
           05  w-let-arc-obp.
               10  w-let-arc-obp-flg      pic  x(01)                  .
               10  w-let-arc-obp-tip      pic  9(02)                  .
               10  w-let-arc-obp-num      pic  9(11)                  .
               10  w-let-arc-obp-rag      pic  x(40)                  .
               10  w-let-arc-obp-via      pic  x(40)                  .
               10  w-let-arc-obp-loc      pic  x(40)                  .
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
               10  w-let-arc-dbt-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio dipendenze debitori          *
      *        *-------------------------------------------------------*
           05  w-let-dpz-dbt.
               10  w-let-dpz-dbt-flg      pic  x(01)                  .
               10  w-let-dpz-dbt-tip      pic  9(02)                  .
               10  w-let-dpz-dbt-cod      pic  9(07)                  .
               10  w-let-dpz-dbt-dpz      pic  x(04)                  .
               10  w-let-dpz-dbt-rag      pic  x(40)                  .
               10  w-let-dpz-dbt-via      pic  x(40)                  .
               10  w-let-dpz-dbt-loc      pic  x(40)                  .
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
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio record [sdb]                     *
      *        *-------------------------------------------------------*
           05  w-sav-rec-sdb.
               10  filler   occurs 1024   pic  x(01)                  .

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
           05  w-pga-gep-301-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo scadenza                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-sdb.
               10  w-exp-tip-sdb-num      pic  9(02)       value 11   .
               10  w-exp-tip-sdb-lun      pic  9(02)       value 04   .
               10  w-exp-tip-sdb-tbl.
                   15  filler             pic  x(04) value
                            "RD  "                                    .
                   15  filler             pic  x(04) value
                            "IE  "                                    .
                   15  filler             pic  x(04) value
                            "RIBA"                                    .
                   15  filler             pic  x(04) value
                            "CDO "                                    .
                   15  filler             pic  x(04) value
                            "MAV "                                    .
                   15  filler             pic  x(04) value
                            "RID "                                    .
                   15  filler             pic  x(04) value
                            "BB  "                                    .
                   15  filler             pic  x(04) value
                            "CCP  "                                    .
                   15  filler             pic  x(04) value
                            "RB  "                                    .
                   15  filler             pic  x(04) value
                            "TR  "                                    .
                   15  filler             pic  x(04) value
                            "PC  "                                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo acquisizione scadenza                 *
      *        *-------------------------------------------------------*
           05  w-exp-tac-sdb.
               10  w-exp-tac-sdb-num      pic  9(02)       value 02   .
               10  w-exp-tac-sdb-lun      pic  9(02)       value 40   .
               10  w-exp-tac-sdb-tbl.
                   15  filler             pic  x(40) value
                            "Emissione interna                       ".
                   15  filler             pic  x(40) value
                            "Cessione da debitore                    ".
      *        *-------------------------------------------------------*
      *        * Work per : Inoltro scadenza al debitore               *
      *        *-------------------------------------------------------*
           05  w-exp-inl-sdb.
               10  w-exp-inl-sdb-num      pic  9(02)       value 03   .
               10  w-exp-inl-sdb-lun      pic  9(02)       value 20   .
               10  w-exp-inl-sdb-tbl.
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
                            "Anticipo       "                         .
                   15  filler             pic  x(15) value
                            "               "                         .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No dilazione concordata                 *
      *        *-------------------------------------------------------*
           05  w-exp-snx-dlc.
               10  w-exp-snx-dlc-num      pic  9(02)       value 02   .
               10  w-exp-snx-dlc-lun      pic  9(02)       value 02   .
               10  w-exp-snx-dlc-tbl.
                   15  filler             pic  x(02)       value "No" .
                   15  filler             pic  x(02)       value "Si" .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No contrassegno                         *
      *        *-------------------------------------------------------*
           05  w-exp-snx-cts.
               10  w-exp-snx-cts-num      pic  9(02)       value 02   .
               10  w-exp-snx-cts-lun      pic  9(02)       value 02   .
               10  w-exp-snx-cts-tbl.
                   15  filler             pic  x(02)       value "No" .
                   15  filler             pic  x(02)       value "Si" .
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
      *        * Work per : Acconto o Saldo per la riscossione         *
      *        *-------------------------------------------------------*
           05  w-exp-aos-ris.
               10  w-exp-aos-ris-num      pic  9(02)       value 02   .
               10  w-exp-aos-ris-lun      pic  9(02)       value 10   .
               10  w-exp-aos-ris-tbl.
                   15  filler             pic  x(10) value
                            "A saldo   "                              .
                   15  filler             pic  x(10) value
                            "In conto  "                              .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo avviso ottenuto su presentazione      *
      *        *-------------------------------------------------------*
           05  w-exp-tav-ott.
               10  w-exp-tav-ott-num      pic  9(02)       value 03   .
               10  w-exp-tav-ott-lun      pic  9(02)       value 04   .
               10  w-exp-tav-ott-tbl.
                   15  filler             pic  x(04) value
                            "RIBA"                                    .
                   15  filler             pic  x(04) value
                            "CDO "                                    .
                   15  filler             pic  x(04) value
                            "MAV "                                    .

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
       01  w-stp-sdb.
      *        *-------------------------------------------------------*
      *        * Titolo per testatina per tipo operazione              *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-tst-ope          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Prompt per la stampa                                  *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-pmt-pls          pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Data per la stampa                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-dat-pls          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo s.aa.nnnnnnnn per la stampa        *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-p08-pls          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo s.aa.nnnnnn per la stampa          *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-p06-pls          pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di scadenza per la stampa                        *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-tds-pls          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 10 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-a10-pls          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 40 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-a40-pls          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione di 40 caratteri per la stampa             *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-d40-pls          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 1 carattere per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-n01-pls          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 2 caratteri per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-n02-pls          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 5 caratteri per la stampa con zeri  *
      *        * in testa                                              *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-z05-pls          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico di 7 caratteri per la stampa           *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-n07-pls          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico con segno d 11 caratteri per la stampa *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-s11-pls          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione per contabilita'                   *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-drc-ope          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo per contabilita'                    *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-npc-ope          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 1                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-edt-001          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 2                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-edt-002          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 3                                    *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-edt-003          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Area per composizione contatore numero stampe         *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-flc.
               10  w-stp-sdb-flc-snx      pic  x(02)                  .
               10  w-stp-sdb-flc-vrg      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-stp-sdb-flc-ctr      pic  z(02)                  .
               10  filler                 pic  x(01)                  .
               10  w-stp-sdb-flc-vlt      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per comodo scansione compensazione               *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-srt-ris          pic  9(11)                  .
           05  w-stp-sdb-srt-nrs          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per comodo scadenza di origine                   *
      *        *-------------------------------------------------------*
           05  w-stp-sdb-srt-nsc          pic  9(11)                  .

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

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
           perform   ipc-num-sdb-000      thru ipc-num-sdb-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "ESPSDB    "         to   w-spg-alf-gat          .
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
           move      "ESPSDB    "         to   w-spg-alf-gat          .
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
       ipc-num-sdb-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'num-sdb' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
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
                     go to ipc-num-sdb-200
           else      go to ipc-num-sdb-400.
       ipc-num-sdb-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-num-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-num-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Se valore della variabile a zero : come per *
      *                  * variabile non esistente                     *
      *                  *---------------------------------------------*
           if        w-ipc-num-sdb-val    =    zero
                     go to ipc-num-sdb-400.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di accettazione      *
      *                  *---------------------------------------------*
           move      w-ipc-num-sdb-val    to   rr-num-sdb             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-sdb-999.
       ipc-num-sdb-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-num-sdb-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-num-sdb-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-sdb-999.
       ipc-num-sdb-999.
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
           if        w-ipc-num-sdb-snx    =    "S"   and
                     w-ipc-num-sdb-val    not  = zero
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
           if        w-ipc-num-sdb-snx    =    "S"   and
                     w-ipc-num-sdb-val    not  = zero
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
      *              * [sdb]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * [ddp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * [rsd]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsd                 .
      *              *-------------------------------------------------*
      *              * [obp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
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
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
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
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [sdb]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * [ddp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * [rsd]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsd                 .
      *              *-------------------------------------------------*
      *              * [obp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
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
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
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
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
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
      *                  * Numero scadenza                             *
      *                  *---------------------------------------------*
           perform   acc-num-sdb-000      thru acc-num-sdb-999        .
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
      *              * Numero scadenza                                 *
      *              *-------------------------------------------------*
           perform   pmt-num-sdb-000      thru pmt-num-sdb-999        .
      *              *-------------------------------------------------*
      *              * Trattini di chiusura                            *
      *              *-------------------------------------------------*
           perform   pmt-trt-chs-000      thru pmt-trt-chs-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt per numero scadenza                                *
      *    *-----------------------------------------------------------*
       pmt-num-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero scadenza            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-sdb-999.
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
      *    * Accettazione campo : Numero scadenza                      *
      *    *-----------------------------------------------------------*
       acc-num-sdb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-sdb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se programma per interrogazione sca-   *
      *                  * denze in portafoglio gia' attivo per abi-   *
      *                  * litazione tasto Find                        *
      *                  *---------------------------------------------*
           move      "INTSDB    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     move  "N"            to   w-pga-gep-301-snx
           else      move  "S"            to   w-pga-gep-301-snx      .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-pga-gep-301-snx    =    "S"
                     move  "FIND"         to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-sdb           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-num-sdb-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-sdb-999.
       acc-num-sdb-250.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-sdb             .
       acc-num-sdb-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-sdb-400.
      *                  *---------------------------------------------*
      *                  * Find su scadenze debitori                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/No ammissibilita' tasto Slct         *
      *                      *-----------------------------------------*
           move      "S"                  to   w-fnd-arc-sdb-sns      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-arc-sdb-cli      .
      *                      *-----------------------------------------*
      *                      * Dipendenza cliente                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-sdb-dpz      .
      *                      *-----------------------------------------*
      *                      * Tipo scadenza e tipo acquisizione       *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-arc-sdb-tip      .
           move      zero                 to   w-fnd-arc-sdb-tac      .
      *                      *-----------------------------------------*
      *                      * Status scadenze : Tutte                 *
      *                      *-----------------------------------------*
           move      02                   to   w-fnd-arc-sdb-sts      .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           perform   fnd-arc-sdb-000      thru fnd-arc-sdb-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata : a reimposta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-fnd-arc-sdb-sel    not  = spaces
                     go to acc-num-sdb-100.
      *                  *---------------------------------------------*
      *                  * Ripresa numero scadenza selezionata         *
      *                  *---------------------------------------------*
           move      w-fnd-arc-sdb-num    to   rr-num-sdb             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero scadenza selezionata *
      *                  *---------------------------------------------*
           perform   vis-num-sdb-000      thru vis-num-sdb-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura simulata del tasto Return         *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-num-sdb-400.
       acc-num-sdb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-sdb-425.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore           *
      *                  *---------------------------------------------*
           if        rr-num-sdb           =    zero
                     go to acc-num-sdb-450
           else      go to acc-num-sdb-475.
       acc-num-sdb-450.
      *                  *---------------------------------------------*
      *                  * Se valore impostato a zero                  *
      *                  *---------------------------------------------*
       acc-num-sdb-452.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dello status di    *
      *                      * visualizzazione dati scadenza           *
      *                      *-----------------------------------------*
           if        rr-svd-sdb           =    "N"
                     go to acc-num-sdb-454
           else      go to acc-num-sdb-456.
       acc-num-sdb-454.
      *                      *-----------------------------------------*
      *                      * Se dati scadenza attualmente non visua- *
      *                      * lizzati                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione             *
      *                          *-------------------------------------*
           go to     acc-num-sdb-100.
       acc-num-sdb-456.
      *                      *-----------------------------------------*
      *                      * Se dati scadenza attualmente visualiz-  *
      *                      * zati                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Cancellazione dati scadenza         *
      *                          *-------------------------------------*
           perform   cnc-dat-sdb-000      thru cnc-dat-sdb-999        .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione             *
      *                          *-------------------------------------*
           go to     acc-num-sdb-100.
       acc-num-sdb-475.
      *                  *---------------------------------------------*
      *                  * Se valore impostato diverso da zero         *
      *                  *---------------------------------------------*
       acc-num-sdb-480.
      *                      *-----------------------------------------*
      *                      * Lettura record scadenza da [sdb]        *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rr-num-sdb           to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
       acc-num-sdb-485.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-num-sdb-500
           else      go to acc-num-sdb-490.
       acc-num-sdb-490.
      *                      *-----------------------------------------*
      *                      * Se record scadenza non esistente        *
      *                      *-----------------------------------------*
       acc-num-sdb-492.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dello status   *
      *                          * di visualizzazione dati scadenza    *
      *                          *-------------------------------------*
           if        rr-svd-sdb           =    "N"
                     go to acc-num-sdb-494
           else      go to acc-num-sdb-496.
       acc-num-sdb-494.
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
           go to     acc-num-sdb-100.
       acc-num-sdb-496.
      *                          *-------------------------------------*
      *                          * Se dati scadenza attualmente visua- *
      *                          * lizzati                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Cancellazione dati scadenza     *
      *                              *---------------------------------*
           perform   cnc-dat-sdb-000      thru cnc-dat-sdb-999        .
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Numero scadenza non esistente in archivio scadenze
      -              " !             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione         *
      *                              *---------------------------------*
           go to     acc-num-sdb-100.
       acc-num-sdb-500.
      *                      *-----------------------------------------*
      *                      * Se record scadenza esistente            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione dati scadenza       *
      *                          *-------------------------------------*
           perform   vis-dat-sdb-000      thru vis-dat-sdb-999        .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-num-sdb-600.
       acc-num-sdb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-sdb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-sdb-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-sdb-100.
       acc-num-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero scadenza                   *
      *    *-----------------------------------------------------------*
       vis-num-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-sdb           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-sdb-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione dati relativi alla scadenza                 *
      *    *-----------------------------------------------------------*
       cnc-dat-sdb-000.
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
       cnc-dat-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione dati relativi alla scadenza               *
      *    *-----------------------------------------------------------*
       vis-dat-sdb-000.
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
       vis-dat-sdb-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione vera e propria                  *
      *              *-------------------------------------------------*
       vis-dat-sdb-125.
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
           move      w-exp-tip-sdb-lun    to   v-car                  .
           move      w-exp-tip-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-sdb-tbl    to   v-txt                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-tip-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-150.
      *                  *---------------------------------------------*
      *                  * Data di emissione della scadenza            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Data di emissione :"
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
           move      rf-sdb-dtr-emi       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-sdb-175.
      *                  *---------------------------------------------*
      *                  * Codice del debitore                         *
      *                  *---------------------------------------------*
       vis-dat-sdb-177.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-179.
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
           move      rf-sdb-cod-dbt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-181.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica del debitore     *
      *                          *-------------------------------------*
           move      rf-sdb-tip-dbt       to   w-let-arc-dbt-tip      .
           move      rf-sdb-cod-dbt       to   w-let-arc-dbt-cod      .
           perform   let-arc-dbt-000      thru let-arc-dbt-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione ragione sociale del *
      *                          * debitore                            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-dbt-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-200.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del debitore              *
      *                  *---------------------------------------------*
       vis-dat-sdb-202.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza del cliente     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-204.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-dpz-dbt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-206.
      *                      *-----------------------------------------*
      *                      * Ragione sociale, indirizzo, localita'   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica dipendenza del   *
      *                          * debitore                            *
      *                          *-------------------------------------*
           move      rf-sdb-tip-dbt       to   w-let-dpz-dbt-tip      .
           move      rf-sdb-cod-dbt       to   w-let-dpz-dbt-cod      .
           move      rf-sdb-dpz-dbt       to   w-let-dpz-dbt-dpz      .
           perform   let-dpz-dbt-000      thru let-dpz-dbt-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione ragione sociale per *
      *                          * la dipendenza del debitore          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-dpz-dbt-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione indirizzo per la    *
      *                          * dipendenza del debitore             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-dpz-dbt-via    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione localita' per la    *
      *                          * dipendenza del debitore             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-dpz-dbt-loc    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-225.
      *                  *---------------------------------------------*
      *                  * Codice nostro conto bancario per pagamento  *
      *                  * tramite bonifico, o nostro conto corrente   *
      *                  * postale per pagamento tramite c/c postale   *
      *                  *---------------------------------------------*
       vis-dat-sdb-227.
      *                      *-----------------------------------------*
      *                      * Se la scadenza non e' ne' un bonifico   *
      *                      * bancario, ne' un c/c postale : nessun   *
      *                      * trattamento per la voce, altrimenti si  *
      *                      * devia al trattamento corrispondente     *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       =    07
                     go to vis-dat-sdb-229
           else if   rf-sdb-tip-sdb       =    08
                     go to vis-dat-sdb-239
           else      go to vis-dat-sdb-250.
       vis-dat-sdb-229.
      *                      *-----------------------------------------*
      *                      * Se bonifico bancario                    *
      *                      *-----------------------------------------*
       vis-dat-sdb-231.
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice nostra banca        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-233.
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-cod-cbp       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-235.
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura anagrafica banca        *
      *                              *---------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-sdb-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione denominazione   *
      *                              * banca                           *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-cbp-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-237.
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     vis-dat-sdb-250.
       vis-dat-sdb-239.
      *                      *-----------------------------------------*
      *                      * Se c/c postale                          *
      *                      *-----------------------------------------*
       vis-dat-sdb-241.
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice nostro c/c postale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-243.
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-cod-cbp       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-245.
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura anagrafica c/c postale  *
      *                              *---------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      rf-sdb-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione denominazione   *
      *                              * c/c postale                     *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-cbp-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-247.
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     vis-dat-sdb-250.
       vis-dat-sdb-250.
      *                  *---------------------------------------------*
      *                  * Codice ABI per l'appoggio in presentazione  *
      *                  *---------------------------------------------*
       vis-dat-sdb-252.
      *                      *-----------------------------------------*
      *                      * Se il tipo scadenza non prevede appog-  *
      *                      * gio bancario : nessun trattamento per   *
      *                      * la voce                                 *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       not  = 02 and
                     rf-sdb-tip-sdb       not  = 03 and
                     rf-sdb-tip-sdb       not  = 04 and
                     rf-sdb-tip-sdb       not  = 05 and
                     rf-sdb-tip-sdb       not  = 06 and
                     rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10 and
                     rf-sdb-tip-sdb       not  = 11
                     go to vis-dat-sdb-275.
       vis-dat-sdb-254.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice ABI per l'appoggio  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-256.
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
           move      rf-sdb-abi-app       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-258.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica istituto di cre- *
      *                          * dito                                *
      *                          *-------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
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
       vis-dat-sdb-275.
      *                  *---------------------------------------------*
      *                  * Codice CAB per l'appoggio in presentazione  *
      *                  *---------------------------------------------*
       vis-dat-sdb-277.
      *                      *-----------------------------------------*
      *                      * Se il tipo scadenza non prevede appog-  *
      *                      * gio bancario : nessun trattamento per   *
      *                      * la voce                                 *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       not  = 02 and
                     rf-sdb-tip-sdb       not  = 03 and
                     rf-sdb-tip-sdb       not  = 04 and
                     rf-sdb-tip-sdb       not  = 05 and
                     rf-sdb-tip-sdb       not  = 06 and
                     rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10 and
                     rf-sdb-tip-sdb       not  = 11
                     go to vis-dat-sdb-300.
       vis-dat-sdb-279.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice CAB per l'appoggio  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-281.
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
           move      rf-sdb-cab-app       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-283.
      *                      *-----------------------------------------*
      *                      * Denominazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura anagrafica sportelli isti-  *
      *                          * tuti di credito                     *
      *                          *-------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
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
       vis-dat-sdb-300.
      *                  *---------------------------------------------*
      *                  * Codice c/c per l'appoggio in presentazione  *
      *                  *---------------------------------------------*
       vis-dat-sdb-302.
      *                      *-----------------------------------------*
      *                      * Se la scadenza non e' un R.I.D. : nes-  *
      *                      * sun trattamento per la voce             *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       not  = 06
                     go to vis-dat-sdb-325.
       vis-dat-sdb-304.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice c/c per l'appoggio  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-306.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-ccc-app       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-325.
      *                  *---------------------------------------------*
      *                  * Inoltro scadenza                            *
      *                  *---------------------------------------------*
       vis-dat-sdb-327.
      *                      *-----------------------------------------*
      *                      * Se la scadenza e' stata ceduta da ter-  *
      *                      * zi : nessun trattamento per la voce     *
      *                      *-----------------------------------------*
           if        rf-sdb-tac-sdb       =    02
                     go to vis-dat-sdb-350.
       vis-dat-sdb-329.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Inoltro scadenza           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-331.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-inl-sdb-lun    to   v-car                  .
           move      w-exp-inl-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-inl-sdb-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-inl-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-350.
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
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-dts-sdb       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-sdb-375.
      *                  *---------------------------------------------*
      *                  * Importo scadenza                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo scadenza           :"
                                          to   v-alf                  .
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
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-sdb-imp-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-400.
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
           move      rf-sdb-dat-ddr       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-sdb-425.
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
           move      rf-sdb-num-ddr       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-450.
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
           if        rf-sdb-tip-ddr       =    01
                     move  01             to   v-num
           else if   rf-sdb-tip-ddr       =    02
                     move  02             to   v-num
           else if   rf-sdb-tip-ddr       =    11
                     move  03             to   v-num
           else if   rf-sdb-tip-ddr       =    21
                     move  04             to   v-num
           else if   rf-sdb-tip-ddr       =    99
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-475.
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
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-sdb-imp-ddr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-500.
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
           move      rf-sdb-nra-ddr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-525.
      *                  *---------------------------------------------*
      *                  * Fine visualizzazione dati                   *
      *                  *---------------------------------------------*
           go to     vis-dat-sdb-900.
       vis-dat-sdb-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sdb-999.
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
           move      zero                 to   rr-num-sdb             .
      *              *-------------------------------------------------*
      *              * Status visualizzazione dati scadenza            *
      *              *-------------------------------------------------*
           move      "N"                  to   rr-svd-sdb             .
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
      *              * Lettura record scadenza da [sdb]                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rr-num-sdb           to   rf-sdb-num-sdb         .
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
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-100.
      *              *-------------------------------------------------*
      *              * Stampa dati per emissione                       *
      *              *-------------------------------------------------*
           perform   stp-dat-emi-000      thru stp-dat-emi-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-150.
      *              *-------------------------------------------------*
      *              * Stampa dati per storno                          *
      *              *-------------------------------------------------*
           perform   stp-dat-sto-000      thru stp-dat-sto-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa dati per riscossione                     *
      *              *-------------------------------------------------*
           perform   stp-dat-ris-000      thru stp-dat-ris-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-225.
      *              *-------------------------------------------------*
      *              * Stampa dati per pagamento                       *
      *              *-------------------------------------------------*
           perform   stp-dat-pag-000      thru stp-dat-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-250.
      *              *-------------------------------------------------*
      *              * Stampa dati per compensazione                   *
      *              *-------------------------------------------------*
           perform   stp-dat-cmp-000      thru stp-dat-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-300.
      *              *-------------------------------------------------*
      *              * Stampa dati per inclusione in distinta          *
      *              *-------------------------------------------------*
           perform   stp-dat-iid-000      thru stp-dat-iid-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-350.
      *              *-------------------------------------------------*
      *              * Stampa dati per richiamo scadenza               *
      *              *-------------------------------------------------*
           perform   stp-dat-rsp-000      thru stp-dat-rsp-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-400.
      *              *-------------------------------------------------*
      *              * Stampa dati per accredito al dopo incasso       *
      *              *-------------------------------------------------*
           perform   stp-dat-acs-000      thru stp-dat-acs-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-450.
      *              *-------------------------------------------------*
      *              * Stampa dati per notizia di buon esito           *
      *              *-------------------------------------------------*
           perform   stp-dat-nbe-000      thru stp-dat-nbe-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-500.
      *              *-------------------------------------------------*
      *              * Stampa dati per presunto buon esito             *
      *              *-------------------------------------------------*
           perform   stp-dat-pbe-000      thru stp-dat-pbe-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-550.
      *              *-------------------------------------------------*
      *              * Stampa dati per insoluto                        *
      *              *-------------------------------------------------*
           perform   stp-dat-isp-000      thru stp-dat-isp-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-600.
      *              *-------------------------------------------------*
      *              * Stampa dati per solleciti                       *
      *              *-------------------------------------------------*
           perform   stp-dat-sol-000      thru stp-dat-sol-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-650.
      *              *-------------------------------------------------*
      *              * Stampa dati per altre informazioni              *
      *              *-------------------------------------------------*
           perform   stp-dat-ain-000      thru stp-dat-ain-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per emissione                                 *
      *    *-----------------------------------------------------------*
       stp-dat-emi-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per emissione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "               Emissione                "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-025.
      *              *-------------------------------------------------*
      *              * Data emissione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data emissione             :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-emi       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-050.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-emi       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-emi       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-075.
      *              *-------------------------------------------------*
      *              * Tipo scadenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo scadenza              :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-tip-sdb       to   w-stp-sdb-tds-pls      .
           perform   stp-tds-pls-000      thru stp-tds-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-100.
      *              *-------------------------------------------------*
      *              * Tipo acquisizione scadenza                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' un paghero' : no stampa           *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 11
                     go to stp-dat-emi-125.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo acquisizione scadenza :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tac-sdb-lun    to   v-car                  .
           move      w-exp-tac-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tac-sdb-tbl    to   v-txt                  .
           move      rf-sdb-tac-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-125.
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
       stp-dat-emi-130.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice cliente             :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-cod-dbt       to   w-stp-sdb-n07-pls      .
           perform   stp-n07-pls-000      thru stp-n07-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-135.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-emi-137.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica del debitore         *
      *                      *-----------------------------------------*
           move      rf-sdb-tip-dbt       to   w-let-arc-dbt-tip      .
           move      rf-sdb-cod-dbt       to   w-let-arc-dbt-cod      .
           perform   let-arc-dbt-000      thru let-arc-dbt-999        .
       stp-dat-emi-139.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-dbt-rag    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
       stp-dat-emi-141.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-dbt-via    =    spaces
                     go to stp-dat-emi-143.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-dbt-via    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
       stp-dat-emi-143.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-arc-dbt-loc    =    spaces
                     go to stp-dat-emi-145.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-arc-dbt-loc    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
       stp-dat-emi-145.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-emi-150.
       stp-dat-emi-150.
      *              *-------------------------------------------------*
      *              * Codice dipendenza                               *
      *              *-------------------------------------------------*
       stp-dat-emi-160.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Dipendenza del cliente     :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dpz-dbt       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-165.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-emi-166.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica dipendenza del debi- *
      *                      * tore                                    *
      *                      *-----------------------------------------*
           move      rf-sdb-tip-dbt       to   w-let-dpz-dbt-tip      .
           move      rf-sdb-cod-dbt       to   w-let-dpz-dbt-cod      .
           move      rf-sdb-dpz-dbt       to   w-let-dpz-dbt-dpz      .
           perform   let-dpz-dbt-000      thru let-dpz-dbt-999        .
       stp-dat-emi-167.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-dpz-dbt-rag    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-168.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-dpz-dbt-via    =    spaces
                     go to stp-dat-emi-169.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-dpz-dbt-via    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-169.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces : oltre                 *
      *                          *-------------------------------------*
           if        w-let-dpz-dbt-loc    =    spaces
                     go to stp-dat-emi-170.
      *                          *-------------------------------------*
      *                          * Prompt a spaces                     *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-pmt-pls      .
           perform   stp-pmt-pls-000      thru stp-pmt-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      w-let-dpz-dbt-loc    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-170.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-emi-175.
       stp-dat-emi-175.
      *              *-------------------------------------------------*
      *              * Inoltro scadenza                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se scadenza ceduta da terzi : no stampa     *
      *                  *---------------------------------------------*
           if        rf-sdb-tac-sdb       =    02
                     go to stp-dat-emi-200.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Inoltro scadenza           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-inl-sdb-lun    to   v-car                  .
           move      w-exp-inl-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-inl-sdb-tbl    to   v-txt                  .
           move      rf-sdb-inl-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-200.
      *              *-------------------------------------------------*
      *              * Data di scadenza                                *
      *              *-------------------------------------------------*
       stp-dat-emi-205.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore             *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       =    zero
                     go to stp-dat-emi-210
           else      go to stp-dat-emi-215.
       stp-dat-emi-210.
      *                  *---------------------------------------------*
      *                  * Se valore a zero                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Data di scadenza           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "A vista"            to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-emi-225.
       stp-dat-emi-215.
      *                  *---------------------------------------------*
      *                  * Se valore diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Data di scadenza           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dts-sdb       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-emi-225.
       stp-dat-emi-225.
      *              *-------------------------------------------------*
      *              * Importo scadenza                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo scadenza           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-imp-sdb       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-250.
      *              *-------------------------------------------------*
      *              * Riferimenti al documento                        *
      *              *-------------------------------------------------*
       stp-dat-emi-255.
      *                  *---------------------------------------------*
      *                  * Se data documento di riferimento a zero :   *
      *                  * no stampa                                   *
      *                  *---------------------------------------------*
           if        rf-sdb-dat-ddr       =    zero
                     go to stp-dat-emi-275.
       stp-dat-emi-260.
      *                  *---------------------------------------------*
      *                  * Data documento di riferimento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Data documento di rifer.   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dat-ddr       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-262.
      *                  *---------------------------------------------*
      *                  * Numero documento di riferimento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Numero documento di rif.   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-num-ddr       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-264.
      *                  *---------------------------------------------*
      *                  * Tipo documento di riferimento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se esiste il protocollo del documento   *
      *                      * di riferimento si legge                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione campo di appoggio   *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-edt-003      .
      *                          *-------------------------------------*
      *                          * Test sul protocollo                 *
      *                          *-------------------------------------*
           if        rf-sdb-prt-fcl       =    zero
                     go to stp-dat-emi-265.
      *                          *-------------------------------------*
      *                          * Normalizzazione record [fit]        *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                          *-------------------------------------*
      *                          * Lettura record [fit]                *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-sdb-prt-fcl       to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                              *---------------------------------*
      *                              * Test su esito lettura           *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-dat-emi-265.
      *                          *-------------------------------------*
      *                          * Test sul codice tipo movimento      *
      *                          *-------------------------------------*
           if        rf-fit-cod-tmo       =    spaces
                     go to stp-dat-emi-265.
      *                          *-------------------------------------*
      *                          * Normalizzazione record [zfi]        *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                          *-------------------------------------*
      *                          * Normalizzazione record [zfi]        *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO    "         to   f-key                  .
           move      rf-fit-cod-tmo       to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                              *---------------------------------*
      *                              * Test su esito lettura           *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-zfi-des-tmo         .
      *                      *-----------------------------------------*
      *                      * Precablaggio dati integrativi documento *
      *                      * di riferimento                          *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "-"                  to   w-all-str-cat (1)      .
           move      rf-fit-cod-tmo       to   w-all-str-cat (2)      .
           move      rf-zfi-des-tmo       to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-stp-sdb-edt-003      .
       stp-dat-emi-265.
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Tipo documento di rifer.   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddr-lun    to   v-car                  .
           move      w-exp-tip-ddr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddr-tbl    to   v-txt                  .
           if        rf-sdb-tip-ddr       =    01
                     move  01             to   v-num
           else if   rf-sdb-tip-ddr       =    02
                     move  02             to   v-num
           else if   rf-sdb-tip-ddr       =    11
                     move  03             to   v-num
           else if   rf-sdb-tip-ddr       =    21
                     move  04             to   v-num
           else if   rf-sdb-tip-ddr       =    99
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a40-pls      .
      *                      *-----------------------------------------*
      *                      * Cablaggio con eventuali dati integrati- *
      *                      * vi documento di riferimento             *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-stp-sdb-a40-pls    to   w-all-str-cat (1)      .
           move      w-stp-sdb-edt-003    to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-stp-sdb-a40-pls      .
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-266.
      *                  *---------------------------------------------*
      *                  * Importo documento di riferimento            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo docum. di rifer.   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-imp-ddr       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-268.
      *                  *---------------------------------------------*
      *                  * Numero rata documento di riferimento        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Nr. rata doc. di rifer.    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nra-ddr       to   w-stp-sdb-n02-pls      .
           perform   stp-n02-pls-000      thru stp-n02-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-275.
      *              *-------------------------------------------------*
      *              * Codice ABI per l'appoggio                       *
      *              *-------------------------------------------------*
       stp-dat-emi-280.
      *                  *---------------------------------------------*
      *                  * Se il tipo scadenza non prevede appoggio    *
      *                  * bancario : no stampa                        *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 02 and
                     rf-sdb-tip-sdb       not  = 03 and
                     rf-sdb-tip-sdb       not  = 04 and
                     rf-sdb-tip-sdb       not  = 05 and
                     rf-sdb-tip-sdb       not  = 06 and
                     rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10 and
                     rf-sdb-tip-sdb       not  = 11
                     go to stp-dat-emi-300.
       stp-dat-emi-285.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice ABI per l'appoggio  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-abi-app       to   w-stp-sdb-z05-pls      .
           perform   stp-z05-pls-000      thru stp-z05-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-290.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica istituto di credito  *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-axi-den    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-300.
      *              *-------------------------------------------------*
      *              * Codice CAB per l'appoggio                       *
      *              *-------------------------------------------------*
       stp-dat-emi-305.
      *                  *---------------------------------------------*
      *                  * Se il tipo scadenza non prevede appoggio    *
      *                  * bancario : no stampa                        *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 02 and
                     rf-sdb-tip-sdb       not  = 03 and
                     rf-sdb-tip-sdb       not  = 04 and
                     rf-sdb-tip-sdb       not  = 05 and
                     rf-sdb-tip-sdb       not  = 06 and
                     rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10 and
                     rf-sdb-tip-sdb       not  = 11
                     go to stp-dat-emi-325.
       stp-dat-emi-310.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice CAB per l'appoggio  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-cab-app       to   w-stp-sdb-z05-pls      .
           perform   stp-z05-pls-000      thru stp-z05-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-315.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica sportelli istituto   *
      *                      * di credito                              *
      *                      *-----------------------------------------*
           move      rf-sdb-abi-app       to   w-let-arc-axs-abi      .
           move      rf-sdb-cab-app       to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-axs-den    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-325.
      *              *-------------------------------------------------*
      *              * Codice c/c per l'appoggio                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' un R.I.D. : no stampa *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 06
                     go to stp-dat-emi-350.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Codice c/c per l'appoggio  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ccc-app       to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-350.
      *              *-------------------------------------------------*
      *              * Codice nostra banca per la riscossione          *
      *              *-------------------------------------------------*
       stp-dat-emi-355.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' un bonifico bancario  *
      *                  * : no stampa                                 *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 07
                     go to stp-dat-emi-375.
       stp-dat-emi-360.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice nostra banca        :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-cod-cbp       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-365.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica nostre banche        *
      *                      *-----------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-sdb-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-375.
      *              *-------------------------------------------------*
      *              * Codice nostro c/c postale per la riscossione    *
      *              *-------------------------------------------------*
       stp-dat-emi-380.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' un c/c postale : no   *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 08
                     go to stp-dat-emi-392.
       stp-dat-emi-385.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice nostro c/c postale  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-cod-cbp       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-390.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica c/c postale          *
      *                      *-----------------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      rf-sdb-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-392.
      *              *-------------------------------------------------*
      *              * Numero protocollo movimento di fatturazione     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Num. prot. di fatturazione :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-prt-fcl       to   w-stp-sdb-p06-pls      .
           perform   stp-p06-pls-000      thru stp-p06-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-396.
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice agente              :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-cod-age       to   w-stp-sdb-n07-pls      .
           perform   stp-n07-pls-000      thru stp-n07-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                  *---------------------------------------------*
      *                  * Nominativo                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica agente               *
      *                      *-----------------------------------------*
           move      rf-sdb-cod-age       to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-age-nom    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-400.
      *              *-------------------------------------------------*
      *              * Obbligato principale                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' un paghero' : no      *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 11
                     go to stp-dat-emi-425.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' stata ceduta da ter-  *
      *                  * zi : no stampa                              *
      *                  *---------------------------------------------*
           if        rf-sdb-tac-sdb       not  = 02
                     go to stp-dat-emi-425.
       stp-dat-emi-405.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica obbligato principale     *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-obp-tip      .
           move      rf-sdb-num-sdb       to   w-let-arc-obp-num      .
           perform   let-arc-obp-000      thru let-arc-obp-999        .
       stp-dat-emi-410.
      *                  *---------------------------------------------*
      *                  * Stampa anagrafica                           *
      *                  *---------------------------------------------*
       stp-dat-emi-412.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Obbligato principale       :"
                                          to   w-stp-sdb-pmt-pls      .
           move      w-let-arc-obp-rag    to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-414.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se indirizzo a spaces : oltre       *
      *                          *-------------------------------------*
           if        w-let-arc-obp-via    =    spaces
                     go to stp-dat-emi-416.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-pmt-pls      .
           move      w-let-arc-obp-via    to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-416.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se localita' a spaces : oltre       *
      *                          *-------------------------------------*
           if        w-let-arc-obp-loc    =    spaces
                     go to stp-dat-emi-418.
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-sdb-pmt-pls      .
           move      w-let-arc-obp-loc    to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-418.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-dat-emi-425.
       stp-dat-emi-425.
      *              *-------------------------------------------------*
      *              * Si/No dilazione concordata                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da stampare                   *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 01
                     go to stp-dat-emi-500.
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to stp-dat-emi-500.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Dilazione concordata       :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-dlc-lun    to   v-car                  .
           move      w-exp-snx-dlc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-dlc-tbl    to   v-txt                  .
           if        rf-sdb-snx-dlc       =    "N"
                     move  01             to   v-num
           else if   rf-sdb-snx-dlc       =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-430.
      *              *-------------------------------------------------*
      *              * Si/No contrassegno                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Contrassegno               :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-cts-lun    to   v-car                  .
           move      w-exp-snx-cts-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-cts-tbl    to   v-txt                  .
      *
           if        rf-sdb-snx-cts       =    spaces
                     move  01             to   v-num
           else if   rf-sdb-snx-cts       =    "N"
                     move  01             to   v-num
           else if   rf-sdb-snx-cts       =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-500.
      *              *-------------------------------------------------*
      *              * Scadenze di origine, in caso di compensazione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se la scadenza di origine proviene da  *
      *                  * una compensazione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-sdb-srt-nrs      .
      *                      *-----------------------------------------*
      *                      * Test se presente scadenza di origine    *
      *                      *-----------------------------------------*
           if        rf-sdb-nsc-org       =    zero
                     go to stp-dat-emi-640.
      *                      *-----------------------------------------*
      *                      * Salvataggio preliminare [sdb]           *
      *                      *-----------------------------------------*
           move      rf-sdb               to   w-sav-rec-sdb          .
      *                      *-----------------------------------------*
      *                      * Lettura [sdb] per la scadenza di ori-   *
      *                      * gine                                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-sdb-nsc-org       to   rf-sdb-num-sdb         .
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
                     go to stp-dat-emi-580.
      *                      *-----------------------------------------*
      *                      * Test se la scadenza di orgine e' stata  *
      *                      * compensata                              *
      *                      *-----------------------------------------*
           if        rf-sdb-mod-ris       not  = 50
                     go to stp-dat-emi-580.
      *                      *-----------------------------------------*
      *                      * Test se presente il numero riscossione  *
      *                      *-----------------------------------------*
           if        rf-sdb-num-ris       =    zero
                     go to stp-dat-emi-580.
       stp-dat-emi-510.
      *                      *-----------------------------------------*
      *                      * Numero riscossione determinato          *
      *                      *-----------------------------------------*
           move      rf-sdb-num-ris       to   w-stp-sdb-srt-ris      .
      *                      *-----------------------------------------*
      *                      * Scansione del file [sdb] per determina- *
      *                      * re tutte le scadenze appartenenti alla  *
      *                      * compensazione                           *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RISSDB    "         to   f-key                  .
           move      w-stp-sdb-srt-ris    to   rf-sdb-num-ris         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-dat-emi-580.
       stp-dat-emi-520.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale record [sdb]        *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                          *-------------------------------------*
      *                          * Test se 'At end'                    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-dat-emi-580.
       stp-dat-emi-530.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-sdb-num-ris       not  = w-stp-sdb-srt-ris
                     go to stp-dat-emi-580.
       stp-dat-emi-540.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-sdb-srt-nrs      .
      *                  *---------------------------------------------*
      *                  * Editing numero scadenza                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      rf-sdb-num-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Editing data documento di riferimento       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-sdb-dat-ddr       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (6)      .
      *                  *---------------------------------------------*
      *                  * Cablaggio con eventuali dati integrativi    *
      *                  * documento di riferimento                    *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           move      "- Ns."              to   w-all-str-cat (2)      .
      *
           if        rf-sdb-tip-ddr       =    01
                     move  "Fatt."        to   w-all-str-cat (3)
           else if   rf-sdb-tip-ddr       =    02
                     move  "N.Ad."        to   w-all-str-cat (3)
           else if   rf-sdb-tip-ddr       =    11
                     move  "N.Ac."        to   w-all-str-cat (3)
           else if   rf-sdb-tip-ddr       =    21
                     move  "Ant."         to   w-all-str-cat (3)
           else      move  "Rif."         to   w-all-str-cat (3)      .
      *
           move      rf-sdb-num-ddr       to   w-all-str-cat (4)      .
           move      "del"                to   w-all-str-cat (5)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-stp-sdb-a40-pls      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           if        w-stp-sdb-srt-nrs    >    1
                     move  """        ""                 :"
                                          to   w-stp-sdb-pmt-pls
           else      move  "Scadenza compensata        :"
                                          to   w-stp-sdb-pmt-pls      .
      *
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-560.
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     stp-dat-emi-520.
       stp-dat-emi-580.
      *                  *---------------------------------------------*
      *                  * Ripristino [sdb]                            *
      *                  *---------------------------------------------*
           move      w-sav-rec-sdb        to   rf-sdb                 .
       stp-dat-emi-600.
      *              *-------------------------------------------------*
      *              * Scadenza di origine, se non compensazione       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se compensazione                       *
      *                  *---------------------------------------------*
           if        w-stp-sdb-srt-nrs    >    zero
                     go to stp-dat-emi-640.
       stp-dat-emi-620.
      *                  *---------------------------------------------*
      *                  * Test se la scadenza di origine proviene da  *
      *                  * un insoluto                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio preliminare [sdb]           *
      *                      *-----------------------------------------*
           move      rf-sdb               to   w-sav-rec-sdb          .
      *                      *-----------------------------------------*
      *                      * Salvataggio numero scadenza di origine  *
      *                      *-----------------------------------------*
           move      rf-sdb-nsc-org       to   w-stp-sdb-srt-nsc      .
      *                      *-----------------------------------------*
      *                      * Lettura [sdb] per la scadenza di ori-   *
      *                      * gine                                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-sdb-nsc-org       to   rf-sdb-num-sdb         .
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
                     go to stp-dat-emi-630.
      *                  *---------------------------------------------*
      *                  * Literal scadenza di origine                 *
      *                  *---------------------------------------------*
           move      "Numero scadenza di origine :"
                                          to   w-stp-sdb-pmt-pls      .
           move      w-stp-sdb-srt-nsc    to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
      *                  *---------------------------------------------*
      *                  * Literal per insoluto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se la scadenza di orgine e' un     *
      *                      * insoluto                                *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-isp       =    zero
                     go to stp-dat-emi-630.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "*** INSOLUTA ***"   to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-emi-999.
       stp-dat-emi-630.
      *                  *---------------------------------------------*
      *                  * Ripristino [sdb]                            *
      *                  *---------------------------------------------*
           move      w-sav-rec-sdb        to   rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     stp-dat-emi-640.
       stp-dat-emi-640.
      *              *-------------------------------------------------*
      *              * Fine elementi da esporre scadenza               *
      *              *-------------------------------------------------*
       stp-dat-emi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-dat-emi-999.
       stp-dat-emi-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per storno                                    *
      *    *-----------------------------------------------------------*
       stp-dat-sto-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-sto       =    zero
                     go to stp-dat-sto-999.
       stp-dat-sto-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per storno                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "                 Storno                 "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-050.
      *              *-------------------------------------------------*
      *              * Data storno                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data storno                :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-sto       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sto-999.
       stp-dat-sto-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-sto       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-sto       to   w-stp-sdb-npc-ope      .
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
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-six-nox-lun    to   v-car                  .
           move      w-exp-six-nox-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-six-nox-tbl    to   v-txt                  .
           move      rf-sdb-ens-sto       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a10-pls      .
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
           if        rf-sdb-ens-sto       not  = 01
                     go to stp-dat-sto-150.
       stp-dat-sto-135.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nns-sto       to   w-stp-sdb-p08-pls      .
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
           if        rf-sdb-ens-sto       not  = 01
                     go to stp-dat-sto-175.
       stp-dat-sto-160.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo nuova scadenza        :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-tns-sto       to   w-stp-sdb-tds-pls      .
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
      *    * Stampa dati per riscossione                               *
      *    *-----------------------------------------------------------*
       stp-dat-ris-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-ris       =    zero
                     go to stp-dat-ris-999.
           if        rf-sdb-mod-ris       not  = 01 and
                     rf-sdb-mod-ris       not  = 02 and
                     rf-sdb-mod-ris       not  = 03 and
                     rf-sdb-mod-ris       not  = 04
                     go to stp-dat-ris-999.
       stp-dat-ris-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per riscossione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "              Riscossione               "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-050.
      *              *-------------------------------------------------*
      *              * Data riscossione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data riscossione           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-ris       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-060.
      *              *-------------------------------------------------*
      *              * Modalita' riscossione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Modalita' di riscossione   :"
                                          to   w-stp-sdb-pmt-pls      .
           if        rf-sdb-mod-ris       =    01
                     move  "Per contanti"
                                          to   w-stp-sdb-a40-pls
           else if   rf-sdb-mod-ris       =    02
                     move  "Con assegno"
                                          to   w-stp-sdb-a40-pls
           else if   rf-sdb-mod-ris       =    03
                     move  "Per bonifico bancario"
                                          to   w-stp-sdb-a40-pls
           else if   rf-sdb-mod-ris       =    04
                     move  "Per c/c postale"
                                          to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-075.
      *              *-------------------------------------------------*
      *              * Numero operazione di riscossione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero riscossione         :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-num-ris       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-100.
      *              *-------------------------------------------------*
      *              * Importo riscosso                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo riscosso           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-imp-ris       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-125.
      *              *-------------------------------------------------*
      *              * Riscossione in acconto o a saldo                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se l'importo riscosso e' pari all'importo   *
      *                  * scadenza : no stampa                        *
      *                  *---------------------------------------------*
           if        rf-sdb-imp-ris       =    rf-sdb-imp-sdb
                     go to stp-dat-ris-150.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "In acconto o a saldo       :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aos-ris-lun    to   v-car                  .
           move      w-exp-aos-ris-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aos-ris-tbl    to   v-txt                  .
           move      rf-sdb-aos-ris       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-150.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte del residuo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non si tratta di una riscossione in con- *
      *                  * to : no stampa                              *
      *                  *---------------------------------------------*
           if        rf-sdb-aos-ris       not  = 02
                     go to stp-dat-ris-175.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nns-ris       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-175.
      *              *-------------------------------------------------*
      *              * Protocollo di contabilita' eventuale            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se lettura riscossione da effettuare   *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ris       =    zero
                     go to stp-dat-ris-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [rsd] riscossione    *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsd                 .
      *                  *---------------------------------------------*
      *                  * Lettura riscossione                         *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMRSD    "         to   f-key                  .
           move      rf-sdb-num-ris       to   rf-rsd-num-rsd         .
           move      "pgm/gep/fls/ioc/obj/iofrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsd                 .
      *                      *-----------------------------------------*
      *                      * Se record non trovato : oltre           *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-dat-ris-200.
      *                  *---------------------------------------------*
      *                  * Test su numero protocollo contabilita'      *
      *                  *---------------------------------------------*
           if        rf-rsd-npc-rsd       =    zero
                     go to stp-dat-ris-200.
      *                  *---------------------------------------------*
      *                  * Test su data contabilita'                   *
      *                  *---------------------------------------------*
           if        rf-rsd-drc-rsd       =    zero
                     go to stp-dat-ris-200.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-rsd-drc-rsd       to   w-stp-sdb-drc-ope      .
           move      rf-rsd-npc-rsd       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ris-999.
       stp-dat-ris-200.
       stp-dat-ris-900.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per riscossione                *
      *              *-------------------------------------------------*
           go to     stp-dat-ris-999.
       stp-dat-ris-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per pagamento                                 *
      *    *-----------------------------------------------------------*
       stp-dat-pag-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-ris       =    zero
                     go to stp-dat-pag-999.
           if        rf-sdb-mod-ris       not  = 21 and
                     rf-sdb-mod-ris       not  = 22 and
                     rf-sdb-mod-ris       not  = 23 and
                     rf-sdb-mod-ris       not  = 24
                     go to stp-dat-pag-999.
       stp-dat-pag-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per pagamento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "               Pagamento                "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-050.
      *              *-------------------------------------------------*
      *              * Data pagamento                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data pagamento             :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-ris       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-060.
      *              *-------------------------------------------------*
      *              * Modalita' pagamento                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Modalita' di pagamento     :"
                                          to   w-stp-sdb-pmt-pls      .
           if        rf-sdb-mod-ris       =    21
                     move  "Per contanti"
                                          to   w-stp-sdb-a40-pls
           else if   rf-sdb-mod-ris       =    22
                     move  "Con assegno"
                                          to   w-stp-sdb-a40-pls
           else if   rf-sdb-mod-ris       =    23
                     move  "Per bonifico bancario"
                                          to   w-stp-sdb-a40-pls
           else if   rf-sdb-mod-ris       =    24
                     move  "Per c/c postale"
                                          to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-075.
      *              *-------------------------------------------------*
      *              * Numero operazione di pagamento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero pagamento           :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-num-ris       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-100.
      *              *-------------------------------------------------*
      *              * Importo pagato                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo pagato             :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-imp-ris       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-125.
      *              *-------------------------------------------------*
      *              * Pagamento in acconto o a saldo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se l'importo pagato e' pari all'importo     *
      *                  * scadenza : no stampa                        *
      *                  *---------------------------------------------*
           if        rf-sdb-imp-ris       =    rf-sdb-imp-sdb
                     go to stp-dat-pag-150.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "In acconto o a saldo       :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aos-ris-lun    to   v-car                  .
           move      w-exp-aos-ris-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aos-ris-tbl    to   v-txt                  .
           move      rf-sdb-aos-ris       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-150.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte del residuo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non si tratta di pagamento in conto : no *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-sdb-aos-ris       not  = 02
                     go to stp-dat-pag-175.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nns-ris       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pag-999.
       stp-dat-pag-175.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per pagamento                  *
      *              *-------------------------------------------------*
           go to     stp-dat-pag-999.
       stp-dat-pag-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per compensazione                             *
      *    *-----------------------------------------------------------*
       stp-dat-cmp-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-ris       =    zero
                     go to stp-dat-cmp-999.
           if        rf-sdb-mod-ris       not  = 50
                     go to stp-dat-cmp-999.
       stp-dat-cmp-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per compensazione              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "             Compensazione              "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cmp-999.
       stp-dat-cmp-050.
      *              *-------------------------------------------------*
      *              * Data compensazione                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data compensazione         :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-ris       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cmp-999.
       stp-dat-cmp-075.
      *              *-------------------------------------------------*
      *              * Numero operazione di compensazione              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero compensazione       :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-num-ris       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cmp-999.
       stp-dat-cmp-100.
      *              *-------------------------------------------------*
      *              * Importo compensato                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo compensato         :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-imp-ris       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cmp-999.
       stp-dat-cmp-125.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte del residuo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non si tratta di compensazione in conto: *
      *                  * no stampa                                   *
      *                  *---------------------------------------------*
           if        rf-sdb-nns-ris       =    zero
                     go to stp-dat-cmp-175.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nns-ris       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cmp-999.
       stp-dat-cmp-175.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per compensazione              *
      *              *-------------------------------------------------*
           go to     stp-dat-cmp-999.
       stp-dat-cmp-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per inclusione in distinta di presentazione   *
      *    *-----------------------------------------------------------*
       stp-dat-iid-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-num-ddp       =    zero
                     go to stp-dat-iid-999.
       stp-dat-iid-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per inclusione in distinta     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Inclusione in distinta di presentazione "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-iid-999.
       stp-dat-iid-050.
      *              *-------------------------------------------------*
      *              * Numero distinta                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero distinta            :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-num-ddp       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-iid-999.
       stp-dat-iid-075.
      *              *-------------------------------------------------*
      *              * Tipo avviso ottenuto                            *
      *              *-------------------------------------------------*
       stp-dat-iid-080.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso ottenuto non ancora determi- *
      *                  * nato : no stampa                            *
      *                  *---------------------------------------------*
           if        rf-sdb-tav-ott       =    zero
                     go to stp-dat-iid-100.
       stp-dat-iid-085.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo avviso ottenuto       :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tav-ott-lun    to   v-car                  .
           move      w-exp-tav-ott-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tav-ott-tbl    to   v-txt                  .
           if        rf-sdb-tav-ott       =    03
                     move  01             to   v-num
           else if   rf-sdb-tav-ott       =    04
                     move  02             to   v-num
           else if   rf-sdb-tav-ott       =    05
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-iid-999.
       stp-dat-iid-100.
      *              *-------------------------------------------------*
      *              * Ns. banca di presentazione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ddp]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *                  *---------------------------------------------*
      *                  * Lettura distinta da [ddp]                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      rf-sdb-num-ddp       to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to stp-dat-iid-110.
      *                  *---------------------------------------------*
      *                  * Se record non trovato : oltre               *
      *                  *---------------------------------------------*
           go to     stp-dat-iid-150.
       stp-dat-iid-110.
      *                  *---------------------------------------------*
      *                  * Test su codice nostra banca                 *
      *                  *---------------------------------------------*
           if        rf-ddp-cod-cbp       =    spaces
                     go to stp-dat-iid-150.
       stp-dat-iid-115.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Ns. banca di presentazione :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-ddp-cod-cbp       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-iid-999.
       stp-dat-iid-120.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica nostre banche        *
      *                      *-----------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-ddp-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-sdb-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-iid-999.
       stp-dat-iid-150.
       stp-dat-iid-900.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per girata a fornitore         *
      *              *-------------------------------------------------*
           go to     stp-dat-iid-999.
       stp-dat-iid-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per richiamo                                  *
      *    *-----------------------------------------------------------*
       stp-dat-rsp-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-rsp       =    zero
                     go to stp-dat-rsp-999.
       stp-dat-rsp-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per richiamo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "                Richiamo                "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-050.
      *              *-------------------------------------------------*
      *              * Data richiamo                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data richiamo              :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-rsp       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-rsp       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-rsp       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-100.
      *              *-------------------------------------------------*
      *              * Data contabile bancaria                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data contabile bancaria    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dcb-rsp       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-125.
      *              *-------------------------------------------------*
      *              * Numero contabile bancaria                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero contabile bancaria  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ncb-rsp       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-150.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate                        *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-spe-rsp       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-175.
      *              *-------------------------------------------------*
      *              * Si/No nuova scadenza a fronte richiamo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Nuova scadenza a fronte    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-six-nox-lun    to   v-car                  .
           move      w-exp-six-nox-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-six-nox-tbl    to   v-txt                  .
           move      rf-sdb-ens-rsp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-200.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte richiamo         *
      *              *-------------------------------------------------*
       stp-dat-rsp-205.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sdb-ens-rsp       not  = 01
                     go to stp-dat-rsp-225.
       stp-dat-rsp-210.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nns-rsp       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-225.
      *              *-------------------------------------------------*
      *              * Tipo nuova scadenza a fronte richiamo           *
      *              *-------------------------------------------------*
       stp-dat-rsp-230.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sdb-ens-rsp       not  = 01
                     go to stp-dat-rsp-250.
       stp-dat-rsp-235.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo nuova scadenza        :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-tns-rsp       to   w-stp-sdb-tds-pls      .
           perform   stp-tds-pls-000      thru stp-tds-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rsp-999.
       stp-dat-rsp-250.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per richiamo                   *
      *              *-------------------------------------------------*
           go to     stp-dat-rsp-999.
       stp-dat-rsp-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per accredito al dopo incasso                 *
      *    *-----------------------------------------------------------*
       stp-dat-acs-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-acs       =    zero
                     go to stp-dat-acs-999.
       stp-dat-acs-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per accredito al dopo incasso  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "   Accredito scadenza al dopo incasso   "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acs-999.
       stp-dat-acs-050.
      *              *-------------------------------------------------*
      *              * Data accredito al dopo incasso                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data accredito             :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-acs       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acs-999.
       stp-dat-acs-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-acs       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-acs       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acs-999.
       stp-dat-acs-100.
      *              *-------------------------------------------------*
      *              * Data contabile bancaria                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data contabile bancaria    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dcb-acs       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acs-999.
       stp-dat-acs-125.
      *              *-------------------------------------------------*
      *              * Numero contabile bancaria                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero contabile bancaria  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ncb-acs       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acs-999.
       stp-dat-acs-150.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate                        *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-spe-acs       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acs-999.
       stp-dat-acs-175.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per accredito al dopo incasso  *
      *              *-------------------------------------------------*
           go to     stp-dat-acs-999.
       stp-dat-acs-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per notizia di buon esito                     *
      *    *-----------------------------------------------------------*
       stp-dat-nbe-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-nbe       =    zero
                     go to stp-dat-nbe-999.
       stp-dat-nbe-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per notizia di buon esito      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         Notizia di buon esito          "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-nbe-999.
       stp-dat-nbe-050.
      *              *-------------------------------------------------*
      *              * Data notizia di buon esito                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data notizia buon esito    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-nbe       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-nbe-999.
       stp-dat-nbe-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-nbe       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-nbe       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-nbe-999.
       stp-dat-nbe-100.
      *              *-------------------------------------------------*
      *              * Data contabile bancaria                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data contabile bancaria    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dcb-nbe       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-nbe-999.
       stp-dat-nbe-125.
      *              *-------------------------------------------------*
      *              * Numero contabile bancaria                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero contabile bancaria  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ncb-nbe       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-nbe-999.
       stp-dat-nbe-150.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per notizia di buon esito      *
      *              *-------------------------------------------------*
           go to     stp-dat-nbe-999.
       stp-dat-nbe-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per presunto buon esito                       *
      *    *-----------------------------------------------------------*
       stp-dat-pbe-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-pbe       =    zero
                     go to stp-dat-pbe-999.
       stp-dat-pbe-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per presunto buon esito        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "          Presunto buon esito           "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pbe-999.
       stp-dat-pbe-050.
      *              *-------------------------------------------------*
      *              * Data presunto buon esito                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data presunto buon esito   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-pbe       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pbe-999.
       stp-dat-pbe-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-pbe       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-pbe       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pbe-999.
       stp-dat-pbe-100.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per notizia di buon esito      *
      *              *-------------------------------------------------*
           go to     stp-dat-pbe-999.
       stp-dat-pbe-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per insoluto                                  *
      *    *-----------------------------------------------------------*
       stp-dat-isp-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-isp       =    zero
                     go to stp-dat-isp-999.
       stp-dat-isp-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per insoluto                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "                Insoluto                "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-050.
      *              *-------------------------------------------------*
      *              * Data insoluto                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data insoluto              :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dtr-isp       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-sdb-drc-isp       to   w-stp-sdb-drc-ope      .
           move      rf-sdb-npc-isp       to   w-stp-sdb-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-100.
      *              *-------------------------------------------------*
      *              * Data contabile bancaria                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data contabile bancaria    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dcb-isp       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-125.
      *              *-------------------------------------------------*
      *              * Numero contabile bancaria                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero contabile bancaria  :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ncb-isp       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-150.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate                        *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-spe-isp       to   w-stp-sdb-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-175.
      *              *-------------------------------------------------*
      *              * Si/No nuova scadenza a fronte insoluto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Nuova scadenza a fronte    :"
                                          to   w-stp-sdb-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-six-nox-lun    to   v-car                  .
           move      w-exp-six-nox-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-six-nox-tbl    to   v-txt                  .
           move      rf-sdb-ens-isp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-200.
      *              *-------------------------------------------------*
      *              * Numero nuova scadenza a fronte insoluto         *
      *              *-------------------------------------------------*
       stp-dat-isp-205.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sdb-ens-isp       not  = 01
                     go to stp-dat-isp-225.
       stp-dat-isp-210.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero nuova scadenza      :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-nns-isp       to   w-stp-sdb-p08-pls      .
           perform   stp-p08-pls-000      thru stp-p08-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-225.
      *              *-------------------------------------------------*
      *              * Tipo nuova scadenza a fronte insoluto           *
      *              *-------------------------------------------------*
       stp-dat-isp-230.
      *                  *---------------------------------------------*
      *                  * Se non e' stata emessa una nuova scadenza a *
      *                  * fronte : no stampa                          *
      *                  *---------------------------------------------*
           if        rf-sdb-ens-isp       not  = 01
                     go to stp-dat-isp-250.
       stp-dat-isp-235.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo nuova scadenza        :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-tns-isp       to   w-stp-sdb-tds-pls      .
           perform   stp-tds-pls-000      thru stp-tds-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-isp-999.
       stp-dat-isp-250.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per insoluto                   *
      *              *-------------------------------------------------*
           go to     stp-dat-isp-999.
       stp-dat-isp-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per solleciti                                 *
      *    *-----------------------------------------------------------*
       stp-dat-sol-000.
      *              *-------------------------------------------------*
      *              * Se solleciti non presenti : uscita              *
      *              *-------------------------------------------------*
           if        rf-sdb-liv-slc       =    zero and
                     rf-sdb-dso-l01       =    zero and
                     rf-sdb-dso-l02       =    zero and
                     rf-sdb-dso-l03       =    zero
                     go to stp-dat-sol-999.
       stp-dat-sol-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per solleciti                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "               Solleciti                "
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sol-999.
       stp-dat-sol-010.
      *              *-------------------------------------------------*
      *              * Ultimo livello sollecito                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non sottoposta a sollecito : no stampa   *
      *                  *---------------------------------------------*
           if        rf-sdb-liv-slc       =    zero
                     go to stp-dat-sol-012.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Ultimo livello di sollecito:"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-liv-slc       to   w-stp-sdb-n01-pls      .
           perform   stp-n01-pls-000      thru stp-n01-pls-999        .
       stp-dat-sol-012.
      *              *-------------------------------------------------*
      *              * 1. data inclusione in sollecito                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : no stampa                       *
      *                  *---------------------------------------------*
           if        rf-sdb-dso-l01       =    zero
                     go to stp-dat-sol-014.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "1. inclusione in sollecito :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dso-l01       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
       stp-dat-sol-014.
      *              *-------------------------------------------------*
      *              * 2. data inclusione in sollecito                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : no stampa                       *
      *                  *---------------------------------------------*
           if        rf-sdb-dso-l02       =    zero
                     go to stp-dat-sol-016.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "2. inclusione in sollecito :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dso-l02       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
       stp-dat-sol-016.
      *              *-------------------------------------------------*
      *              * 3. data inclusione in sollecito                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : no stampa                       *
      *                  *---------------------------------------------*
           if        rf-sdb-dso-l03       =    zero
                     go to stp-dat-sol-200.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "3. inclusione in sollecito :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dso-l03       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
       stp-dat-sol-200.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per solleciti                  *
      *              *-------------------------------------------------*
           go to     stp-dat-sol-999.
       stp-dat-sol-999.
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
                                          to   w-stp-sdb-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-010.
      *              *-------------------------------------------------*
      *              * Data chiusura scadenza                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se scadenza ancora aperta : no stampa       *
      *                  *---------------------------------------------*
           if        rf-sdb-dat-chs       =    9999999
                     go to stp-dat-ain-020.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data chiusura scadenza     :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-dat-chs       to   w-stp-sdb-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-020.
      *              *-------------------------------------------------*
      *              * Stampa scadenza eseguita oppure no              *
      *              *-------------------------------------------------*
       stp-dat-ain-030.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di un tipo scadenza sotto- *
      *                  * ponibile alla stampa : no stampa            *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10
                     go to stp-dat-ain-050.
       stp-dat-ain-035.
      *                  *---------------------------------------------*
      *                  * Preparazione literal di stampa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare area per    *
      *                      * composizione                            *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sdb-flc          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del contatore di   *
      *                      * stampe eseguite sulla scadenza          *
      *                      *-----------------------------------------*
           if        rf-sdb-flc-stp       =    zero
                     go to stp-dat-ain-036
           else if   rf-sdb-flc-stp       =    1
                     go to stp-dat-ain-037
           else      go to stp-dat-ain-038.
       stp-dat-ain-036.
      *                      *-----------------------------------------*
      *                      * Se contatore stampe a zero              *
      *                      *-----------------------------------------*
           move      "No"                 to   w-stp-sdb-flc-snx      .
           go to     stp-dat-ain-040.
       stp-dat-ain-037.
      *                      *-----------------------------------------*
      *                      * Se contatore stampe a 1                 *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-sdb-flc-snx      .
           go to     stp-dat-ain-040.
       stp-dat-ain-038.
      *                      *-----------------------------------------*
      *                      * Se contatore stampe maggiore di 1       *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-sdb-flc-snx      .
           move      ","                  to   w-stp-sdb-flc-vrg      .
           move      rf-sdb-flc-stp       to   w-stp-sdb-flc-ctr      .
           move      "volte"              to   w-stp-sdb-flc-vlt      .
           go to     stp-dat-ain-040.
       stp-dat-ain-040.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Stampa scadenza eseguita   :"
                                          to   w-stp-sdb-pmt-pls      .
           move      w-stp-sdb-flc        to   w-stp-sdb-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
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
           if        rf-sdb-flg-blo       =    spaces
                     go to stp-dat-ain-075.
       stp-dat-ain-060.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags bloccanti            :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-flg-blo       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
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
           if        rf-sdb-flg-nbl       =    spaces
                     go to stp-dat-ain-100.
       stp-dat-ain-085.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags non bloccanti        :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-flg-nbl       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
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
           if        rf-sdb-flg-pul       =    spaces
                     go to stp-dat-ain-125.
       stp-dat-ain-110.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags per pulizia          :"
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-flg-pul       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
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
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ide-dat       to   w-stp-sdb-dat-pls      .
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
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ide-ute       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
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
                                          to   w-stp-sdb-pmt-pls      .
           move      rf-sdb-ide-fas       to   w-stp-sdb-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
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
           move      w-stp-sdb-tst-ope    to   v-alf                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa data                                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sdb-dat-pls    to   v-dat                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-p08-pls    to   v-num                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-p06-pls    to   v-num                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa tipo di scadenza                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sdb-lun    to   v-car                  .
           move      w-exp-tip-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-sdb-tbl    to   v-txt                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sdb-tds-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tds-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo alfanumerico di 10 caratteri           *
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-a10-pls    to   v-alf                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-a40-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a40-pls-999.
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
           move      w-stp-sdb-d40-pls    to   v-alf                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-n01-pls    to   v-num                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-n02-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-n02-pls-999.
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-z05-pls    to   v-num                  .
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
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
           move      w-stp-sdb-n07-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-n07-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico con segno di 11 caratteri     *
      *    * espresso in valuta base                                   *
      *    *-----------------------------------------------------------*
       stp-s11-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-s11-pls-999.
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
           move      w-stp-sdb-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa valore in valuta base                    *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-sdb-s11-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-s11-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa data registrazione e numero protocollo di contabi- *
      *    * lita' generale                                            *
      *    *-----------------------------------------------------------*
       stp-drc-npc-000.
      *              *-------------------------------------------------*
      *              * Se i valori sono entrambi a zero : uscita       *
      *              *-------------------------------------------------*
           if        w-stp-sdb-drc-ope    =    zero and
                     w-stp-sdb-npc-ope    =    zero
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
           move      w-stp-sdb-npc-ope    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-edt-001      .
      *              *-------------------------------------------------*
      *              * Editing data di registrazione                   *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-stp-sdb-drc-ope    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-sdb-edt-002      .
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
           string    w-stp-sdb-edt-001
                                delimited by   spaces
                     " del "
                                delimited by   size
                     w-stp-sdb-edt-002
                                delimited by   spaces
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-drc-npc-400.
      *              *-------------------------------------------------*
      *              * Verifica esistenza registrazione contabile      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mgt]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mgt]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      w-stp-sdb-drc-ope    to   rf-mgt-dat-reg         .
           move      w-stp-sdb-npc-ope    to   rf-mgt-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to stp-drc-npc-999.
      *                  *---------------------------------------------*
      *                  * Se record non trovato                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      51                   to   v-pos                  .
           move      "*** CANCELLATO ***" to   v-alf                  .
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
           move      rf-sdb-num-sdb       to   v-num                  .
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
           move      zero                 to   w-let-arc-dbt-cge      .
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
           move      zero                 to   w-let-arc-dbt-cge      .
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
           move      w-let-arc-cli-cge    to   w-let-arc-dbt-cge      .
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
           move      zero                 to   w-let-arc-dbt-cge      .
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
           move      zero                 to   w-let-arc-dbt-cge      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dbt]                         *
      *    *-----------------------------------------------------------*
       let-dpz-dbt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-dpz-dbt-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo debitore          *
      *              *-------------------------------------------------*
           if        w-let-dpz-dbt-tip    =    zero
                     go to let-dpz-dbt-100
           else if   w-let-dpz-dbt-tip    =    01
                     go to let-dpz-dbt-200
           else      go to let-dpz-dbt-900.
       let-dpz-dbt-100.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : zero                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore e  *
      *                  * della dipendenza                            *
      *                  *---------------------------------------------*
           if        w-let-dpz-dbt-cod    =    zero   and
                     w-let-dpz-dbt-dpz    =    spaces
                     go to let-dpz-dbt-120
           else      go to let-dpz-dbt-140.
       let-dpz-dbt-120.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza a zero ed a *
      *                  * spaces                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-140.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza diversi da  *
      *                  * zero e spaces                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-dpz-dbt-flg      .
           move      all   "."            to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-200.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : 01                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcc]                      *
      *                  *---------------------------------------------*
           if        w-let-dpz-dbt-dpz    =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      w-let-dpz-dbt-cod    to   w-let-arc-dcc-cod      .
           move      w-let-dpz-dbt-dpz    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Riporto risultati da lettura                *
      *                  *---------------------------------------------*
           move      w-let-arc-dcc-flg    to   w-let-dpz-dbt-flg      .
           move      w-let-arc-dcc-rag    to   w-let-dpz-dbt-rag      .
           move      w-let-arc-dcc-via    to   w-let-dpz-dbt-via      .
           move      w-let-arc-dcc-loc    to   w-let-dpz-dbt-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-900.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : non riconosciuto             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore e  *
      *                  * della dipendenza                            *
      *                  *---------------------------------------------*
           if        w-let-dpz-dbt-cod    =    zero   and
                     w-let-dpz-dbt-dpz    =    spaces
                     go to let-dpz-dbt-920
           else      go to let-dpz-dbt-940.
       let-dpz-dbt-920.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza a zero ed a *
      *                  * spaces                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-940.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza diversi da  *
      *                  * zero e spaces                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-dpz-dbt-flg      .
           move      all   "."            to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-999.
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
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-nom      .
           go to     let-arc-age-999.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [sdb]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-sdb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      "N"                  to   w-fnd-arc-sdb-sel      .
       fnd-arc-sdb-050.
      *              *-------------------------------------------------*
      *              * Se programma di interrogazione gia' attivo :    *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           move      "INTSDB    "         to   w-spg-alf-gat          .
           perform   tst-spg-gat-000      thru tst-spg-gat-999        .
           if        w-spg-snx-gat        not  = spaces
                     go to fnd-arc-sdb-999.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per lo  *
      *              * stesso livello per l'ammissibilita' del tasto   *
      *              * Slct                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non ammesso : no scrittura               *
      *                  *---------------------------------------------*
           if        w-fnd-arc-sdb-sns    not  = "S"
                     go to fnd-arc-sdb-300.
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
       fnd-arc-sdb-300.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname completo per l'esecu- *
      *                  * zione del sottoprogramma da richiamare, ri- *
      *                  * cercando nella tabella dei tipi interroga-  *
      *                  * zione; se non trovato : uscita              *
      *                  *---------------------------------------------*
           move      zero                 to   w-tin-ele-inx          .
       fnd-arc-sdb-320.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-num
                     go to fnd-arc-sdb-999.
           if        w-tin-alf-tin
                    (w-tin-ele-inx)       not  = "INTSDB    "
                     go to fnd-arc-sdb-320.
           if        w-tin-ovy-tin
                    (w-tin-ele-inx)       =    spaces
                     go to fnd-arc-sdb-999.
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-ovy-exe-pos          .
       fnd-arc-sdb-325.
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
       fnd-arc-sdb-335.
      *                  *---------------------------------------------*
      *                  * Cancellazione del sottoprogramma            *
      *                  *---------------------------------------------*
           move      w-ovy-exe-spv
                    (w-ovy-exe-inx)       to   w-ovy-exe-pos          .
           cancel    w-ovy-exe-pat                                    .
           subtract  1                    from w-ovy-exe-inx          .
       fnd-arc-sdb-400.
      *              *-------------------------------------------------*
      *              * Determinazione selezione avvenuta               *
      *              *-------------------------------------------------*
       fnd-arc-sdb-405.
      *                  *---------------------------------------------*
      *                  * Se non era ammessa la selezione : uscita    *
      *                  *---------------------------------------------*
           if        w-fnd-arc-sdb-sns    not  = "S"
                     go to fnd-arc-sdb-999.
       fnd-arc-sdb-410.
      *                  *---------------------------------------------*
      *                  * Lettura variabile di i.p.c. 'num-sdb' dallo *
      *                  * stesso livello per numero scadenza selezio- *
      *                  * nato                                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to fnd-arc-sdb-415
           else      go to fnd-arc-sdb-420.
       fnd-arc-sdb-415.
      *                  *---------------------------------------------*
      *                  * Se variabile esistente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di selezione avvenuta              *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-sdb-sel      .
      *                      *-----------------------------------------*
      *                      * Numero scadenza selezionata             *
      *                      *-----------------------------------------*
           move      s-num                to   w-fnd-arc-sdb-num      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-sdb-999.
       fnd-arc-sdb-420.
      *                  *---------------------------------------------*
      *                  * Se variabile non esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-sdb-999.
       fnd-arc-sdb-999.
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

