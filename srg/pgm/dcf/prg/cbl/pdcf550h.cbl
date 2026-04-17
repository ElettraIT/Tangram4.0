       Identification Division.
       Program-Id.                                 pdcf550h           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    lst                 *
      *                                   Fase:    dcf550              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 16/02/04    *
      *                       Ultima revisione:    NdK del 24/11/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni automatiche su listini fornitori *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Storicizzazione listino fornitore           *
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
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [lfd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rflfd"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data di sistema attuale                               *
      *        *-------------------------------------------------------*
           05  rr-dat-sys                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di sistema attuale portata a fine mese           *
      *        *-------------------------------------------------------*
           05  rr-dsa-afm                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice fornitore di cui storicizzare il listino       *
      *        *-------------------------------------------------------*
           05  rr-cod-dcf                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice fornitore di cui storicizzare il listino,      *
      *        * ragione sociale                                       *
      *        *-------------------------------------------------------*
           05  rr-cod-dcf-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice fornitore di cui storicizzare il listino,      *
      *        * sigla valuta                                          *
      *        *-------------------------------------------------------*
           05  rr-cod-dcf-vlt             pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Data di validita' finale dei prezzi di listino da     *
      *        * storicizzare                                          *
      *        *                                                       *
      *        * Non puo' essere a zero                                *
      *        *-------------------------------------------------------*
           05  rr-dva-fin                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di validita' finale dei prezzi di listino da     *
      *        * storicizzare, in formato 'reverse'                    *
      *        *                                                       *
      *        * Si ottiene con il seguente calcolo:                   *
      *        * rr-dvf-rev = (99999999-(19000000+rr-dva-fin))         *
      *        *                                                       *
      *        * Se rr-dva-fin vale zero, questo campo viene forzato   *
      *        * al valore 99999999                                    *
      *        *-------------------------------------------------------*
           05  rr-dvf-rev                 pic  9(08)                  .

      *    *===========================================================*
      *    * Work per controllo validita' richieste                    *
      *    *-----------------------------------------------------------*
       01  w-ctl-val-ric.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *  - Spaces : Ok                                        *
      *        *  - #      : Ko                                        *
      *        *-------------------------------------------------------*
           05  w-ctl-val-ric-flg          pic  x(01)                  .

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
               10  w-let-arc-fnt-piv      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-vlt      pic  x(03)                  .

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

      *    *===========================================================*
      *    * Work per esecuzione storicizzazione effettiva             *
      *    *-----------------------------------------------------------*
       01  w-sto-lst.
      *        *-------------------------------------------------------*
      *        * Comodo per editing data di validita' finale dichiara- *
      *        * ta                                                    *
      *        *-------------------------------------------------------*
           05  w-sto-lst-dfd-edt          pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per editing data di validita' finale trovata   *
      *        * ta                                                    *
      *        *-------------------------------------------------------*
           05  w-sto-lst-dft-edt          pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per editing numero di 9 cifre allineato a si-  *
      *        * nistra                                                *
      *        *-------------------------------------------------------*
           05  w-sto-lst-cdl-n9s          pic  x(09)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita dalle singole subroutines attinenti la *
      *        * storicizzazione dei listini                           *
      *        *-------------------------------------------------------*
           05  w-sto-lst-flg-exs          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori per l'esecuzione globale della storicizza-  *
      *        * zione                                                 *
      *        *-------------------------------------------------------*
           05  w-sto-lst-ctt-exe.
      *            *---------------------------------------------------*
      *            * Numero records letti e presi in considerazione    *
      *            *---------------------------------------------------*
               10  w-sto-lst-ctt-lec      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Numero records scritti ex novo o aggiornati       *
      *            *---------------------------------------------------*
               10  w-sto-lst-ctt-sen      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Numero records riscritti solo per aggiornamento   *
      *            * data finale                                       *
      *            *---------------------------------------------------*
               10  w-sto-lst-ctt-apd      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Sub-area per esecuzione aggiornamento archivio sto-   *
      *        * rico listini                                          *
      *        *-------------------------------------------------------*
           05  w-sto-lst-are-agg.
      *            *---------------------------------------------------*
      *            * Indici di comodo                                  *
      *            *---------------------------------------------------*
               10  w-sto-lst-inx-00i      pic  9(02)                  .
               10  w-sto-lst-inx-00j      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Flag di prezzi variati                            *
      *            *---------------------------------------------------*
               10  w-sto-lst-flg-vrp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tipo aggiornamento da eseguire                *
      *        *  - N : Record nuovo                                   *
      *        *  - U : Record vecchio da aggiornare                   *
      *        *  - V : Record vecchio da sostituire                   *
      *        *-------------------------------------------------------*
           05  w-sto-lst-tip-agg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag per operazione di scrittura nuovo record         *
      *        *  - Spaces : Non eseguita                              *
      *        *  - S      : Eseguita Ok                               *
      *        *  - E      : Eseguita ma con errori                    *
      *        *-------------------------------------------------------*
           05  w-sto-lst-flg-put          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag per operazione di cancellazione vecchio record   *
      *        *  - Spaces : Non eseguita                              *
      *        *  - S      : Eseguita Ok                               *
      *        *  - E      : Eseguita ma con errori                    *
      *        *-------------------------------------------------------*
           05  w-sto-lst-flg-del          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio record di [lfd]                           *
      *        *-------------------------------------------------------*
           05  w-sto-lst-sav-lfd.
               10  filler  occurs 1024    pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice commerciale fornitore   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pdcf5500       *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/pdcf5500.pgl"                   .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-ovy-exe
                                               w-tmn
                                               w-spg
                                               w-prs                  .
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
                                         using v                      .
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
      *              * Esecuzione sub-program                          *
      *              *-------------------------------------------------*
           perform   exe-sub-pgm-000      thru exe-sub-pgm-999        .
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
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      "dcf550"             to   v-alf                  .
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
           move      "   STORICIZZAZIONE PREZZI DI LISTINO    "
                                          to   v-alf                  .
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
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-ovy-exe-vis        not  = "V"
                     go to pre-exe-pgm-020.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Programma non eseguibile dall'utente !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-020.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
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
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
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
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [lfd]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
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
      *              * [dps]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [lfd]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
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
      *              * [dps]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
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
      *                  * Data di sistema attuale e relativo fine me- *
      *                  * se                                          *
      *                  *---------------------------------------------*
           perform   acc-dat-sys-000      thru acc-dat-sys-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Codice fornitore di cui storicizzare i      *
      *                  * prezzi di listino                           *
      *                  *---------------------------------------------*
           perform   acc-cod-dcf-000      thru acc-cod-dcf-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data di validita' finale dei prezzi da sto- *
      *                  * ricizzare                                   *
      *                  *---------------------------------------------*
           perform   acc-dva-fin-000      thru acc-dva-fin-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
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
           move      spaces               to   v-alf                  .
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
      *              * Codice fornitore di cui storicizzare i prezzi   *
      *              * di listino                                      *
      *              *-------------------------------------------------*
           perform   pmt-cod-dcf-000      thru pmt-cod-dcf-999        .
      *              *-------------------------------------------------*
      *              * Data di validita' finale dei prezzi da stori-   *
      *              * cizzare                                         *
      *              *-------------------------------------------------*
           perform   pmt-dva-fin-000      thru pmt-dva-fin-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompts per : Codice del fornitore per cui storicizzare   *
      *    *               i prezzi                                    *
      *    *-----------------------------------------------------------*
       pmt-cod-dcf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice del fornitore per   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      cui storicizzare i    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       prezzi di listino    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-dcf-999.
           exit.

      *    *===========================================================*
      *    * Prompts per : Data di validita' finale dei prezzi da      *
      *    *               storicizzare                                *
      *    *-----------------------------------------------------------*
       pmt-dva-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data fino alla quale sono  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        rimasti in vigore   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "   i prezzi di listino da   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "             storicizzare   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dva-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di sistema attuale, e relativo fine   *
      *    *                mese                                       *
      *    *-----------------------------------------------------------*
       acc-dat-sys-000.
      *              *-------------------------------------------------*
      *              * Se gia' diversa da zero : uscita immediata      *
      *              *-------------------------------------------------*
           if        rr-dat-sys           not  = zero
                     go to acc-dat-sys-999.
       acc-dat-sys-100.
      *              *-------------------------------------------------*
      *              * Estrazione data di sistema                      *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione data di sistema                  *
      *              *-------------------------------------------------*
           move      s-dat                to   rr-dat-sys             .
       acc-dat-sys-200.
      *              *-------------------------------------------------*
      *              * Determinazione della data di sistema attuale    *
      *              * portata a fine mese                             *
      *              *-------------------------------------------------*
       acc-dat-sys-210.
           move      31                   to   s-gio                  .
       acc-dat-sys-220.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to acc-dat-sys-230.
           subtract  1                    from s-gio                  .
           go to     acc-dat-sys-220.
       acc-dat-sys-230.
      *              *-------------------------------------------------*
      *              * Memorizzazione della data di sistema attuale    *
      *              * portata a fine mese                             *
      *              *-------------------------------------------------*
           move      s-dat                to   rr-dsa-afm             .
       acc-dat-sys-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice del fornitore per cui storicizzare  *
      *    *                i prezzi di listino                        *
      *    *-----------------------------------------------------------*
       acc-cod-dcf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dcf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      rr-cod-dcf           to   w-cod-mne-dcf-cod      .
           move      05                   to   w-cod-mne-dcf-lin      .
           move      30                   to   w-cod-mne-dcf-pos      .
           move      05                   to   w-cod-mne-dcf-rln      .
           move      41                   to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-cod-dcf-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-dcf-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-dcf-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dcf-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-dcf-110.
       acc-cod-dcf-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
       acc-cod-dcf-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-dcf-999.
       acc-cod-dcf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-dcf             .
       acc-cod-dcf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-dcf-410.
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica contabile del     *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           move      rr-cod-dcf           to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica commerciale del   *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           move      rr-cod-dcf           to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
       acc-cod-dcf-420.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale del forni-   *
      *                  * tore                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     move  w-let-arc-dcf-rag
                                          to   rr-cod-dcf-rag
           else      move  w-let-arc-fnt-rag
                                          to   rr-cod-dcf-rag         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-dcf-rag-000  thru vis-cod-dcf-rag-999    .
       acc-cod-dcf-430.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice valuta da anagrafica  *
      *                  * commerciale del fornitore                   *
      *                  *---------------------------------------------*
           move      w-let-arc-dcf-vlt    to   rr-cod-dcf-vlt         .
       acc-cod-dcf-440.
      *                  *---------------------------------------------*
      *                  * Se mancano sia l'anagrafica contabile che   *
      *                  * quella commerciale : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    not  = spaces and
                     w-let-arc-dcf-flg    not  = spaces
                     go to acc-cod-dcf-100.
      *                  *---------------------------------------------*
      *                  * Se manca l'anagrafica commerciale : messag- *
      *                  * gio e reimpostazione                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-dcf-450.
           move      "Mancano i dati commerciali del fornitore !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dcf-100.
       acc-cod-dcf-450.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione           *
      *                  *---------------------------------------------*
           if        rr-cod-dcf           not  = zero
                     go to acc-cod-dcf-600.
           go to     acc-cod-dcf-100.
       acc-cod-dcf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dcf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-dcf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-dcf-100.
       acc-cod-dcf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice del fornitore per cui storiciz-  *
      *    *                   zare i prezzi di listino                *
      *    *-----------------------------------------------------------*
       vis-cod-dcf-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-dcf           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dcf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice del fornitore per cui storiciz-  *
      *    *                   zare i prezzi di listino, ragione so-   *
      *    *                   ciale                                   *
      *    *-----------------------------------------------------------*
       vis-cod-dcf-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-dcf-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dcf-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di validita' finale per i prezzi da   *
      *    *                storicizzare                               *
      *    *-----------------------------------------------------------*
       acc-dva-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dva-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      ">"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dva-fin           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dva-fin-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dva-fin-999.
       acc-dva-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dva-fin             .
       acc-dva-fin-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-dva-fin           not  = zero
                     go to acc-dva-fin-600.
           if        v-key                =    "UP  "
                     go to acc-dva-fin-600
           else      go to acc-dva-fin-100.
       acc-dva-fin-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dva-fin-620.
      *                  *---------------------------------------------*
      *                  * Preparazione della data di validita' finale *
      *                  * in formato reverse                          *
      *                  *---------------------------------------------*
           move      99999999             to   rr-dvf-rev             .
           if        rr-dva-fin           not  = zero
                     subtract 19000000    from rr-dvf-rev
                     subtract rr-dva-fin  from rr-dvf-rev             .
       acc-dva-fin-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dva-fin-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dva-fin-100.
       acc-dva-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data di validita' finale per i prezzi   *
      *    *                   da storicizzare                         *
      *    *-----------------------------------------------------------*
       vis-dva-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dva-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dva-fin-999.
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
           if        rr-cod-dcf           not  = zero
                     go to tdo-ric-sel-300.
           move      "Manca il codice del fornitore !                   
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.

       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su data di validita' finale           *
      *              *-------------------------------------------------*
           if        rr-dva-fin           not  = zero
                     go to tdo-ric-sel-400.
           move      "Manca la data di riferimento !                    
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo su validita' richieste                *
      *              *-------------------------------------------------*
           perform   ctl-val-ric-000      thru ctl-val-ric-999        .
           if        w-ctl-val-ric-flg    =    spaces
                     go to tdo-ric-sel-500
           else      go to tdo-ric-sel-950.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazione eventuale sigla valuta asso-   *
      *              * ciata al fornitore                              *
      *              *-------------------------------------------------*
           if        rr-cod-dcf-vlt       =    spaces
                     move  c-sgl          to   rr-cod-dcf-vlt         .
       tdo-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore                   *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       tdo-ric-sel-950.
      *              *-------------------------------------------------*
      *              * Flag di errore in uscita                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
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
      *              * Data di sistema attuale                         *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-sys             .
      *              *-------------------------------------------------*
      *              * Data di sistema attuale portata a fine mese     *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dsa-afm             .
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           move      zero                 to   rr-cod-dcf             .
      *              *-------------------------------------------------*
      *              * Codice fornitore, ragione sociale               *
      *              *-------------------------------------------------*
           move      spaces               to   rr-cod-dcf-rag         .
      *              *-------------------------------------------------*
      *              * Codice fornitore, sigla valuta                  *
      *              *-------------------------------------------------*
           move      spaces               to   rr-cod-dcf-vlt         .
      *              *-------------------------------------------------*
      *              * Data di validita' finale del listino            *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dva-fin             .
      *              *-------------------------------------------------*
      *              * Data di validita' finale del listino in formato *
      *              * 'reverse'                                       *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dvf-rev             .
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
      *    * Routine lettura archivio [fnt]                            *
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
           move      rf-fnt-prt-iva       to   w-let-arc-fnt-piv      .
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
           move      zero                 to   w-let-arc-fnt-piv      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcf]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-fnt    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      w-let-arc-dcf-fnt    to   rf-dcf-cod-fnt         .
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
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-600.
           move      spaces               to   w-let-arc-dcf-vlt      .
       let-arc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Controllo su validita' richieste                          *
      *    *-----------------------------------------------------------*
       ctl-val-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita a Ok             *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-val-ric-flg      .
       ctl-val-ric-200.
      *              *-------------------------------------------------*
      *              * Controllo che non esistano prezzi storicizzati  *
      *              * con data di validita' finale incompatibile con  *
      *              * la data di validita' finale dichiarata          *
      *              *-------------------------------------------------*
       ctl-val-ric-210.
      *                  *---------------------------------------------*
      *                  * Start per data finale in formato 'reverse'  *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTFNTDFR "         to   f-key                  .
           move      01                   to   rf-lfd-tip-rec         .
           move      rr-cod-dcf           to   rf-lfd-cod-dcf         .
           move      rr-cod-dcf-vlt       to   rf-lfd-sgl-vlt         .
           move      zero                 to   rf-lfd-tip-mag         .
           move      zero                 to   rf-lfd-num-mag         .
           move      spaces               to   rf-lfd-fda-pif         .
           move      zero                 to   rf-lfd-dvf-rev         .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : controllo superato        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-val-ric-900.
       ctl-val-ric-220.
      *                  *---------------------------------------------*
      *                  * Read next per data finale in formato 're-   *
      *                  * verse'                                      *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                  *---------------------------------------------*
      *                  * Se At end : controllo superato              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-val-ric-900.
       ctl-val-ric-230.
      *                  *---------------------------------------------*
      *                  * Test max                                    *
      *                  *---------------------------------------------*
           if        rf-lfd-tip-rec       not  = 01                 or
                     rf-lfd-cod-dcf       not  = rr-cod-dcf
                     go to ctl-val-ric-900.
       ctl-val-ric-240.
      *                  *---------------------------------------------*
      *                  * Controllo che la data di validita' finale   *
      *                  * del prezzo in esame non sia superiore alla  *
      *                  * data di validita' finale dichiarata         *
      *                  *                                             *
      *                  * Nota : E' ammessa la data uguale, perche'   *
      *                  *        cosi' e' possibile rieseguire una    *
      *                  *        storicizzazione con la stessa data   *
      *                  *        in caso di storicizzazione erronea-  *
      *                  *        mente eseguita                       *
      *                  *---------------------------------------------*
           if        rf-lfd-dva-fin       >    rr-dva-fin
                     go to ctl-val-ric-250
           else      go to ctl-val-ric-900.
       ctl-val-ric-250.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
       ctl-val-ric-251.
      *                      *-----------------------------------------*
      *                      * Editing data di validita' finale di-    *
      *                      * chiarata                                *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rr-dva-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-dfd-edt      .
      *                      *-----------------------------------------*
      *                      * Editing data di validita' finale tro-   *
      *                      * vata                                    *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-lfd-dva-fin       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-dft-edt      .
       ctl-val-ric-252.
      *                      *-----------------------------------------*
      *                      * Composizione messaggio di errore        *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Non e' possibile storicizzare con data "
                                delimited by   size
                     w-sto-lst-dfd-edt
                                delimited by   size
                     ","
                                delimited by   size
                                          into w-err-box-err-msg      .
      *
           move      spaces               to   w-err-box-err-m02      .
           string    "perche' esistono prezzi storicizzati con data "
                                delimited by   size
                     w-sto-lst-dft-edt
                                delimited by   size
                     " !"
                                delimited by   size
                                          into w-err-box-err-m02      .
      *
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * Flag di controllo ad errore             *
      *                      *-----------------------------------------*
           move      "#"                  to   w-ctl-val-ric-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ctl-val-ric-900.
       ctl-val-ric-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ctl-val-ric-999.
       ctl-val-ric-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione della storicizzazione                          *
      *    *-----------------------------------------------------------*
       exe-sub-pgm-000.
      *              *-------------------------------------------------*
      *              * Pre-storicizzazione listini                     *
      *              *-------------------------------------------------*
           perform   pre-sto-lst-000      thru pre-sto-lst-999        .
      *              *-------------------------------------------------*
      *              * Se uscita con errore : ad uscita                *
      *              *-------------------------------------------------*
           if        w-sto-lst-flg-exs    not  = spaces
                     go to exe-sub-pgm-900.
      *              *-------------------------------------------------*
      *              * Storicizzazione dei prezzi di listino del for-  *
      *              * nitore                                          *
      *              *-------------------------------------------------*
           perform   sto-prz-lst-000      thru sto-prz-lst-999        .
      *              *-------------------------------------------------*
      *              * Se uscita con errore : ad uscita                *
      *              *-------------------------------------------------*
           if        w-sto-lst-flg-exs    not  = spaces
                     go to exe-sub-pgm-900.
      *              *-------------------------------------------------*
      *              * Post-storicizzazione listini                    *
      *              *-------------------------------------------------*
           perform   pos-sto-lst-000      thru pos-sto-lst-999        .
      *              *-------------------------------------------------*
      *              * Se uscita con errore : ad uscita                *
      *              *-------------------------------------------------*
           if        w-sto-lst-flg-exs    not  = spaces
                     go to exe-sub-pgm-900.
       exe-sub-pgm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sub-pgm-999.
       exe-sub-pgm-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione procedura pre-storicizzazione listini          *
      *    *-----------------------------------------------------------*
       pre-sto-lst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-sto-lst-flg-exs      .
       pre-sto-lst-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatori di controllo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero records letti e presi in considera-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-ctt-lec      .
      *                  *---------------------------------------------*
      *                  * Numero records scritti ex novo o aggiornati *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-ctt-sen      .
      *                  *---------------------------------------------*
      *                  * Numero records riscritti solo per aggiorna- *
      *                  * mento data finale                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-ctt-apd      .
       pre-sto-lst-200.
      *              *-------------------------------------------------*
      *              * Preparazione area per note operative            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione linee 15..21                  *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      15                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 15                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 16                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                            Programma in esecuzion
      -              "e                             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 18                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero prezzi letti dal listino attuale ..........
      -              "............... :             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 19                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero nuovi prezzi scritti sul listino storico ..
      -              "............... :             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 20                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero prezzi invariati ..........................
      -              "............... :             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-sto-lst-800.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pre-sto-lst-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione procedura post-storicizzazione listini         *
      *    *-----------------------------------------------------------*
       pos-sto-lst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-sto-lst-flg-exs      .
       pos-sto-lst-100.
      *              *-------------------------------------------------*
      *              * Scrittura su rullino messaggi dei risultati     *
      *              * della storicizzazione totale                    *
      *              *-------------------------------------------------*
       pos-sto-lst-110.
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "                    Risultati globali della storic
      -              "izzazione                     "
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pos-sto-lst-115.
      *                  *---------------------------------------------*
      *                  * Linea bianca di separazione                 *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pos-sto-lst-120.
      *                  *---------------------------------------------*
      *                  * Numero prezzi letti dal listino attuale     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-sto-lst-ctt-lec    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-cdl-n9s      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Numero prezzi letti dai listini attuali ..........
      -              "............... : "
                                delimited by   size
                     w-sto-lst-cdl-n9s
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pos-sto-lst-125.
      *                  *---------------------------------------------*
      *                  * Numero prezzi scritti ex novo o aggiornati  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-sto-lst-ctt-sen    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-cdl-n9s      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Numero nuovi prezzi scritti sui listini storici ..
      -              "............... : "
                                delimited by   size
                     w-sto-lst-cdl-n9s
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pos-sto-lst-130.
      *                  *---------------------------------------------*
      *                  * Numero records riscritti solo per aggiorna- *
      *                  * mento data finale                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-sto-lst-ctt-apd    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-cdl-n9s      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Numero prezzi invariati ..........................
      -              "............... : "
                                delimited by   size
                     w-sto-lst-cdl-n9s
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pos-sto-lst-135.
      *                  *---------------------------------------------*
      *                  * Linea bianca di separazione                 *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Lineette di separazione finali              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "*-------------------------------------------------
      -              "-----------------------------*"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea bianca di separazione                 *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pos-sto-lst-200.
      *              *-------------------------------------------------*
      *              * Preparazione area per note operative            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione linee 16..21                  *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      16                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 20                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                                  
      -              " Fine esecuzione programma [ ]"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pos-sto-lst-300.
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      79                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pos-sto-lst-400.
      *              *-------------------------------------------------*
      *              * Cancellazione area per note operative           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione linee 15..21                  *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      15                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pos-sto-lst-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione rullino messaggi                *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      "   STORICIZZAZIONE PREZZI DI LISTINO    "
                                          to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       pos-sto-lst-999.
           exit.

      *    *===========================================================*
      *    * Storicizzazione dei prezzi di listino del fornitore       *
      *    *-----------------------------------------------------------*
       sto-prz-lst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-sto-lst-flg-exs      .
       sto-prz-lst-200.
      *              *-------------------------------------------------*
      *              * Start su prezzi di listino da storicizzare      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "FNPRFM    "         to   f-key                  .
           move      rr-cod-dcf           to   rf-aaf-cod-dcf         .
           move      zero                 to   rf-aaf-tip-mag         .
           move      zero                 to   rf-aaf-num-pro         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a fine storicizzazione        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-prz-lst-900.
       sto-prz-lst-300.
      *              *-------------------------------------------------*
      *              * Read next su prezzi di listino da storicizzare  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Se fine file errata : a fine storicizzazione    *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-prz-lst-900.
       sto-prz-lst-400.
      *              *-------------------------------------------------*
      *              * Test max su listino da storicizzare             *
      *              *-------------------------------------------------*
           if        rf-aaf-cod-dcf       not  = rr-cod-dcf
                     go to sto-prz-lst-900.
       sto-prz-lst-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento archivio storico                  *
      *              *-------------------------------------------------*
       sto-prz-lst-510.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flags di:                   *
      *                  *   - Operazione di scrittura nuovo record    *
      *                  *   - Operazione di cancellazione vecchio re- *
      *                  *     cord                                    *
      *                  * a : Non eseguita                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-sto-lst-flg-put      .
           move      spaces               to   w-sto-lst-flg-del      .
       sto-prz-lst-520.
      *                  *---------------------------------------------*
      *                  * Start per data finale in formato 'reverse'  *
      *                  * relativamente all'elemento in esame         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTFNTDFR "         to   f-key                  .
           move      01                   to   rf-lfd-tip-rec         .
           move      rf-aaf-cod-dcf       to   rf-lfd-cod-dcf         .
           move      rf-aaf-sgl-vlt       to   rf-lfd-sgl-vlt         .
           move      rf-aaf-tip-mag       to   rf-lfd-tip-mag         .
           move      rf-aaf-num-pro       to   rf-lfd-num-mag         .
           move      rf-aaf-fda-pif       to   rf-lfd-fda-pif         .
           move      zero                 to   rf-lfd-dvf-rev         .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad inserimento prezzo     *
      *                  * storico nuovo                               *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-prz-lst-590.
       sto-prz-lst-530.
      *                  *---------------------------------------------*
      *                  * Read next per data finale in formato 're-   *
      *                  * verse', relativamente al codice in esame    *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                  *---------------------------------------------*
      *                  * Se At end : ad inserimento prezzo storico   *
      *                  * nuovo                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-prz-lst-590.
       sto-prz-lst-540.
      *                  *---------------------------------------------*
      *                  * Test max, e se non superato : ad inserimen- *
      *                  * to prezzo storico nuovo                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su tipo record                          *
      *                      *-----------------------------------------*
           if        rf-lfd-tip-rec       not  = 01
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Su codice fornitore                     *
      *                      *-----------------------------------------*
           if        rf-lfd-cod-dcf       not  = rf-aaf-cod-dcf
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Su sigla valuta : riciclo alla lettura  *
      *                      *-----------------------------------------*
           if        rf-lfd-sgl-vlt       not  = rf-aaf-sgl-vlt
                     go to sto-prz-lst-530.
      *                      *-----------------------------------------*
      *                      * Su tipo codice magazzino                *
      *                      *-----------------------------------------*
           if        rf-lfd-tip-mag       not  = rf-aaf-tip-mag
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Su codice magazzino                     *
      *                      *-----------------------------------------*
           if        rf-lfd-num-mag       not  = rf-aaf-num-pro
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Su formato di acquisizione              *
      *                      *-----------------------------------------*
           if        rf-lfd-fda-pif       not  = rf-aaf-fda-pif
                     go to sto-prz-lst-590.
       sto-prz-lst-550.
      *                  *---------------------------------------------*
      *                  * Confronto i dati contenuti nel record sto-  *
      *                  * rico con i dati da inserire, escludendo dal *
      *                  * confronto solamente la data di validita'    *
      *                  * finale ed il prezzo. Se anche un solo valo- *
      *                  * re differisce si esegue sicuramente un in-  *
      *                  * serimento di un prezzo storico nuovo        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo record                             *
      *                      *-----------------------------------------*
           if        rf-lfd-tip-rec       not  = 01
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Codice fornitore                        *
      *                      *-----------------------------------------*
           if        rf-lfd-cod-dcf       not  = rf-aaf-cod-dcf
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Sigla della valuta                      *
      *                      *-----------------------------------------*
           if        rf-lfd-sgl-vlt       not  = rf-aaf-sgl-vlt
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Numero decimali della valuta            *
      *                      *-----------------------------------------*
           if        rf-lfd-dec-vlt       not  = rf-aaf-dec-vlt
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Tipo codice magazzino                   *
      *                      *-----------------------------------------*
           if        rf-lfd-tip-mag       not  = rf-aaf-tip-mag
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Codice numerico magazzino               *
      *                      *-----------------------------------------*
           if        rf-lfd-num-mag       not  = rf-aaf-num-pro
                     go to sto-prz-lst-590.
      *                      *-----------------------------------------*
      *                      * Formato di acquisizione                 *
      *                      *-----------------------------------------*
           if        rf-lfd-fda-pif       not  = rf-aaf-fda-pif
                     go to sto-prz-lst-590.
       sto-prz-lst-560.
      *                  *---------------------------------------------*
      *                  * Determinazione flag di variazione o no di   *
      *                  * prezzi e sconti                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-sto-lst-flg-vrp      .
           move      zero                 to   w-sto-lst-inx-00i      .
       sto-prz-lst-561.
           add       1                    to   w-sto-lst-inx-00i      .
           if        w-sto-lst-inx-00i    >    06
                     go to sto-prz-lst-565.
           if        rf-lfd-qta-pes
                    (w-sto-lst-inx-00i)   not  = rf-aaf-qta-pes
                                                (w-sto-lst-inx-00i)
                     go to sto-prz-lst-564.
           if        rf-lfd-prz-pes
                    (w-sto-lst-inx-00i)   not  = rf-aaf-prz-pes
                                                (w-sto-lst-inx-00i)
                     go to sto-prz-lst-564.
           if        rf-lfd-csr-pes
                    (w-sto-lst-inx-00i)   not  = rf-aaf-csr-pes
                                                (w-sto-lst-inx-00i)
                     go to sto-prz-lst-564.
       sto-prz-lst-562.
           move      zero                 to   w-sto-lst-inx-00j      .
       sto-prz-lst-563.
           add       1                    to   w-sto-lst-inx-00j      .
           if        w-sto-lst-inx-00j    >    05
                     go to sto-prz-lst-561.
           if        rf-lfd-psr-pes
                    (w-sto-lst-inx-00i,
                     w-sto-lst-inx-00j)   not  = rf-aaf-psr-pes
                                                (w-sto-lst-inx-00i,
                                                 w-sto-lst-inx-00j)
                     go to sto-prz-lst-564.
           go to     sto-prz-lst-563.
       sto-prz-lst-564.
           move      "#"                  to   w-sto-lst-flg-vrp      .
       sto-prz-lst-565.
      *                  *---------------------------------------------*
      *                  * Se anche i rimanenti valori:                *
      *                  *  - Prezzo di listino                        *
      *                  *  - Data di validita' finale                 *
      *                  * sono uguali, non e' necessario in assoluto  *
      *                  * alcun aggiornamento.                        *
      *                  *                                             *
      *                  * Nota : Cio' e' possibile solo in caso di    *
      *                  *        una ristoricizzazione a causa di una *
      *                  *        precedente errata storicizzazione.   *
      *                  *---------------------------------------------*
           if        w-sto-lst-flg-vrp    =    spaces     and
                     rf-lfd-dva-fin       =    rr-dva-fin
                     go to sto-prz-lst-660.
      *                  *---------------------------------------------*
      *                  * Se i rimanenti valori:                      *
      *                  *  - Prezzo di listino                        *
      *                  *  - Data di validita' finale                 *
      *                  * sono entrambi variati : si esegue l'inse-   *
      *                  * rimento di un prezzo storico nuovo          *
      *                  *---------------------------------------------*
           if        w-sto-lst-flg-vrp    not  = spaces     and
                     rf-lfd-dva-fin       not  = rr-dva-fin
                     go to sto-prz-lst-590.
      *                  *---------------------------------------------*
      *                  * Se la data di validita' finale e' invaria-  *
      *                  * ta, e quindi l'unico dato cambiato e' il    *
      *                  * prezzo di listino : si esegue un update     *
      *                  *                                             *
      *                  * Nota : Cio' e' possibile solo in caso di    *
      *                  *        una ristoricizzazione a causa di una *
      *                  *        precedente errata storicizzazione.   *
      *                  *---------------------------------------------*
           if        rf-lfd-dva-fin       =    rr-dva-fin
                     go to sto-prz-lst-580.
      *                  *---------------------------------------------*
      *                  * Se l'unico dato variato e' la data di vali- *
      *                  * dita' finale : si esegue un controaggiorna- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           go to     sto-prz-lst-570.
       sto-prz-lst-570.
      *                  *---------------------------------------------*
      *                  * Controaggiornamento                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di tipo aggiornamento : per record *
      *                      * vecchio da sostituire                   *
      *                      *-----------------------------------------*
           move      "V"                  to   w-sto-lst-tip-agg      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sto-prz-lst-600.
       sto-prz-lst-580.
      *                  *---------------------------------------------*
      *                  * Update                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di tipo aggiornamento : per record *
      *                      * vecchio da aggiornare                   *
      *                      *-----------------------------------------*
           move      "U"                  to   w-sto-lst-tip-agg      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sto-prz-lst-600.
       sto-prz-lst-590.
      *                  *---------------------------------------------*
      *                  * Inserimento di un prezzo storico nuovo      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di tipo aggiornamento : per record *
      *                      * nuovo                                   *
      *                      *-----------------------------------------*
           move      "N"                  to   w-sto-lst-tip-agg      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sto-prz-lst-600.
       sto-prz-lst-600.
      *                  *---------------------------------------------*
      *                  * Salvataggio record attuale di [lfd]         *
      *                  *---------------------------------------------*
           move      rf-lfd               to   w-sto-lst-sav-lfd      .
       sto-prz-lst-610.
      *                  *---------------------------------------------*
      *                  * Scrittura del nuovo record                  *
      *                  *---------------------------------------------*
       sto-prz-lst-612.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
       sto-prz-lst-614.
      *                      *-----------------------------------------*
      *                      * Composizione record                     *
      *                      *-----------------------------------------*
           move      01                   to   rf-lfd-tip-rec         .
           move      rf-aaf-cod-dcf       to   rf-lfd-cod-dcf         .
           move      rf-aaf-sgl-vlt       to   rf-lfd-sgl-vlt         .
           move      rf-aaf-dec-vlt       to   rf-lfd-dec-vlt         .
           move      rf-aaf-tip-mag       to   rf-lfd-tip-mag         .
           move      rf-aaf-num-pro       to   rf-lfd-num-mag         .
           move      rf-aaf-fda-pif       to   rf-lfd-fda-pif         .
           move      rf-aaf-qta-pes (1)   to   rf-lfd-qta-pes (1)     .
           move      rf-aaf-prz-pes (1)   to   rf-lfd-prz-pes (1)     .
           move      rf-aaf-csr-pes (1)   to   rf-lfd-csr-pes (1)     .
           move      rf-aaf-psr-pes (1, 1)
                                          to   rf-lfd-psr-pes (1, 1)  .
           move      rf-aaf-psr-pes (1, 2)
                                          to   rf-lfd-psr-pes (1, 2)  .
           move      rf-aaf-psr-pes (1, 3)
                                          to   rf-lfd-psr-pes (1, 3)  .
           move      rf-aaf-psr-pes (1, 4)
                                          to   rf-lfd-psr-pes (1, 4)  .
           move      rf-aaf-psr-pes (1, 5)
                                          to   rf-lfd-psr-pes (1, 5)  .
           move      rf-aaf-qta-pes (2)   to   rf-lfd-qta-pes (2)     .
           move      rf-aaf-prz-pes (2)   to   rf-lfd-prz-pes (2)     .
           move      rf-aaf-csr-pes (2)   to   rf-lfd-csr-pes (2)     .
           move      rf-aaf-psr-pes (2, 1)
                                          to   rf-lfd-psr-pes (2, 1)  .
           move      rf-aaf-psr-pes (2, 2)
                                          to   rf-lfd-psr-pes (2, 2)  .
           move      rf-aaf-psr-pes (2, 3)
                                          to   rf-lfd-psr-pes (2, 3)  .
           move      rf-aaf-psr-pes (2, 4)
                                          to   rf-lfd-psr-pes (2, 4)  .
           move      rf-aaf-psr-pes (2, 5)
                                          to   rf-lfd-psr-pes (2, 5)  .
           move      rf-aaf-qta-pes (3)   to   rf-lfd-qta-pes (3)     .
           move      rf-aaf-prz-pes (3)   to   rf-lfd-prz-pes (3)     .
           move      rf-aaf-csr-pes (3)   to   rf-lfd-csr-pes (3)     .
           move      rf-aaf-psr-pes (3, 1)
                                          to   rf-lfd-psr-pes (3, 1)  .
           move      rf-aaf-psr-pes (3, 2)
                                          to   rf-lfd-psr-pes (3, 2)  .
           move      rf-aaf-psr-pes (3, 3)
                                          to   rf-lfd-psr-pes (3, 3)  .
           move      rf-aaf-psr-pes (3, 4)
                                          to   rf-lfd-psr-pes (3, 4)  .
           move      rf-aaf-psr-pes (3, 5)
                                          to   rf-lfd-psr-pes (3, 5)  .
           move      rf-aaf-qta-pes (4)   to   rf-lfd-qta-pes (4)     .
           move      rf-aaf-prz-pes (4)   to   rf-lfd-prz-pes (4)     .
           move      rf-aaf-csr-pes (4)   to   rf-lfd-csr-pes (4)     .
           move      rf-aaf-psr-pes (4, 1)
                                          to   rf-lfd-psr-pes (4, 1)  .
           move      rf-aaf-psr-pes (4, 2)
                                          to   rf-lfd-psr-pes (4, 2)  .
           move      rf-aaf-psr-pes (4, 3)
                                          to   rf-lfd-psr-pes (4, 3)  .
           move      rf-aaf-psr-pes (4, 4)
                                          to   rf-lfd-psr-pes (4, 4)  .
           move      rf-aaf-psr-pes (4, 5)
                                          to   rf-lfd-psr-pes (4, 5)  .
           move      rf-aaf-qta-pes (5)   to   rf-lfd-qta-pes (5)     .
           move      rf-aaf-prz-pes (5)   to   rf-lfd-prz-pes (5)     .
           move      rf-aaf-csr-pes (5)   to   rf-lfd-csr-pes (5)     .
           move      rf-aaf-psr-pes (5, 1)
                                          to   rf-lfd-psr-pes (5, 1)  .
           move      rf-aaf-psr-pes (5, 2)
                                          to   rf-lfd-psr-pes (5, 2)  .
           move      rf-aaf-psr-pes (5, 3)
                                          to   rf-lfd-psr-pes (5, 3)  .
           move      rf-aaf-psr-pes (5, 4)
                                          to   rf-lfd-psr-pes (5, 4)  .
           move      rf-aaf-psr-pes (5, 5)
                                          to   rf-lfd-psr-pes (5, 5)  .
           move      rf-aaf-qta-pes (6)   to   rf-lfd-qta-pes (6)     .
           move      rf-aaf-prz-pes (6)   to   rf-lfd-prz-pes (6)     .
           move      rf-aaf-csr-pes (6)   to   rf-lfd-csr-pes (6)     .
           move      rf-aaf-psr-pes (6, 1)
                                          to   rf-lfd-psr-pes (6, 1)  .
           move      rf-aaf-psr-pes (6, 2)
                                          to   rf-lfd-psr-pes (6, 2)  .
           move      rf-aaf-psr-pes (6, 3)
                                          to   rf-lfd-psr-pes (6, 3)  .
           move      rf-aaf-psr-pes (6, 4)
                                          to   rf-lfd-psr-pes (6, 4)  .
           move      rf-aaf-psr-pes (6, 5)
                                          to   rf-lfd-psr-pes (6, 5)  .
           move      zero                 to   rf-lfd-dva-ini         .
           move      rr-dva-fin           to   rf-lfd-dva-fin         .
           move      rr-dvf-rev           to   rf-lfd-dvf-rev         .
           move      spaces               to   rf-lfd-alx-exp         .
       sto-prz-lst-616.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di ag-    *
      *                      * giornamento da eseguire                 *
      *                      *-----------------------------------------*
           if        w-sto-lst-tip-agg    =    "U"
                     go to sto-prz-lst-620.
       sto-prz-lst-618.
      *                      *-----------------------------------------*
      *                      * Se tipo aggiornamento:                  *
      *                      *                                         *
      *                      *  - Inserimento record nuovo             *
      *                      *  - Record vecchio da sostituire         *
      *                      *                                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scrittura record                    *
      *                          *-------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                          *-------------------------------------*
      *                          * Set del flag sull'esito dell'opera- *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "E"            to   w-sto-lst-flg-put
           else      move  "S"            to   w-sto-lst-flg-put      .
      *                          *-------------------------------------*
      *                          * Se errore in scrittura : si ignora  *
      *                          * il codice in esame e si ricicla per *
      *                          * il codice successivo                *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-prz-lst-660
           else      go to sto-prz-lst-630.
       sto-prz-lst-620.
      *                      *-----------------------------------------*
      *                      * Se tipo aggiornamento:                  *
      *                      *                                         *
      *                      *  - Record vecchio da aggiornare         *
      *                      *                                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scrittura forzata del record        *
      *                          *-------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                          *-------------------------------------*
      *                          * Set del flag sull'esito dell'opera- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "S"                  to   w-sto-lst-flg-put      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     sto-prz-lst-630.
       sto-prz-lst-630.
      *                  *---------------------------------------------*
      *                  * Cancellazione del vecchio record            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il flag di tipo aggiornamento e' per *
      *                      *                                         *
      *                      *  - Inserimento record nuovo             *
      *                      *  - Record vecchio da aggiornare         *
      *                      *                                         *
      *                      * nessuna cancellazione                   *
      *                      *-----------------------------------------*
           if        w-sto-lst-tip-agg    =    "N" or
                     w-sto-lst-tip-agg    =    "U"
                     go to sto-prz-lst-660.
      *                      *-----------------------------------------*
      *                      * Ripristino area record [lfd] da area di *
      *                      * salvataggio                             *
      *                      *-----------------------------------------*
           move      w-sto-lst-sav-lfd    to   rf-lfd                 .
      *                      *-----------------------------------------*
      *                      * Cancellazione record                    *
      *                      *-----------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/ioflfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lfd                 .
      *                      *-----------------------------------------*
      *                      * Set del flag sull'esito dell'operazione *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "E"            to   w-sto-lst-flg-del
           else      move  "S"            to   w-sto-lst-flg-del      .
       sto-prz-lst-660.
      *              *-------------------------------------------------*
      *              * Aggiornamento area per note operative           *
      *              *-------------------------------------------------*
       sto-prz-lst-665.
      *                  *---------------------------------------------*
      *                  * Numero records letti e presi in considera-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento                              *
      *                      *-----------------------------------------*
           add       1                    to   w-sto-lst-ctt-lec      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-sto-lst-ctt-lec    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-prz-lst-670.
      *                  *---------------------------------------------*
      *                  * Numero records scritti ex novo o aggiornati *
      *                  *---------------------------------------------*
       sto-prz-lst-671.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-sto-lst-flg-put    not  = "S" or
                     w-sto-lst-flg-del    =    "S"   or
                     w-sto-lst-flg-del    =    "E"
                     go to sto-prz-lst-675.
       sto-prz-lst-672.
      *                      *-----------------------------------------*
      *                      * Incremento                              *
      *                      *-----------------------------------------*
           add       1                    to   w-sto-lst-ctt-sen      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-sto-lst-ctt-sen    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-prz-lst-675.
      *                  *---------------------------------------------*
      *                  * Numero records riscritti solo per aggiorna- *
      *                  * mento data finale                           *
      *                  *---------------------------------------------*
       sto-prz-lst-676.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-sto-lst-flg-put    =    spaces and
                     w-sto-lst-flg-del    =    spaces
                     go to sto-prz-lst-677.
           if        w-sto-lst-flg-put    not  = "S" or
                     w-sto-lst-flg-del    not  = "S"
                     go to sto-prz-lst-680.
       sto-prz-lst-677.
      *                      *-----------------------------------------*
      *                      * Incremento                              *
      *                      *-----------------------------------------*
           add       1                    to   w-sto-lst-ctt-apd      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      20                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-sto-lst-ctt-apd    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-prz-lst-680.
      *                  *---------------------------------------------*
      *                  * Fine aggiornamento note operative           *
      *                  *---------------------------------------------*
           go to     sto-prz-lst-700.
       sto-prz-lst-700.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     sto-prz-lst-300.
       sto-prz-lst-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     sto-prz-lst-999.
       sto-prz-lst-999.
           exit.
