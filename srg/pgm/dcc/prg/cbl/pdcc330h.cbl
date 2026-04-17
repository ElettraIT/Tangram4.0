       Identification Division.
       Program-Id.                                 pdcc330h           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    lst                 *
      *                                   Fase:    dcc330              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 08/11/02    *
      *                       Ultima revisione:    NdK del 31/05/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni automatiche su prezzi clienti    *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Storicizzazione prezzi netti concordati di  *
      *                    uno o piu' clienti                          *
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
                     "STORICIZZAZIONE PREZZI NETTI CONCORDATI "       .

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
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

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
      *        * Tipo di storicizzazione                               *
      *        *                                                       *
      *        *  - 01 : Per un solo cliente                             *
      *        *  - 02 : Per tutti i clienti                             *
      *        *  - 03 : Per i clienti di un agente                      *
      *        *  - 04 : Per un selezione di clienti                     *
      *        *-------------------------------------------------------*
           05  rr-tip-sto                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice listino                                        *
      *        *                                                       *
      *        * Forzato a spaces                                      *
      *        *-------------------------------------------------------*
           05  rr-cod-lst                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente                                         *
      *        *-------------------------------------------------------*
           05  rr-cod-age                 pic  9(07)                  .
           05  rr-cod-age-nom             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente da storicizzare                        *
      *        *-------------------------------------------------------*
           05  rr-cod-cli                 pic  9(07)                  .
           05  rr-cod-cli-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Si/no elementi selezionati per il codice cliente      *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-sns             pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero elementi selezionati per il codice cliente     *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-els             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Elenco elementi selezionati per il codice cliente     *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-edd.
               10  rr-cod-cli-ele  occurs 36
                                  indexed by   rr-cod-cli-inx         .
                   15  rr-cod-cli-eco     pic  9(07)                  .
                   15  rr-cod-cli-ers     pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Data di validita' finale del listino, o dei listini,  *
      *        * da storicizzare                                       *
      *        *                                                       *
      *        * Non puo' essere a zero                                *
      *        *-------------------------------------------------------*
           05  rr-dva-fin                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di validita' finale del listino, o dei listini,  *
      *        * da storicizzare, in formato 'reverse'                 *
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
      *    * Work-area per comodi di accettazione                      *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Area per impostazione serie elementi da selezionare   *
      *        *-------------------------------------------------------*
           05  w-acc-ser-edd.
               10  w-acc-ser-edd-max      pic  9(02) value 36         .
               10  w-acc-ser-edd-pev      pic  9(02)                  .
               10  w-acc-ser-edd-nel      pic  9(02)                  .
               10  w-acc-ser-edd-n1v      pic  9(02)                  .
               10  w-acc-ser-edd-nev      pic  9(02)                  .
               10  w-acc-ser-edd-nec      pic  9(02)                  .
               10  w-acc-ser-edd-fce      pic  x(01)                  .
               10  w-acc-ser-edd-led.
                   15  w-acc-ser-edd-rig  pic  x(03)                  .
                   15  w-acc-ser-edd-dpu  pic  x(01)                  .
                   15  filler             pic  x(02)                  .
                   15  w-acc-ser-edd-cod  pic  x(07)                  .
                   15  filler             pic  x(05)                  .
                   15  w-acc-ser-edd-des  pic  x(40)                  .
                   15  filler             pic  x(18)                  .
               10  w-acc-ser-edd-c01      pic  9(02)                  .
               10  w-acc-ser-edd-c02      pic  9(02)                  .
               10  w-acc-ser-edd-c0a      pic  9(02)                  .
               10  w-acc-ser-edd-c0b      pic  9(02)                  .
               10  w-acc-ser-edd-c0c      pic  9(02)                  .
               10  w-acc-ser-edd-c0p      pic  9(02)                  .
               10  w-acc-ser-edd-c0q      pic  9(02)                  .
               10  w-acc-ser-edd-c0r      pic  9(02)                  .
               10  w-acc-ser-edd-spe      pic  9(07)                  .
               10  w-acc-ser-edd-svk      pic  x(04)                  .
               10  w-acc-ser-edd-stu      pic  x(01)                  .
               10  w-acc-ser-edd-fcl.
                   15  filler             pic  x(08)                  .
                   15  w-acc-ser-edd-070  pic  x(70)                  .
                   15  filler             pic  x(02)                  .
               10  w-acc-ser-edd-txt.
                   15  w-acc-ser-edd-rtr  occurs 03
                                          pic  x(40)                  .
               10  w-acc-ser-edd-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-acc-ser-edd-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-acc-ser-edd-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .
               10  w-let-arc-age-rag      pic  x(40)                  .
               10  w-let-arc-age-via      pic  x(40)                  .
               10  w-let-arc-age-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-tus      pic  9(02)                  .
               10  w-let-arc-dcc-tud      pic  9(07)                  .
               10  w-let-arc-dcc-tuc      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Tipo di storicizzazione             *
      *        *-------------------------------------------------------*
           05  w-sav-tip-sto              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per accettazione codice cliente           *
      *        *-------------------------------------------------------*
           05  w-sav-acc-cod-cli          pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Exp                               *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di storicizzazione                    *
      *        *-------------------------------------------------------*
           05  w-exp-tip-sto.
               10  w-exp-tip-sto-num      pic  9(02)       value 4    .
               10  w-exp-tip-sto-lun      pic  9(02)       value 40   .
               10  w-exp-tip-sto-tbl.
                   15  filler             pic  x(40) value
                       "per Un solo cliente                     "     .
                   15  filler             pic  x(40) value
                       "per Tutti i clienti                     "     .
                   15  filler             pic  x(40) value
                       "per i clienti di un Agente              "     .
                   15  filler             pic  x(40) value
                       "per una Selezione di clienti            "     .

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
      *        * Indice in tabella listini del codice listino in fase  *
      *        * di storicizzazione                                    *
      *        *-------------------------------------------------------*
           05  w-sto-lst-inx-att          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero listino in fase di storicizzazione             *
      *        *-------------------------------------------------------*
           05  w-sto-lst-num-att          pic  9(03)                  .
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
      *        * Comodo per editing codice cliente                     *
      *        *-------------------------------------------------------*
           05  w-sto-pnc-cli-edt          pic  x(07)                  .
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
      *        * Contatori per l'esecuzione della storicizzazione di   *
      *        * un singolo listino                                    *
      *        *-------------------------------------------------------*
           05  w-sto-lst-ctr-exe.
      *            *---------------------------------------------------*
      *            * Numero records letti e presi in considerazione    *
      *            *---------------------------------------------------*
               10  w-sto-lst-ctr-lec      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Numero records scritti ex novo o aggiornati       *
      *            *---------------------------------------------------*
               10  w-sto-lst-ctr-sen      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Numero records riscritti solo per aggiornamento   *
      *            * data finale                                       *
      *            *---------------------------------------------------*
               10  w-sto-lst-ctr-apd      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Sub-area per esecuzione aggiornamento archivio sto-   *
      *        * rico listini                                          *
      *        *-------------------------------------------------------*
           05  w-sto-lst-are-agg.
      *            *---------------------------------------------------*
      *            * Tipo record                                       *
      *            *---------------------------------------------------*
               10  w-sto-lst-tip-rec      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice listino                                    *
      *            *---------------------------------------------------*
               10  w-sto-lst-cod-lst      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-sto-lst-cod-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Sigla della valuta                                *
      *            *---------------------------------------------------*
               10  w-sto-lst-sgl-vlt      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali della valuta                      *
      *            *---------------------------------------------------*
               10  w-sto-lst-dec-vlt      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto numerico                          *
      *            *---------------------------------------------------*
               10  w-sto-lst-num-pro      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Prezzo                                            *
      *            *---------------------------------------------------*
               10  w-sto-lst-prz-lst      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * % di provvigione associate direttamente al listi- *
      *            * no                                                *
      *            *---------------------------------------------------*
               10  w-sto-lst-per-pvg
                               occurs 03  pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * % di sconto associate direttamente al listino     *
      *            *---------------------------------------------------*
               10  w-sto-lst-per-sco
                               occurs 05  pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * Si/no utilizzo del prezzo                         *
      *            *---------------------------------------------------*
               10  w-sto-lst-snx-prz      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no utilizzo delle % di provvigione             *
      *            *---------------------------------------------------*
               10  w-sto-lst-snx-pvg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no utilizzo delle % di sconto                  *
      *            *---------------------------------------------------*
               10  w-sto-lst-snx-sco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Data di validita' iniziale                        *
      *            *---------------------------------------------------*
               10  w-sto-lst-dva-ini      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data di validita' finale                          *
      *            *---------------------------------------------------*
               10  w-sto-lst-dva-fin      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data di validita' finale in formato 'reverse'     *
      *            *---------------------------------------------------*
               10  w-sto-lst-dvf-rev      pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Area libera per espansioni speciali               *
      *            *---------------------------------------------------*
               10  w-sto-lst-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .
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
      *        * Salvataggio record di [lsd]                           *
      *        *-------------------------------------------------------*
           05  w-sto-lst-sav-lsd.
               10  filler  occurs 1024    pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

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
      *    * del tipo di manutenzione                                  *
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
      *    * Work-area per tipi operazione                             *
      *    *-----------------------------------------------------------*
       01  w-top.
      *        *-------------------------------------------------------*
      *        * Tabella tipi operazione e dati ad essi associati      *
      *        *-------------------------------------------------------*
           05  w-top-tbl-top.
      *            *---------------------------------------------------*
      *            * Indice per puntamento su elemento in tabella ed   *
      *            * altri indici di comodo                            *
      *            *---------------------------------------------------*
               10  w-top-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per caricamento iniziale elementi          *
      *            *---------------------------------------------------*
               10  w-top-ele-wci.
                   15  w-top-ele-wci-des  pic  x(50)                  .
                   15  filler             pic  x(01)                  .
                   15  w-top-ele-wci-alf  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-top-ele-wci-ovy  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-top-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi per pagina                     *
      *            *---------------------------------------------------*
               10  w-top-ele-nep          pic  9(03)       value 14   .
      *            *---------------------------------------------------*
      *            * Numero di pagine totali                           *
      *            *---------------------------------------------------*
               10  w-top-ele-npt          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina attualmente visualizzata            *
      *            *---------------------------------------------------*
               10  w-top-ele-pag          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-top-ele-max          pic  9(03)       value 50   .
      *            *---------------------------------------------------*
      *            * Elementi per tipi operazione                      *
      *            *---------------------------------------------------*
               10  w-top-ele-top occurs 50.
      *                *-----------------------------------------------*
      *                * Codice numerico tipo operazione               *
      *                *-----------------------------------------------*
                   15  w-top-num-top      pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo operazione            *
      *                *-----------------------------------------------*
                   15  w-top-alf-top      pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo operazione                   *
      *                *-----------------------------------------------*
                   15  w-top-des-top      pic  x(50)                  .
      *                *-----------------------------------------------*
      *                * Overlay da richiamare per il tipo di opera-   *
      *                * zione                                         *
      *                *-----------------------------------------------*
                   15  w-top-ovy-top      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per sottoprogrammi attivi della fase            *
      *    *-----------------------------------------------------------*
       01  w-spg.
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, codice   *
      *        * alfanumerico del tipo di operazione                   *
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
               10  w-spg-ele-max          pic  9(03)       value 99   .
      *            *---------------------------------------------------*
      *            * Elementi per sottoprogrammi attivi                *
      *            *---------------------------------------------------*
               10  w-spg-ele-spg occurs 99.
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo operazione            *
      *                *-----------------------------------------------*
                   15  w-spg-alf-top      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  filler                     pic  x(01)                  .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-ovy-exe
                                               w-top
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
           move      "dcc330"             to   v-alf                  .
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
      *              * [lst]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * [lsd]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
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
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [lst]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * [lsd]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
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
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente commer-*
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
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
      *                  * Tipo di storicizzazione                     *
      *                  *---------------------------------------------*
           perform   acc-tip-sto-000      thru acc-tip-sto-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Data di validita' finale del listino da     *
      *                  * storicizzare                                *
      *                  *---------------------------------------------*
           perform   acc-dva-fin-000      thru acc-dva-fin-999        .
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
      *              * Tipo di storicizzazione                         *
      *              *-------------------------------------------------*
           perform   pmt-tip-sto-000      thru pmt-tip-sto-999        .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Data di validita' finale del listino da stori-  *
      *              * cizzare                                         *
      *              *-------------------------------------------------*
           perform   pmt-dva-fin-000      thru pmt-dva-fin-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per : Tipo di storicizzazione     *
      *    *-----------------------------------------------------------*
       pmt-tip-sto-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di storicizzazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-sto-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per : Codice agente               *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per : Codice cliente              *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per : Listino da storicizzare     *
      *    *-----------------------------------------------------------*
       pmt-dva-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data fino alla quale sono  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      rimasti in vigore i   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  prezzi netti concordati   "
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
      *    * Accettazione : Tipo di storicizzazione                    *
      *    *-----------------------------------------------------------*
       acc-tip-sto-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-sto           to   w-sav-tip-sto          .
       acc-tip-sto-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sto-lun    to   v-car                  .
           move      w-exp-tip-sto-num    to   v-ldt                  .
           move      "UTAS#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-sto-tbl    to   v-txt                  .
           move      rr-tip-sto           to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-sto-999.
       acc-tip-sto-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-sto             .
       acc-tip-sto-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-sto-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se variato il tipo storicizzazione     *
      *                  *---------------------------------------------*
           if        rr-tip-sto           =    w-sav-tip-sto
                     go to acc-tip-sto-800.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo storicizza- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           go to     acc-tip-sto-610
                     acc-tip-sto-620
                     acc-tip-sto-630
                     acc-tip-sto-640
                     depending            on   rr-tip-sto             .
       acc-tip-sto-610.
      *                  *---------------------------------------------*
      *                  * Un solo cliente                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice e nominativo     *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e nominativo     *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-sto-800.
       acc-tip-sto-620.
      *                  *---------------------------------------------*
      *                  * Tutti i clienti                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice e nominativo     *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e nominativo     *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice e ragione socia- *
      *                      * le cliente                              *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e ragione socia- *
      *                      * le cliente                              *
      *                      *-----------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-sto-800.
       acc-tip-sto-630.
      *                  *---------------------------------------------*
      *                  * I clienti di un agente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice e ragione socia- *
      *                      * le cliente                              *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e ragione socia- *
      *                      * le cliente                              *
      *                      *-----------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-sto-800.
       acc-tip-sto-640.
      *                  *---------------------------------------------*
      *                  * Una selezione di clienti                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice e nominativo     *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e nominativo     *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-sto-800.
       acc-tip-sto-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-sto-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-sto-100.
       acc-tip-sto-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo di storicizzazione                 *
      *    *-----------------------------------------------------------*
       vis-tip-sto-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sto-lun    to   v-car                  .
           move      w-exp-tip-sto-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-sto-tbl    to   v-txt                  .
           move      rr-tip-sto           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-sto-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente                              *
      *    *-----------------------------------------------------------*
       acc-cod-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-sto           not  = 03
                     go to acc-cod-age-999.
       acc-cod-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      rr-cod-age           to   w-cod-mne-age-cod      .
           move      07                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      07                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-cod-age-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-cod-age-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-cod-age-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-age-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-cod-age-110.
       acc-cod-age-120.
           move      w-cod-mne-age-cod    to   v-num                  .
       acc-cod-age-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-age-999.
       acc-cod-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-age             .
       acc-cod-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo storicizzazione                *
      *                  *---------------------------------------------*
           if        rr-tip-sto           =    03   and
                     rr-cod-age           =    zero
                     go to acc-cod-age-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [age]                      *
      *                  *---------------------------------------------*
           move      rr-cod-age           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     move  "Tutti gli agenti                        "
                                          to   rr-cod-age-nom
           else      move  w-let-arc-age-nom
                                          to   rr-cod-age-nom         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo agente           *
      *                  *---------------------------------------------*
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-cod-age-100.
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-age-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-age-100.
       acc-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice agente                           *
      *    *-----------------------------------------------------------*
       vis-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Nominativo agente                       *
      *    *-----------------------------------------------------------*
       vis-cod-age-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-age-nom       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice cliente                             *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-sto           not  = 01 and
                     rr-tip-sto           not  = 04
                     go to acc-cod-cli-999.
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Test se note operative da emettere              *
      *              *-------------------------------------------------*
           if        rr-tip-sto           not  = 04
                     go to acc-cod-cli-105.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione SELEZIONE per scegliere piu' client
      -              "i (fino ad un massimo di 36)  "
                                          to   v-nt1                  .
                                          
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-105.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli           to   w-cod-mne-dcc-cod      .
           move      09                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      09                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        rr-tip-sto           =    04
                     move  "SLCT"         to   v-pfk (11)             .
      *
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
       acc-cod-cli-150.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
       acc-cod-cli-300.
      *              *-------------------------------------------------*
      *              * Se 'Select'                                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-cli-400.
      *                  *---------------------------------------------*
      *                  * Select su codici cliente                    *
      *                  *---------------------------------------------*
           perform   acc-cli-esl-000      thru acc-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Verifica se elementi selezionati per atti-  *
      *                  * vazione del segnale di elementi selezionati *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di selezione    *
      *                      * effettuata                              *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-cli-sns         .
           move      zero                 to   w-acc-ser-edd-c01      .
       acc-cod-cli-310.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to acc-cod-cli-320.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to acc-cod-cli-320.
      *                      *-----------------------------------------*
      *                      * Attivazione segnale di selezione effet- *
      *                      * tuata                                   *
      *                      *-----------------------------------------*
           move      1                    to   rr-cod-cli-sns         .
       acc-cod-cli-320.
      *                  *---------------------------------------------*
      *                  * Normalizzazione singolo codice cliente      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
      *                  *---------------------------------------------*
      *                  * Preparazione visualizzazione elementi sele- *
      *                  * zionati                                     *
      *                  *---------------------------------------------*
           perform   pcs-cli-esl-000      thru pcs-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codici selezionati          *
      *                  *---------------------------------------------*
           perform   vcs-cli-esl-000      thru vcs-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo storicizzazione                *
      *                  *---------------------------------------------*
           if        rr-tip-sto           =    01   and
                     rr-cod-cli           =    zero
                     go to acc-cod-cli-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcc]                      *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     move  "Tutti i clienti                         "
                                          to   rr-cod-cli-rag
           else      move  w-let-arc-dcc-rag
                                          to   rr-cod-cli-rag         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale completa    *
      *                  *---------------------------------------------*
           if        rr-cod-cli-sns       not  = zero
                     go to acc-cod-cli-420.
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
       acc-cod-cli-420.
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to acc-cod-cli-100.
       acc-cod-cli-500.
      *                  *---------------------------------------------*
      *                  * Test su status commerciale del cliente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tutti i clienti : oltre              *
      *                      *-----------------------------------------*
           if        rr-cod-cli           =    zero
                     go to acc-cod-cli-600.
      *                      *-----------------------------------------*
      *                      * Se status normale : oltre               *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-tus    =    01
                     go to acc-cod-cli-600.
      *                      *-----------------------------------------*
      *                      * Se status non normale : box specifico   *
      *                      * per lo status commerciale               *
      *                      *-----------------------------------------*
           perform   acc-cod-cli-bsc-000  thru acc-cod-cli-bsc-999    .
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
      *    * Accettazione : Codice cliente                             *
      *    *                                                           *
      *    * Box di errore emesso in funzione dello status commerciale *
      *    * del cliente, se diverso da 'normale'                      *
      *    *-----------------------------------------------------------*
       acc-cod-cli-bsc-000.
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio del video attuale               *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      20                   to   v-lto                  .
           move      08                   to   v-pos                  .
           move      73                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-bsc-200.
      *                  *---------------------------------------------*
      *                  * Messaggi entro il box                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo centrale                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "                     A T T E N Z I O N E          
      -              "            "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riga messaggio 1                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Per il cliente impostato si rileva la seguente con
      -              "dizione :   "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione riga messaggio 2           *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-tus    =    11
                     move  "[Esauriti i rapporti commerciali]           
      -                    "                  "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    21
                     move  "[Sostituito da ns. nuovo cliente]           
      -                    "                  "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    51
                     move  "[Cessata attivita']                         
      -                    "                  "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    52
                     move  "[Cessata attivita', ma sostituito da ns. nuo
      -                    "vo cliente]       "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    61
                     move  "[In contenzioso]                            
      -                    "                  "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    62
                     move  "[In contenzioso, ma sostituito da ns. nuovo 
      -                    "cliente]          "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    71
                     move  "[Fallito]                                   
      -                    "                  "
                                          to   v-alf
           else if   w-let-arc-dcc-tus    =    72
                     move  "[Fallito, ma sostituito da ns. nuovo cliente
      -                    "]                 "
                                          to   v-alf
           else      move  "(status non determinato)                    
      -                    "                  "
                                          to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Riga messaggio 2                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      10                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riga messaggio 3                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da emettere                 *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-tud    =    zero
                     go to acc-cod-cli-bsc-300.
      *                          *-------------------------------------*
      *                          * Emissione                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "La determinazione di tale condizione risale al :  
      -              "            "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Data di rilevamento status          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      59                   to   v-pos                  .
           move      w-let-arc-dcc-tud    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-bsc-300.
      *                      *-----------------------------------------*
      *                      * Righe messaggio 4 e 5                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da emettere                 *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-tuc    =    zero
                     go to acc-cod-cli-bsc-600.
      *                          *-------------------------------------*
      *                          * Emissione                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Il cliente utilizzabile in alternativa e' il segue
      -              "nte :       "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Codice cliente alternativo          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      w-let-arc-dcc-tuc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Lettura descrizione per il cliente  *
      *                          * alternativo                         *
      *                          *-------------------------------------*
           move      w-let-arc-dcc-tuc    to   w-let-arc-dcc-cod      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           if        w-let-arc-dcc-flg    not  = spaces
                     move  all "."        to   v-alf
           else      move  w-let-arc-dcc-rag
                                          to   v-alf                  .
      *                          *-------------------------------------*
      *                          * Emissione                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      20                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-bsc-600.
      *                      *-----------------------------------------*
      *                      * Prompt per presa visione                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-bsc-800.
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
           move      19                   to   v-lin                  .
           move      70                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-bsc-999.
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
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-cli           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Ragione sociale cliente                 *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Serie elementi da selezionare              *
      *    *-----------------------------------------------------------*
       acc-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titoli all'interno del box                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Codici cliente da seleziona
      -              "re                        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titoli                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura inferiore titoli             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero 1                              *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-n1v      .
           perform   acc-cli-esl-900      thru acc-cli-esl-909        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Primo elemento visualizzato : 1             *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-pev      .
      *                  *---------------------------------------------*
      *                  * Numero elemento in accettazione : 1         *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-cli-esl-100.
       acc-cli-esl-080.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina se necessario            *
      *              *-------------------------------------------------*
       acc-cli-esl-082.
           move      w-acc-ser-edd-pev    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       11                   to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-nel    <    w-acc-ser-edd-c0a or
                     w-acc-ser-edd-nel    >    w-acc-ser-edd-c0b
                     go to acc-cli-esl-084
           else      go to acc-cli-esl-086.
       acc-cli-esl-084.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-n1v      .
           subtract  1                    from w-acc-ser-edd-n1v      .
           divide    12                   into w-acc-ser-edd-n1v      .
           multiply  12                   by   w-acc-ser-edd-n1v      .
           add       1                    to   w-acc-ser-edd-n1v      .
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-pev      .
           perform   acc-cli-esl-900      thru acc-cli-esl-909        .
       acc-cli-esl-086.
           go to     acc-cli-esl-100.
       acc-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
       acc-cli-esl-120.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero di pagina                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina in corso di    *
      *                  * trattamento                                 *
      *                  *---------------------------------------------*
           if        w-acc-ser-edd-nel    >    24
                     move  3              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    12
                     move  2              to   w-acc-ser-edd-lt1
           else      move  1              to   w-acc-ser-edd-lt1      .
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina totale         *
      *                  *---------------------------------------------*
           if        rr-cod-cli-els       >    24
                     move  3              to   w-acc-ser-edd-lt2
           else if   rr-cod-cli-els       >    12
                     move  2              to   w-acc-ser-edd-lt2
           else      move  1              to   w-acc-ser-edd-lt2      .
      *
           move      3                    to   w-acc-ser-edd-lt2      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-ser-edd-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cli-esl-150.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti linea         *
      *                  *---------------------------------------------*
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-acc-ser-edd-spe      .
       acc-cli-esl-200.
      *                  *---------------------------------------------*
      *                  * Accettazione codice elemento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri                  *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-cod-mne-dcc-cod      .
           move      "<B"                 to   v-edm                  .
      *                          *-------------------------------------*
      *                          * Coordinate di posizionamento        *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           move      w-acc-ser-edd-c0r    to   w-cod-mne-dcc-lin      .
           move      09                   to   w-cod-mne-dcc-pos      .
           move      w-cod-mne-dcc-lin    to   w-cod-mne-dcc-rln      .
           move      19                   to   w-cod-mne-dcc-rps      .
      *                          *-------------------------------------*
      *                          * Tasto 'Up'                          *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Down'                        *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Find'                        *
      *                          *-------------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Insr' : disattivato          *
      *                          *-------------------------------------*
           move      spaces               to   v-pfk (04)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Do'                          *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Remove'                      *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   not  = zero
                     go to acc-cli-esl-202.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-cli-esl-204.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-204.
       acc-cli-esl-202.
           move      "REMV"               to   v-pfk (06)             .
       acc-cli-esl-204.
      *                          *-------------------------------------*
      *                          * Tasto 'Previous screen'             *
      *                          *-------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Next screen'                 *
      *                          *-------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Back'                        *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    >    1
                     move  "BACK"         to   v-pfk (09)             .
           if        w-acc-ser-edd-nel    =    w-acc-ser-edd-max
                     go to acc-cli-esl-206.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero and
                     rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-206.
      *                          *-------------------------------------*
      *                          * Tasto 'Tab'                         *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
       acc-cli-esl-206.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cli-esl-208.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cli-esl-210.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cli-esl-212.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cli-esl-210.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cli-esl-208.
       acc-cli-esl-212.
           move      w-cod-mne-dcc-cod    to   v-num                  .
       acc-cli-esl-215.
      *                      *-----------------------------------------*
      *                      * Valore in campo di destinazione         *
      *                      *-----------------------------------------*
           move      v-num                to   rr-cod-cli-eco
                                              (w-acc-ser-edd-nel)     .
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-cli-esl-800.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to acc-cli-esl-425.
       acc-cli-esl-220.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cli-esl-225.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sul primo : uscita               *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    =    zero
                     move  "UP  "         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-225.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-cli-esl-250.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sull'ultimo : uscita             *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-fce    =    spaces
                     add   1              to   w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
           if        w-acc-ser-edd-nel    =    1
                     go to acc-cli-esl-230
           else      go to acc-cli-esl-235.
       acc-cli-esl-230.
           if        rr-cod-cli-eco (1)    =    zero and
                     rr-cod-cli-eco (2)    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-235.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero and
                     rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-250.
      *                      *-----------------------------------------*
      *                      * Se Insr                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-cli-esl-275.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-940      thru acc-cli-esl-949        .
           go to     acc-cli-esl-080.
       acc-cli-esl-275.
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cli-esl-300.
      *                          *-------------------------------------*
      *                          * Controlli per tasto Do              *
      *                          *-------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     go to acc-cli-esl-280
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-cli-esl-200.
       acc-cli-esl-280.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-acc-ric-sel      .
           go to     acc-cli-esl-800.
       acc-cli-esl-300.
      *                      *-----------------------------------------*
      *                      * Se Remv                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-cli-esl-325.
      *                          *-------------------------------------*
      *                          * Compattamento in ogni caso          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-930      thru acc-cli-esl-939        .
      *                          *-------------------------------------*
      *                          * A reimpostare                       *
      *                          *-------------------------------------*
           go to     acc-cli-esl-080.
       acc-cli-esl-325.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cli-esl-350.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su prima facciata : uscita       *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    not  > 14
                     move  "UP  "         to   v-key
                     go to acc-cli-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * precedente                          *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           divide    14                   into w-acc-ser-edd-nel      .
           subtract  1                    from w-acc-ser-edd-nel      .
           multiply  14                   by   w-acc-ser-edd-nel      .
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-350.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-cli-esl-375.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su ultima facciata : uscita      *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
           divide    12                   into w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           multiply  12                   by   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * successiva                          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-375.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-cli-esl-400.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Al primo elemento                   *
      *                          *-------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-400.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-cli-esl-425.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Dopo l'ultimo elemento inserito     *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-max)   not  = zero
                     move  w-acc-ser-edd-max
                                          to   w-acc-ser-edd-nel
                     go to acc-cli-esl-080.
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-nel      .
       acc-cli-esl-405.
           if        w-acc-ser-edd-nel    =    zero
                     go to acc-cli-esl-410.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero
                     subtract  1          from w-acc-ser-edd-nel
                     go to     acc-cli-esl-405.
       acc-cli-esl-410.
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-425.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcc]              *
      *                          *-------------------------------------*
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-let-arc-dcc-cod      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Trattamento descrizione             *
      *                          *-------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-cod-cli-ers
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
      *                          *-------------------------------------*
      *                          * Se record non trovato : reimposta-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to acc-cli-esl-200.
      *                          *-------------------------------------*
      *                          * Controllo valore impostato          *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero
                     go to acc-cli-esl-450
           else      go to acc-cli-esl-500.
       acc-cli-esl-450.
      *                          *-------------------------------------*
      *                          * Se impostato zero                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore precedente era a   *
      *                              * zero : come per Down            *
      *                              *---------------------------------*
           if        w-acc-ser-edd-spe    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-225.
      *                              *---------------------------------*
      *                              * Altrimenti come per Remv        *
      *                              *---------------------------------*
           move      "REMV"               to   v-key                  .
           go to     acc-cli-esl-300.
       acc-cli-esl-500.
      *                  *---------------------------------------------*
      *                  * Passaggio ad elemento successivo            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione elementi selezionati    *
      *                      *-----------------------------------------*
           perform   acc-cli-esl-960      thru acc-cli-esl-969        .
      *                      *-----------------------------------------*
      *                      * Incremento numero elemento              *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-ser-edd-nel      .
      *                      *-----------------------------------------*
      *                      * Se fine elementi : uscita               *
      *                      *-----------------------------------------*
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  spaces         to   v-key
                     go to acc-cli-esl-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad impostazione linea      *
      *                      *-----------------------------------------*
           go to     acc-cli-esl-080.
       acc-cli-esl-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio v-key e w-cnt-acc-ric-sel       *
      *                  *---------------------------------------------*
           move      v-key                to   w-acc-ser-edd-svk      .
           move      w-cnt-acc-ric-sel    to   w-acc-ser-edd-stu      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino  immagine video                  *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino v-key e w-cnt-acc-ric-sel        *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-svk    to   v-key                  .
           move      w-acc-ser-edd-stu    to   w-cnt-acc-ric-sel      .
      *                  *---------------------------------------------*
      *                  * Fine routine                                *
      *                  *---------------------------------------------*
           go to     acc-cli-esl-999.
       acc-cli-esl-890.
      *              *-------------------------------------------------*
      *              * Subroutines interne                             *
      *              *-------------------------------------------------*
       acc-cli-esl-900.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero w-acc-ser-edd-n1v              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-c0p      .
           subtract  1                    from w-acc-ser-edd-c0p      .
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-c0q      .
           add       12                   to   w-acc-ser-edd-c0q      .
       acc-cli-esl-901.
           add       1                    to   w-acc-ser-edd-c0p      .
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-max
                     go to acc-cli-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-cli-esl-909.
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nev    to   w-acc-ser-edd-c0r      .
       acc-cli-esl-903.
           if        w-acc-ser-edd-c0r    >    12
                     subtract  12         from w-acc-ser-edd-c0r
                     go to     acc-cli-esl-903.
           add       6                    to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
           go to     acc-cli-esl-901.
       acc-cli-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-cli-esl-909.
           add       1                    to   w-acc-ser-edd-c0r      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0p      .
           go to     acc-cli-esl-905.
       acc-cli-esl-909.
           exit.
       acc-cli-esl-910.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elemento indirizzato da     *
      *                  * w-acc-ser-edd-nev a linea w-acc-ser-edd-c0r *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-led      .
      *                      *-----------------------------------------*
      *                      * Composizione linea da visualizzare      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing numero riga                 *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-acc-ser-edd-nev    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-rig      .
      *                          *-------------------------------------*
      *                          * Editing carattere  ':'              *
      *                          *-------------------------------------*
           move      ":"                  to   w-acc-ser-edd-dpu      .
      *                          *-------------------------------------*
      *                          * Editing codice cliente              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nev)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-cod      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rr-cod-cli-ers
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-des      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-acc-ser-edd-led    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-919.
           exit.
       acc-cli-esl-920.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec se necessario             *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-fce      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nec)   not  = zero
                     go to acc-cli-esl-929.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-cli-esl-929.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-cli-esl-929.
           perform   acc-cli-esl-930      thru acc-cli-esl-939        .
           move      "#"                  to   w-acc-ser-edd-fce      .
       acc-cli-esl-929.
           exit.
       acc-cli-esl-930.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec in ogni caso              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
       acc-cli-esl-931.
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-cli-esl-932.
           move      rr-cod-cli-ele
                    (w-acc-ser-edd-c0b)   to   rr-cod-cli-ele
                                              (w-acc-ser-edd-c0a)     .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-cli-esl-933.
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           go to     acc-cli-esl-931.
       acc-cli-esl-932.
           move      zero                 to   rr-cod-cli-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-cli-ers
                                              (w-acc-ser-edd-c0a)     .
       acc-cli-esl-933.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-934.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-934.
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       6                    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0c      .
           add       1                    to   w-acc-ser-edd-c0c      .
       acc-cli-esl-935.
           if        w-acc-ser-edd-c0a    =    12
                     go to acc-cli-esl-936.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0c      .
           go to     acc-cli-esl-935.
       acc-cli-esl-936.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-937.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-937.
           subtract  w-acc-ser-edd-c0a    from 12
                                        giving w-acc-ser-edd-c0b      .
           add       w-acc-ser-edd-nec
                     w-acc-ser-edd-c0b  giving w-acc-ser-edd-c0c      .
           if        w-acc-ser-edd-c0c    >    w-acc-ser-edd-max
                     go to acc-cli-esl-938.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0c)   =    zero
                     go to acc-cli-esl-938.
           move      w-acc-ser-edd-c0c    to   w-acc-ser-edd-nev      .
           move      18                   to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
           go to     acc-cli-esl-939.
       acc-cli-esl-938.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-939.
           exit.
       acc-cli-esl-940.
      *                  *---------------------------------------------*
      *                  * Inserimento dell' elemento indirizzato da   *
      *                  * w-acc-ser-edd-nec                           *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
       acc-cli-esl-941.
           move      rr-cod-cli-ele
                    (w-acc-ser-edd-c0a)   to   rr-cod-cli-ele
                                              (w-acc-ser-edd-c0b)     .
           if        w-acc-ser-edd-c0a    =    w-acc-ser-edd-nec
                     go to acc-cli-esl-942.
           subtract  1                    from w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           go to     acc-cli-esl-941.
       acc-cli-esl-942.
           move      zero                 to   rr-cod-cli-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-cli-ers
                                              (w-acc-ser-edd-c0a)     .
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-943.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-943.
           add       6                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    =    18
                     go to acc-cli-esl-945.
           move      17                   to   w-acc-ser-edd-c0b      .
           move      18                   to   w-acc-ser-edd-c0c      .
       acc-cli-esl-944.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        w-acc-ser-edd-c0b    =    w-acc-ser-edd-c0a
                     go to acc-cli-esl-945.
           subtract  1                    from w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0c      .
           go to     acc-cli-esl-944.
       acc-cli-esl-945.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0a    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-949.
           exit.
       acc-cli-esl-960.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi da trattare        *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c0a      .
       acc-cli-esl-962.
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-cli-esl-964.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-964.
           go to     acc-cli-esl-962.
       acc-cli-esl-964.
           subtract  1                    from w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   rr-cod-cli-els         .
       acc-cli-esl-969.
           exit.
       acc-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Preparazione codici selezionati                           *
      *    *-----------------------------------------------------------*
       pcs-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Preparazione di un campo text di massimo 3 li-  *
      *              * nee da 40 caratteri ciascuna                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-ser-edd-txt      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-cod-cli-sns       =    zero
                     go to pcs-cli-esl-900.
           move      zero                 to   w-acc-ser-edd-c01      .
       pcs-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt
                    (119:1)               not  = spaces
                     go to pcs-cli-esl-900.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to pcs-cli-esl-900.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to pcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Editing codice elemento                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "("            to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
           move      w-acc-ser-edd-rtr (1)
                                          to   w-all-str-cat (2)      .
           move      w-acc-ser-edd-rtr (2)
                                          to   w-all-str-cat (3)      .
           move      w-acc-ser-edd-rtr (3)
                                          to   w-all-str-cat (4)      .
           if        w-acc-ser-edd-txt    =    spaces
                     move  spaces         to   w-all-str-cat (5)
           else      move  ","            to   w-all-str-cat (5)      .
           move      v-edt                to   w-all-str-cat (6)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-acc-ser-edd-txt      .
       pcs-cli-esl-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pcs-cli-esl-100.
       pcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt    =    spaces
                     go to pcs-cli-esl-999.
      *              *-------------------------------------------------*
      *              * Completamento valore in uscita                  *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-acc-ser-edd-rtr (1)
                                          to   w-all-str-cat (1)      .
           move      w-acc-ser-edd-rtr (2)
                                          to   w-all-str-cat (2)      .
           move      w-acc-ser-edd-rtr (3)
                                          to   w-all-str-cat (3)      .
           move      ")"                  to   w-all-str-cat (4)      .   
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-acc-ser-edd-txt      .
           if        w-acc-ser-edd-txt
                    (120:1)               not  = ")"  and
                     w-acc-ser-edd-txt
                    (120:1)               not  = spaces
                     move  "...)"         to   w-acc-ser-edd-txt
                                              (117:4)                 .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pcs-cli-esl-999.
       pcs-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione codici selezionati                        *
      *    *-----------------------------------------------------------*
       vcs-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Pulizia della prima riga e segnale di selezione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "Tutti     "   to   v-alf
           else      move  "+         "   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Preparazione contatore                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       vcs-cli-esl-200.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    1
                     go to vcs-cli-esl-900.
       vcs-cli-esl-300.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           add       w-acc-ser-edd-c01    to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-acc-ser-edd-rtr
                    (w-acc-ser-edd-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-cli-esl-400.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vcs-cli-esl-200.
       vcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vcs-cli-esl-999.
       vcs-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di validita' finale del listino da    *
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
           move      11                   to   v-lin                  .
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
      *    * Visualizzazione : Data di validita' finale del listino da *
      *    *                   storicizzare                            *
      *    *-----------------------------------------------------------*
       vis-dva-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      11                   to   v-lin                  .
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
      *              * Controllo su tipo storicizzazione               *
      *              *-------------------------------------------------*
           if        rr-tip-sto           =    zero or
                     rr-tip-sto           <    05
                     go to tdo-ric-sel-200.
           move      "Tipo di storicizzazione errato !                  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controlli in funzione del tipo storicizzazione  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo storicizza- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-210
                     tdo-ric-sel-220
                     tdo-ric-sel-230
                     tdo-ric-sel-240
                     depending            on   rr-tip-sto             .
       tdo-ric-sel-210.
      *                  *---------------------------------------------*
      *                  * Un solo cliente                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se presente codice cliente         *
      *                      *-----------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to tdo-ric-sel-300.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca il codice cliente da storicizzare !         
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-220.
      *                  *---------------------------------------------*
      *                  * Tutti i clienti                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessun controllo                        *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-300.
       tdo-ric-sel-230.
      *                  *---------------------------------------------*
      *                  * I clienti di un agente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se presente codice agente          *
      *                      *-----------------------------------------*
           if        rr-cod-age           not  = zero
                     go to tdo-ric-sel-300.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca il codice agente da storicizzare !          
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-240.
      *                  *---------------------------------------------*
      *                  * Una selezione di clienti                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se presente codice cliente         *
      *                      *-----------------------------------------*
           if        rr-cod-cli-els       not  = zero
                     go to tdo-ric-sel-300.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca almeno un codice cliente da storicizzare !  
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
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
                     go to tdo-ric-sel-800
           else      go to tdo-ric-sel-950.
       tdo-ric-sel-800.
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
      *              * Tipo di storicizzazione                         *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-sto             .
      *              *-------------------------------------------------*
      *              * Codice listino                                  *
      *              *-------------------------------------------------*
           move      spaces               to   rr-cod-lst             .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
      *              *-------------------------------------------------*
      *              * Data di validita' finale del listino            *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dva-fin             .
      *              *-------------------------------------------------*
      *              * Data di validita' finale del listino in formato *
      *              * 'reverse'                                       *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dvf-rev             .
      *              *-------------------------------------------------*
      *              * Normalizzazioni cliente                         *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-cli-000  thru nor-ric-sel-cli-999    .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Normalizzazione specifica per codici cliente              *
      *    *-----------------------------------------------------------*
       nor-ric-sel-cli-000.
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      zero                 to   rr-cod-cli-sns         .
           move      zero                 to   rr-cod-cli-els         .
       nor-ric-sel-cli-100.
           add       1                    to   rr-cod-cli-els         .
           if        rr-cod-cli-els       >    36
                     go to nor-ric-sel-cli-200.
           move      zero                 to   rr-cod-cli-eco
                                              (rr-cod-cli-els)        .
           move      spaces               to   rr-cod-cli-ers
                                              (rr-cod-cli-els)        .
           go to     nor-ric-sel-cli-100.
       nor-ric-sel-cli-200.
           move      zero                 to   rr-cod-cli-els         .
           go to     nor-ric-sel-cli-999.
       nor-ric-sel-cli-999.
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
      *    * Controllo su validita' richieste                          *
      *    *-----------------------------------------------------------*
       ctl-val-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita a Ok             *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-val-ric-flg      .
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
       exe-sub-pgm-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero listino attualmente     *
      *              * trattato                                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-sto-lst-num-att      .
      *              *-------------------------------------------------*
      *              * Start su [dcc]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      rr-cod-cli           to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-sub-pgm-800.
       exe-sub-pgm-200.
      *              *-------------------------------------------------*
      *              * Read next su [dcc]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-sub-pgm-800.
       exe-sub-pgm-300.
      *              *-------------------------------------------------*
      *              * Test max su [dcc]                               *
      *              *-------------------------------------------------*
           if        rr-cod-cli           =    zero
                     go to exe-sub-pgm-400.
           if        rf-dcc-cod-cli       not  = rr-cod-cli
                     go to exe-sub-pgm-800.
       exe-sub-pgm-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [dcc]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo clienti principali                     *
      *                  *---------------------------------------------*
           if        rf-dcc-dpz-cli       not  = spaces
                     go to exe-sub-pgm-200.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo storicizza- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           go to     exe-sub-pgm-410
                     exe-sub-pgm-420
                     exe-sub-pgm-430
                     exe-sub-pgm-440
                     depending            on   rr-tip-sto             .
       exe-sub-pgm-410.
      *                  *---------------------------------------------*
      *                  * Un solo cliente                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice cliente                  *
      *                      *-----------------------------------------*
           if        rf-dcc-cod-cli       not  = rr-cod-cli
                     go to exe-sub-pgm-200.
      *                      *-----------------------------------------*
      *                      * A determinazione listino cliente        *
      *                      *-----------------------------------------*
           go to     exe-sub-pgm-500.
       exe-sub-pgm-420.
      *                  *---------------------------------------------*
      *                  * Tutti i clienti                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A determinazione listino cliente        *
      *                      *-----------------------------------------*
           go to     exe-sub-pgm-500.
       exe-sub-pgm-430.
      *                  *---------------------------------------------*
      *                  * I clienti di un agente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice agente                   *
      *                      *-----------------------------------------*
           if        rf-dcc-cod-age       not  = rr-cod-age
                     go to exe-sub-pgm-200.
      *                      *-----------------------------------------*
      *                      * A determinazione listino cliente        *
      *                      *-----------------------------------------*
           go to     exe-sub-pgm-500.
       exe-sub-pgm-440.
      *                  *---------------------------------------------*
      *                  * Una selezione di clienti                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su selezione codici clienti        *
      *                      *-----------------------------------------*
           set       rr-cod-cli-inx       to   1                      .
           search    rr-cod-cli-ele
                     when    rr-cod-cli-eco
                            (rr-cod-cli-inx)
                                          =    rf-dcc-cod-cli
                     go to   exe-sub-pgm-500.
      *                      *-----------------------------------------*
      *                      * A riciclo su cliente successivo         *
      *                      *-----------------------------------------*
           go to     exe-sub-pgm-200.
       exe-sub-pgm-500.
      *              *-------------------------------------------------*
      *              * Determinazione preliminare se esistono prezzi   *
      *              * netti concordati per il cliente in corso si     *
      *              * trattamento                                     *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRO    "         to   f-key                  .
           move      02                   to   rf-lst-tip-rec         .
           move      rr-cod-lst           to   rf-lst-cod-lst         .
           move      rf-dcc-cod-cli       to   rf-lst-cod-cli         .
           move      spaces               to   rf-lst-sgl-vlt         .
           move      zero                 to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a riciclo su clienti      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-sub-pgm-200.
       exe-sub-pgm-520.
      *              *-------------------------------------------------*
      *              * Read next su listino da storicizzare            *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : a riciclo su clienti    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-sub-pgm-200.
       exe-sub-pgm-530.
      *              *-------------------------------------------------*
      *              * Test max su listino da storicizzare             *
      *              *-------------------------------------------------*
           if        rf-lst-tip-rec       not  = 02             or
                     rf-lst-cod-lst       not  = spaces         or
                     rf-lst-cod-cli       not  = rf-dcc-cod-cli
                     go to exe-sub-pgm-200.
       exe-sub-pgm-600.
      *              *-------------------------------------------------*
      *              * Storicizzazione prezzi netti del cliente        *
      *              *-------------------------------------------------*
           perform   sto-pnc-cli-000      thru sto-pnc-cli-999        .
      *              *-------------------------------------------------*
      *              * Se uscita con errore : ad uscita                *
      *              *-------------------------------------------------*
           if        w-sto-lst-flg-exs    not  = spaces
                     go to exe-sub-pgm-900.
       exe-sub-pgm-700.
      *              *-------------------------------------------------*
      *              * Riciclo al cliente successivo da storicizzare   *
      *              *-------------------------------------------------*
           go to     exe-sub-pgm-200.
       exe-sub-pgm-800.
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
       pos-sto-lst-105.
      *                  *---------------------------------------------*
      *                  * Test se un solo listino                     *
      *                  *---------------------------------------------*
           if        w-sto-lst-num-att    not  > 1
                     go to pos-sto-lst-200.
       pos-sto-lst-110.
      *                  *---------------------------------------------*
      *                  * Titolo per il totale                        *
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
           move      "STORICIZZAZIONE PREZZI NETTI CONCORDATI "
                                          to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       pos-sto-lst-999.
           exit.

      *    *===========================================================*
      *    * Storicizzazione prezzi netti concordati cliente           *
      *    *-----------------------------------------------------------*
       sto-pnc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-sto-lst-flg-exs      .
       sto-pnc-cli-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatori di controllo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero records letti e presi in considera-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-ctr-lec      .
      *                  *---------------------------------------------*
      *                  * Numero records scritti ex novo o aggiornati *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-ctr-sen      .
      *                  *---------------------------------------------*
      *                  * Numero records riscritti solo per aggiorna- *
      *                  * mento data finale                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-ctr-apd      .
       sto-pnc-cli-125.
      *              *-------------------------------------------------*
      *              * Editing codice cliente                          *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-dcc-cod-cli       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-pnc-cli-edt      .
       sto-pnc-cli-150.
      *              *-------------------------------------------------*
      *              * Preparazione area per note operative relative   *
      *              * al singolo cliente                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione linee 17..21                  *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      17                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 18                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente in corso di storicizzazione .......
      -              "............... :             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-sto-pnc-cli-edt    to   v-alf                  .
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
           move      "Numero prezzi letti dal listino attuale ..........
      -              "............... : 0           "
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
           move      "Numero nuovi prezzi scritti sul listino storico ..
      -              "............... : 0           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 21                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero prezzi invariati ..........................
      -              "............... : 0           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-pnc-cli-200.
      *              *-------------------------------------------------*
      *              * Start su listino da storicizzare                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRO    "         to   f-key                  .
           move      02                   to   rf-lst-tip-rec         .
           move      rr-cod-lst           to   rf-lst-cod-lst         .
           move      rf-dcc-cod-cli       to   rf-lst-cod-cli         .
           move      spaces               to   rf-lst-sgl-vlt         .
           move      zero                 to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a fine storicizzazione    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-pnc-cli-800.
      *                  *---------------------------------------------*
      *                  * Altrimenti : continuazione                  *
      *                  *---------------------------------------------*
           go to     sto-pnc-cli-300.
       sto-pnc-cli-300.
      *              *-------------------------------------------------*
      *              * Read next su listino da storicizzare            *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : a fine storicizzazione  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-pnc-cli-800.
      *                  *---------------------------------------------*
      *                  * Altrimenti : continuazione                  *
      *                  *---------------------------------------------*
           go to     sto-pnc-cli-400.
       sto-pnc-cli-400.
      *              *-------------------------------------------------*
      *              * Test max su listino da storicizzare             *
      *              *-------------------------------------------------*
           if        rf-lst-tip-rec       not  = 02             or
                     rf-lst-cod-lst       not  = spaces         or
                     rf-lst-cod-cli       not  = rf-dcc-cod-cli
                     go to sto-pnc-cli-800.
       sto-pnc-cli-450.
      *              *-------------------------------------------------*
      *              * Preparazione sub-area per esecuzione            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      02                   to   w-sto-lst-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-sto-lst-cod-lst      .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-cli       to   w-sto-lst-cod-cli      .
      *                  *---------------------------------------------*
      *                  * Sigla della valuta                          *
      *                  *---------------------------------------------*
           move      rf-lst-sgl-vlt       to   w-sto-lst-sgl-vlt      .
      *                  *---------------------------------------------*
      *                  * Numero decimali della valuta                *
      *                  *---------------------------------------------*
           move      rf-lst-dec-vlt       to   w-sto-lst-dec-vlt      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto numerico                    *
      *                  *---------------------------------------------*
           move      rf-lst-num-pro       to   w-sto-lst-num-pro      .
      *                  *---------------------------------------------*
      *                  * Prezzo                                      *
      *                  *---------------------------------------------*
           move      rf-lst-prz-lst       to   w-sto-lst-prz-lst      .
      *                  *---------------------------------------------*
      *                  * % di provvigione associate direttamente     *
      *                  * al listino                                  *
      *                  *---------------------------------------------*
           move      rf-lst-per-pvg (1)   to   w-sto-lst-per-pvg (1)  .
           move      rf-lst-per-pvg (2)   to   w-sto-lst-per-pvg (2)  .
           move      rf-lst-per-pvg (3)   to   w-sto-lst-per-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * % di sconto associate direttamente al lis-  *
      *                  * tino                                        *
      *                  *---------------------------------------------*
           move      rf-lst-per-sco (1)   to   w-sto-lst-per-sco (1)  .
           move      rf-lst-per-sco (2)   to   w-sto-lst-per-sco (2)  .
           move      rf-lst-per-sco (3)   to   w-sto-lst-per-sco (3)  .
           move      rf-lst-per-sco (4)   to   w-sto-lst-per-sco (4)  .
           move      rf-lst-per-sco (5)   to   w-sto-lst-per-sco (5)  .
      *                  *---------------------------------------------*
      *                  * Si/no utilizzo del prezzo                   *
      *                  *---------------------------------------------*
           move      rf-lst-snx-prz       to   w-sto-lst-snx-prz      .
      *                  *---------------------------------------------*
      *                  * Si/no utilizzo delle % di provvigione       *
      *                  *---------------------------------------------*
           move      rf-lst-snx-pvg       to   w-sto-lst-snx-pvg      .
      *                  *---------------------------------------------*
      *                  * Si/no utilizzo delle % di sconto            *
      *                  *---------------------------------------------*
           move      rf-lst-snx-sco       to   w-sto-lst-snx-sco      .
      *                  *---------------------------------------------*
      *                  * Data di validita' iniziale                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-sto-lst-dva-ini      .
      *                  *---------------------------------------------*
      *                  * Data di validita' finale                    *
      *                  *---------------------------------------------*
           move      rr-dva-fin           to   w-sto-lst-dva-fin      .
      *                  *---------------------------------------------*
      *                  * Data di validita' finale formato 'reverse'  *
      *                  *---------------------------------------------*
           move      rr-dvf-rev           to   w-sto-lst-dvf-rev      .
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni speciali         *
      *                  *---------------------------------------------*
           move      rf-lst-alx-exp       to   w-sto-lst-alx-exp      .
       sto-pnc-cli-456.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     sto-pnc-cli-500.
       sto-pnc-cli-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento archivio storico                  *
      *              *-------------------------------------------------*
       sto-pnc-cli-510.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flags di:                   *
      *                  *   - Operazione di scrittura nuovo record    *
      *                  *   - Operazione di cancellazione vecchio re- *
      *                  *     cord                                    *
      *                  * a : Non eseguita                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-sto-lst-flg-put      .
           move      spaces               to   w-sto-lst-flg-del      .
       sto-pnc-cli-520.
      *                  *---------------------------------------------*
      *                  * Start per data finale in formato 'reverse'  *
      *                  * relativamente al codice in esame            *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODFR "         to   f-key                  .
           move      w-sto-lst-tip-rec    to   rf-lsd-tip-rec         .
           move      w-sto-lst-cod-lst    to   rf-lsd-cod-lst         .
           move      w-sto-lst-cod-cli    to   rf-lsd-cod-cli         .
           move      w-sto-lst-sgl-vlt    to   rf-lsd-sgl-vlt         .
           move      w-sto-lst-num-pro    to   rf-lsd-num-pro         .
           move      zero                 to   rf-lsd-dvf-rev         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad inserimento prezzo     *
      *                  * storico nuovo                               *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-pnc-cli-590.
       sto-pnc-cli-530.
      *                  *---------------------------------------------*
      *                  * Read next per data finale in formato 're-   *
      *                  * verse', relativamente al codice in esame    *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * Se At end : ad inserimento prezzo storico   *
      *                  * nuovo                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sto-pnc-cli-590.
       sto-pnc-cli-540.
      *                  *---------------------------------------------*
      *                  * Test max, e se non superato : ad inserimen- *
      *                  * to prezzo storico nuovo                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su tipo record                          *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = w-sto-lst-tip-rec
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Su codice listino                       *
      *                      *-----------------------------------------*
           if        rf-lsd-cod-lst       not  = w-sto-lst-cod-lst
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Su codice cliente                       *
      *                      *-----------------------------------------*
           if        rf-lsd-cod-cli       not  = w-sto-lst-cod-cli
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Su sigla valuta : riciclo alla lettura  *
      *                      *-----------------------------------------*
           if        rf-lsd-sgl-vlt       not  = w-sto-lst-sgl-vlt
                     go to sto-pnc-cli-530.
      *                      *-----------------------------------------*
      *                      * Su codice prodotto                      *
      *                      *-----------------------------------------*
           if        rf-lsd-num-pro       not  = w-sto-lst-num-pro
                     go to sto-pnc-cli-590.
       sto-pnc-cli-550.
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
           if        rf-lsd-tip-rec       not  = w-sto-lst-tip-rec
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Codice listino                          *
      *                      *-----------------------------------------*
           if        rf-lsd-cod-lst       not  = w-sto-lst-cod-lst
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           if        rf-lsd-cod-cli       not  = w-sto-lst-cod-cli
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Sigla valuta                            *
      *                      *-----------------------------------------*
           if        rf-lsd-sgl-vlt       not  = w-sto-lst-sgl-vlt
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           if        rf-lsd-dec-vlt       not  = w-sto-lst-dec-vlt
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Codice prodotto, numerico               *
      *                      *-----------------------------------------*
           if        rf-lsd-num-pro       not  = w-sto-lst-num-pro
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * % di provvigione 1..3                   *
      *                      *-----------------------------------------*
           if        rf-lsd-per-pvg (1)   not  = w-sto-lst-per-pvg (1)
                     go to sto-pnc-cli-590.
           if        rf-lsd-per-pvg (2)   not  = w-sto-lst-per-pvg (2)
                     go to sto-pnc-cli-590.
           if        rf-lsd-per-pvg (3)   not  = w-sto-lst-per-pvg (3)
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * % di sconto 1..5                        *
      *                      *-----------------------------------------*
           if        rf-lsd-per-sco (1)   not  = w-sto-lst-per-sco (1)
                     go to sto-pnc-cli-590.
           if        rf-lsd-per-sco (2)   not  = w-sto-lst-per-sco (2)
                     go to sto-pnc-cli-590.
           if        rf-lsd-per-sco (3)   not  = w-sto-lst-per-sco (3)
                     go to sto-pnc-cli-590.
           if        rf-lsd-per-sco (4)   not  = w-sto-lst-per-sco (4)
                     go to sto-pnc-cli-590.
           if        rf-lsd-per-sco (5)   not  = w-sto-lst-per-sco (5)
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Si/No applicazione prezzo               *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-prz       not  = w-sto-lst-snx-prz
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Si/No applicazione % provvigione        *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-pvg       not  = w-sto-lst-snx-pvg
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Si/No applicazione % sconto             *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-sco       not  = w-sto-lst-snx-sco
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Data di validita' iniziale              *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       not  = w-sto-lst-dva-ini
                     go to sto-pnc-cli-590.
      *                      *-----------------------------------------*
      *                      * Area libera per espansioni speciali     *
      *                      *-----------------------------------------*
           if        rf-lsd-alx-exp       not  = w-sto-lst-alx-exp
                     go to sto-pnc-cli-590.
       sto-pnc-cli-560.
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
           if        rf-lsd-prz-lst       =    w-sto-lst-prz-lst and
                     rf-lsd-dva-fin       =    w-sto-lst-dva-fin
                     go to sto-pnc-cli-660.
      *                  *---------------------------------------------*
      *                  * Se i rimanenti valori:                      *
      *                  *  - Prezzo di listino                        *
      *                  *  - Data di validita' finale                 *
      *                  * sono entrambi variati : si esegue l'inse-   *
      *                  * rimento di un prezzo storico nuovo          *
      *                  *---------------------------------------------*
           if        rf-lsd-prz-lst       not  = w-sto-lst-prz-lst and
                     rf-lsd-dva-fin       not  = w-sto-lst-dva-fin
                     go to sto-pnc-cli-590.
      *                  *---------------------------------------------*
      *                  * Se la data di validita' finale e' invaria-  *
      *                  * ta, e quindi l'unico dato cambiato e' il    *
      *                  * prezzo di listino : si esegue un update     *
      *                  *                                             *
      *                  * Nota : Cio' e' possibile solo in caso di    *
      *                  *        una ristoricizzazione a causa di una *
      *                  *        precedente errata storicizzazione.   *
      *                  *---------------------------------------------*
           if        rf-lsd-dva-fin       =    w-sto-lst-dva-fin
                     go to sto-pnc-cli-580.
      *                  *---------------------------------------------*
      *                  * Se l'unico dato variato e' la data di vali- *
      *                  * dita' finale : si esegue un controaggiorna- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           go to     sto-pnc-cli-570.
       sto-pnc-cli-570.
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
           go to     sto-pnc-cli-600.
       sto-pnc-cli-580.
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
           go to     sto-pnc-cli-600.
       sto-pnc-cli-590.
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
           go to     sto-pnc-cli-600.
       sto-pnc-cli-600.
      *                  *---------------------------------------------*
      *                  * Salvataggio record attuale di [lsd]         *
      *                  *---------------------------------------------*
           move      rf-lsd               to   w-sto-lst-sav-lsd      .
       sto-pnc-cli-610.
      *                  *---------------------------------------------*
      *                  * Scrittura del nuovo record                  *
      *                  *---------------------------------------------*
       sto-pnc-cli-612.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       sto-pnc-cli-614.
      *                      *-----------------------------------------*
      *                      * Composizione record                     *
      *                      *-----------------------------------------*
           move      w-sto-lst-tip-rec    to   rf-lsd-tip-rec         .
           move      w-sto-lst-cod-lst    to   rf-lsd-cod-lst         .
           move      w-sto-lst-cod-cli    to   rf-lsd-cod-cli         .
           move      w-sto-lst-sgl-vlt    to   rf-lsd-sgl-vlt         .
           move      w-sto-lst-dec-vlt    to   rf-lsd-dec-vlt         .
           move      w-sto-lst-num-pro    to   rf-lsd-num-pro         .
           move      w-sto-lst-prz-lst    to   rf-lsd-prz-lst         .
           move      w-sto-lst-per-pvg (1)
                                          to   rf-lsd-per-pvg (1)     .
           move      w-sto-lst-per-pvg (2)
                                          to   rf-lsd-per-pvg (2)     .
           move      w-sto-lst-per-pvg (3)
                                          to   rf-lsd-per-pvg (3)     .
           move      w-sto-lst-per-sco (1)
                                          to   rf-lsd-per-sco (1)     .
           move      w-sto-lst-per-sco (2)
                                          to   rf-lsd-per-sco (2)     .
           move      w-sto-lst-per-sco (3)
                                          to   rf-lsd-per-sco (3)     .
           move      w-sto-lst-per-sco (4)
                                          to   rf-lsd-per-sco (4)     .
           move      w-sto-lst-per-sco (5)
                                          to   rf-lsd-per-sco (5)     .
           move      w-sto-lst-snx-prz    to   rf-lsd-snx-prz         .
           move      w-sto-lst-snx-pvg    to   rf-lsd-snx-pvg         .
           move      w-sto-lst-snx-sco    to   rf-lsd-snx-sco         .
           move      w-sto-lst-dva-ini    to   rf-lsd-dva-ini         .
           move      w-sto-lst-dva-fin    to   rf-lsd-dva-fin         .
           move      w-sto-lst-dvf-rev    to   rf-lsd-dvf-rev         .
           move      w-sto-lst-alx-exp    to   rf-lsd-alx-exp         .
       sto-pnc-cli-616.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di ag-    *
      *                      * giornamento da eseguire                 *
      *                      *-----------------------------------------*
           if        w-sto-lst-tip-agg    =    "U"
                     go to sto-pnc-cli-620.
       sto-pnc-cli-618.
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
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
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
                     go to sto-pnc-cli-660
           else      go to sto-pnc-cli-630.
       sto-pnc-cli-620.
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
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                          *-------------------------------------*
      *                          * Set del flag sull'esito dell'opera- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "S"                  to   w-sto-lst-flg-put      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     sto-pnc-cli-630.
       sto-pnc-cli-630.
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
                     go to sto-pnc-cli-660.
      *                      *-----------------------------------------*
      *                      * Ripristino area record [lsd] da area di *
      *                      * salvataggio                             *
      *                      *-----------------------------------------*
           move      w-sto-lst-sav-lsd    to   rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Cancellazione record                    *
      *                      *-----------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Set del flag sull'esito dell'operazione *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "E"            to   w-sto-lst-flg-del
           else      move  "S"            to   w-sto-lst-flg-del      .
       sto-pnc-cli-660.
      *              *-------------------------------------------------*
      *              * Aggiornamento area per note operative relative  *
      *              * al singolo listino                              *
      *              *-------------------------------------------------*
       sto-pnc-cli-665.
      *                  *---------------------------------------------*
      *                  * Numero records letti e presi in considera-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento                              *
      *                      *-----------------------------------------*
           add       1                    to   w-sto-lst-ctr-lec      .
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
           move      w-sto-lst-ctr-lec    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-pnc-cli-670.
      *                  *---------------------------------------------*
      *                  * Numero records scritti ex novo o aggiornati *
      *                  *---------------------------------------------*
       sto-pnc-cli-671.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-sto-lst-flg-put    not  = "S" or
                     w-sto-lst-flg-del    =    "S"   or
                     w-sto-lst-flg-del    =    "E"
                     go to sto-pnc-cli-675.
       sto-pnc-cli-672.
      *                      *-----------------------------------------*
      *                      * Incremento                              *
      *                      *-----------------------------------------*
           add       1                    to   w-sto-lst-ctr-sen      .
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
           move      w-sto-lst-ctr-sen    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-pnc-cli-675.
      *                  *---------------------------------------------*
      *                  * Numero records riscritti solo per aggiorna- *
      *                  * mento data finale                           *
      *                  *---------------------------------------------*
       sto-pnc-cli-676.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-sto-lst-flg-put    =    spaces and
                     w-sto-lst-flg-del    =    spaces
                     go to sto-pnc-cli-677.
           if        w-sto-lst-flg-put    not  = "S" or
                     w-sto-lst-flg-del    not  = "S"
                     go to sto-pnc-cli-680.
       sto-pnc-cli-677.
      *                      *-----------------------------------------*
      *                      * Incremento                              *
      *                      *-----------------------------------------*
           add       1                    to   w-sto-lst-ctr-apd      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-sto-lst-ctr-apd    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sto-pnc-cli-680.
      *                  *---------------------------------------------*
      *                  * Fine aggiornamento note operative           *
      *                  *---------------------------------------------*
           go to     sto-pnc-cli-700.
       sto-pnc-cli-700.
      *              *-------------------------------------------------*
      *              * Riciclo a read next su listino da storicizzare  *
      *              *-------------------------------------------------*
           go to     sto-pnc-cli-300.
       sto-pnc-cli-800.
      *              *-------------------------------------------------*
      *              * Scrittura su rullino messaggi dei risultati     *
      *              * della storicizzazione del listino               *
      *              *-------------------------------------------------*
       sto-pnc-cli-805.
      *                  *---------------------------------------------*
      *                  * Test se primo listino                       *
      *                  *---------------------------------------------*
           if        w-sto-lst-num-att    >    1
                     go to sto-pnc-cli-810.
      *                  *---------------------------------------------*
      *                  * Editing data di validita' finale dichiarata *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rr-dva-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-dfd-edt      .
      *                  *---------------------------------------------*
      *                  * Lineette di separazione iniziali, con data  *
      *                  * di storicizzazione                          *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "*-------------------------------------------------
      -              "-----------------------------*"
                                          to   m-msg                  .
           move      "Storicizzazione al : "
                                          to   m-msg (26 : 21)        .
           move      w-sto-lst-dfd-edt    to   m-msg (47 : 08)        .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea bianca di separazione                 *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       sto-pnc-cli-810.
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Storicizzazione cliente ..........................
      -              "............... : "
                                delimited by   size
                     w-sto-pnc-cli-edt
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       sto-pnc-cli-815.
      *                  *---------------------------------------------*
      *                  * Linea bianca di separazione                 *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       sto-pnc-cli-820.
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
           move      w-sto-lst-ctr-lec    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-cdl-n9s      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Numero prezzi letti dal listino attuale ..........
      -              "............... : "
                                delimited by   size
                     w-sto-lst-cdl-n9s
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       sto-pnc-cli-825.
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
           move      w-sto-lst-ctr-sen    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sto-lst-cdl-n9s      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Numero nuovi prezzi scritti sul listino storico ..
      -              "............... : "
                                delimited by   size
                     w-sto-lst-cdl-n9s
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       sto-pnc-cli-830.
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
           move      w-sto-lst-ctr-apd    to   v-num                  .
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
       sto-pnc-cli-835.
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
       sto-pnc-cli-850.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatori di controllo totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero records letti e presi in considera-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           add       w-sto-lst-ctr-lec    to   w-sto-lst-ctt-lec      .
      *                  *---------------------------------------------*
      *                  * Numero records scritti ex novo o aggiornati *
      *                  *---------------------------------------------*
           add       w-sto-lst-ctr-sen    to   w-sto-lst-ctt-sen      .
      *                  *---------------------------------------------*
      *                  * Numero records riscritti solo per aggiorna- *
      *                  * mento data finale                           *
      *                  *---------------------------------------------*
           add       w-sto-lst-ctr-apd    to   w-sto-lst-ctt-apd      .
       sto-pnc-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     sto-pnc-cli-999.
       sto-pnc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio agenti [age]                     *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
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
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
           move      rf-age-via-age       to   w-let-arc-age-via      .
           move      rf-age-loc-age       to   w-let-arc-age-loc      .
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
           go to     let-arc-age-600.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-600.
           move      spaces               to   w-let-arc-age-rag      .
           move      spaces               to   w-let-arc-age-via      .
           move      spaces               to   w-let-arc-age-loc      .
       let-arc-age-999.
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
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
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
           go to     let-arc-cli-999.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio cliente principale in [dcc]      *
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
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-dcc-cod    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
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
           move      rf-dcc-sta-tus       to   w-let-arc-dcc-tus      .
           move      rf-dcc-sta-tud       to   w-let-arc-dcc-tud      .
           move      rf-dcc-sta-tuc       to   w-let-arc-dcc-tuc      .
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
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      zero                 to   w-let-arc-dcc-tus      .
           move      zero                 to   w-let-arc-dcc-tud      .
           move      zero                 to   w-let-arc-dcc-tuc      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

