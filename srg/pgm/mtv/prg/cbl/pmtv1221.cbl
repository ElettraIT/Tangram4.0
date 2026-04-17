       Identification division.
       Program-Id.                                 pmtv1221           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mtv                 *
      *                                Settore:    tab                 *
      *                                   Fase:    mtv122              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/09/91    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione stampa per il programma pmtv1220 *
      *                                                                *
      *                    Stampa classificazioni materie varie        *
      *                                                                *
      *                    - Classi                                    *
      *                    - Gruppi                                    *
      *                    - Sottogruppi                               *
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
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "mtv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mtv122"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmtv1221"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  STAMPA CLASSIFICAZIONI MATERIALI VARI "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

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
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-rec-ric-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-rec-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No record richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No stampa                                      *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records logici                                            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zv1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv1"                          .
      *        *-------------------------------------------------------*
      *        * [zv2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv2"                          .
      *        *-------------------------------------------------------*
      *        * [zv3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv3"                          .

      *    *===========================================================*
      *    * Record per le richieste                                   *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Classe materia varia                                  *
      *        *-------------------------------------------------------*
           05  rr-cla-map                 pic  9(05)                  .
           05  rr-cla-map-des             pic  x(40)                  .
           05  rr-cla-map-sud             pic  9(02)                  .
           05  rr-cla-map-sqz             pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Gruppo materia varia                                  *
      *        *-------------------------------------------------------*
           05  rr-gru-map                 pic  9(05)                  .
           05  rr-gru-map-des             pic  x(40)                  .
           05  rr-gru-map-sud             pic  9(02)                  .
           05  rr-gru-map-sqz             pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Sottogruppo materia varia                             *
      *        *-------------------------------------------------------*
           05  rr-sgr-map                 pic  9(05)                  .
           05  rr-sgr-map-des             pic  x(40)                  .
           05  rr-sgr-map-sud             pic  9(02)                  .
           05  rr-sgr-map-sqz             pic  9(05)                  .

      *    *===========================================================*
      *    * Work per scansione scalare classi, gruppi, sottogruppi    *
      *    *-----------------------------------------------------------*
       01  w-scs-cgs.
      *        *-------------------------------------------------------*
      *        * Tipo elemento da trattare                             *
      *        * - 01 : Classe                                         *
      *        * - 02 : Gruppo                                         *
      *        * - 03 : Sottogruppo                                    *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-tip-ele          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo operazione da eseguire sull'elemento             *
      *        * - 01 : Start                                          *
      *        * - 02 : Read Next                                      *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-tip-ope          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza iniziale per la classe             *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-sqz-icl          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza finale per la classe               *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-sqz-fcl          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza iniziale per il gruppo             *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-sqz-igr          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza finale per il gruppo               *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-sqz-fgr          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza iniziale per il sottogruppo        *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-sqz-isg          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza finale per il sottogruppo          *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-sqz-fsg          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Tipo dell'ultimo elemento letto                       *
      *        * - 01 : Classe                                         *
      *        * - 02 : Gruppo                                         *
      *        * - 03 : Sottogruppo                                    *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-tip-uel          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Ultimo codice Classe letto                            *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-cod-ucl          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Ultimo codice Gruppo letto                            *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-cod-ugr          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Ultimo codice Sottogruppo letto                       *
      *        *-------------------------------------------------------*
           05  w-scs-cgs-cod-usg          pic  9(05)                  .

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

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Lettura record richieste                        *
      *              *-------------------------------------------------*
           perform   let-rec-ric-000      thru let-rec-ric-999        .
           if        w-cnt-let-rec-ric    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Lettura parametri di selezione stampa           *
      *              *-------------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Ciclo di report-program                         *
      *              *-------------------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
       main-800.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo stampa                     *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "N"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm
           else      move  spaces         to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura record richieste                                  *
      *    *-----------------------------------------------------------*
       let-rec-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-rec-ric      .
      *              *-------------------------------------------------*
      *              * Test se programma senza richieste               *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to let-rec-ric-999.
      *              *-------------------------------------------------*
      *              * Richiesta tipo funzionamento a segreteria       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Inizio lettura record richieste                 *
      *              *-------------------------------------------------*
           move      "OI"                 to   b-ope                  .
           move      s-fun                to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric
                     go to let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Estrazione segmenti da 255  bytes da record ri- *
      *              * chieste                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stu-pnt-stu      .
       let-rec-ric-100.
           move      "GT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           if        b-rsc                not  = spaces
                     go to let-rec-ric-200.
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    b-chr
                     delimited by size    into rr
                                  with pointer w-cnt-stu-pnt-stu      .
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-rec-ric-100.
       let-rec-ric-200.
      *              *-------------------------------------------------*
      *              * Fine lettura record richieste                   *
      *              *-------------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric      .
       let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Cancel modulo trattamento richieste             *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
       let-rec-ric-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   prn-opn-fls-000      thru prn-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Se errori                           *
      *                          *-------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   prn-cls-fls-000      thru prn-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No record richieste                          *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No stampa                                    *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Open files                         *
      *    *-----------------------------------------------------------*
       prn-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [zv1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *              *-------------------------------------------------*
      *              * [zv2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *              *-------------------------------------------------*
      *              * [zv3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zv1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *              *-------------------------------------------------*
      *              * [zv2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *              *-------------------------------------------------*
      *              * [zv3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
       prn-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Preparazione per Start su classi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo elemento da trattare : Classe          *
      *                  *---------------------------------------------*
           move      01                   to   w-scs-cgs-tip-ele      .
      *                  *---------------------------------------------*
      *                  * Tipo operazione da eseguire sull'elemento : *
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      01                   to   w-scs-cgs-tip-ope      .
      *                  *---------------------------------------------*
      *                  * Numero di sequenza iniziale e finale per la *
      *                  * Classe                                      *
      *                  *---------------------------------------------*
           if        rr-cla-map           =    zero
                     move  00000          to   w-scs-cgs-sqz-icl
                     move  99999          to   w-scs-cgs-sqz-fcl
           else      move  rr-cla-map-sqz to   w-scs-cgs-sqz-icl
                     move  rr-cla-map-sqz to   w-scs-cgs-sqz-fcl      .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun elemento da stampare entro i limiti assegna
      -              "ti !"               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento da     *
      *              * trattare                                        *
      *              *-------------------------------------------------*
           if        w-scs-cgs-tip-ele    =    01
                     go to prn-let-seq-100
           else if   w-scs-cgs-tip-ele    =    02
                     go to prn-let-seq-200
           else if   w-scs-cgs-tip-ele    =    03
                     go to prn-let-seq-300
           else      go to prn-let-seq-900.
       prn-let-seq-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento da trattare : Classe           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  * da eseguire                                 *
      *                  *---------------------------------------------*
           if        w-scs-cgs-tip-ope    =    01
                     go to prn-let-seq-110
           else if   w-scs-cgs-tip-ope    =    02
                     go to prn-let-seq-140
           else      go to prn-let-seq-900.
       prn-let-seq-110.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Start           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start per numero di sequenza su codice  *
      *                      * Classe iniziale                         *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-scs-cgs-sqz-icl    to   rf-zv1-sqz-num         .
           move      zero                 to   rf-zv1-cod-cla         .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * Start                                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-let-seq-120
           else      go to prn-let-seq-130.
       prn-let-seq-120.
      *                      *-----------------------------------------*
      *                      * Se Start errata                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ad uscita per Eof                   *
      *                          *-------------------------------------*
           go to     prn-let-seq-900.
       prn-let-seq-130.
      *                      *-----------------------------------------*
      *                      * Se Start Ok                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A Read Next                         *
      *                          *-------------------------------------*
           go to     prn-let-seq-140.
       prn-let-seq-140.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Read Next       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Read Next su Classe                     *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * Read Next                               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-let-seq-150
           else      go to prn-let-seq-160.
       prn-let-seq-150.
      *                      *-----------------------------------------*
      *                      * Se Read Next errata                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Come per Start errata               *
      *                          *-------------------------------------*
           go to     prn-let-seq-120.
       prn-let-seq-160.
      *                      *-----------------------------------------*
      *                      * Se Read Next Ok                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max su classe                  *
      *                          *-------------------------------------*
           if        rf-zv1-sqz-num       >    w-scs-cgs-sqz-fcl
                     go to prn-let-seq-120.
      *                          *-------------------------------------*
      *                          * Tipo ultimo elemento letto : Classe *
      *                          *-------------------------------------*
           move      01                   to   w-scs-cgs-tip-uel      .
      *                          *-------------------------------------*
      *                          * Ultimo codice Classe letto          *
      *                          *-------------------------------------*
           move      rf-zv1-cod-cla       to   w-scs-cgs-cod-ucl      .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se classe sud- *
      *                          * divisa in gruppi oppure no          *
      *                          *-------------------------------------*
           if        rf-zv1-ult-sud       =    02
                     go to prn-let-seq-180.
       prn-let-seq-170.
      *                          *-------------------------------------*
      *                          * Se classe non suddivisa in gruppi   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo del prossimo elemento da   *
      *                              * trattare : Classe               *
      *                              *---------------------------------*
           move      01                   to   w-scs-cgs-tip-ele      .
      *                              *---------------------------------*
      *                              * Tipo della prossima operazione  *
      *                              * da eseguire : Read Next         *
      *                              *---------------------------------*
           move      02                   to   w-scs-cgs-tip-ope      .
      *                              *---------------------------------*
      *                              * Uscita per Ok                   *
      *                              *---------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-180.
      *                          *-------------------------------------*
      *                          * Se classe suddivisa in gruppi       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo del prossimo elemento da   *
      *                              * trattare : Gruppo               *
      *                              *---------------------------------*
           move      02                   to   w-scs-cgs-tip-ele      .
      *                              *---------------------------------*
      *                              * Tipo della prossima operazione  *
      *                              * da eseguire : Start             *
      *                              *---------------------------------*
           move      01                   to   w-scs-cgs-tip-ope      .
      *                              *---------------------------------*
      *                              * Numero di sequenza iniziale e   *
      *                              * finale per il gruppo            *
      *                              *---------------------------------*
           if        rr-gru-map           =    zero
                     move  00000          to   w-scs-cgs-sqz-igr
                     move  99999          to   w-scs-cgs-sqz-fgr
           else      move  rr-gru-map-sqz to   w-scs-cgs-sqz-igr
                     move  rr-gru-map-sqz to   w-scs-cgs-sqz-fgr      .
      *                              *---------------------------------*
      *                              * Uscita per Ok                   *
      *                              *---------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento da trattare : Gruppo           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  * da eseguire                                 *
      *                  *---------------------------------------------*
           if        w-scs-cgs-tip-ope    =    01
                     go to prn-let-seq-210
           else if   w-scs-cgs-tip-ope    =    02
                     go to prn-let-seq-240
           else      go to prn-let-seq-900.
       prn-let-seq-210.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Start           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start per numero di sequenza su codice  *
      *                      * Gruppo iniziale                         *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-scs-cgs-cod-ucl    to   rf-zv2-cod-cla         .
           move      w-scs-cgs-sqz-igr    to   rf-zv2-sqz-num         .
           move      zero                 to   rf-zv2-cod-gru         .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * Start                                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-let-seq-220
           else      go to prn-let-seq-230.
       prn-let-seq-220.
      *                      *-----------------------------------------*
      *                      * Se Start errata                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo del prossimo elemento da trat- *
      *                          * tare : Classe                       *
      *                          *-------------------------------------*
           move      01                   to   w-scs-cgs-tip-ele      .
      *                          *-------------------------------------*
      *                          * Tipo della prossima operazione da   *
      *                          * eseguire : Read Next                *
      *                          *-------------------------------------*
           move      02                   to   w-scs-cgs-tip-ope      .
      *                          *-------------------------------------*
      *                          * Riciclo ad inizio routine           *
      *                          *-------------------------------------*
           go to     prn-let-seq-000.
       prn-let-seq-230.
      *                      *-----------------------------------------*
      *                      * Se Start Ok                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A Read Next                         *
      *                          *-------------------------------------*
           go to     prn-let-seq-240.
       prn-let-seq-240.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Read Next       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Read Next su Gruppo                     *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * Read Next                               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-let-seq-250
           else      go to prn-let-seq-260.
       prn-let-seq-250.
      *                      *-----------------------------------------*
      *                      * Se Read Next errata                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Come per Start errata               *
      *                          *-------------------------------------*
           go to     prn-let-seq-220.
       prn-let-seq-260.
      *                      *-----------------------------------------*
      *                      * Se Read Next Ok                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max su Classe                  *
      *                          *-------------------------------------*
           if        rf-zv2-cod-cla       not  = w-scs-cgs-cod-ucl
                     go to prn-let-seq-220.
      *                          *-------------------------------------*
      *                          * Test max su Gruppo                  *
      *                          *-------------------------------------*
           if        rf-zv2-sqz-num       >    w-scs-cgs-sqz-fgr
                     go to prn-let-seq-220.
      *                          *-------------------------------------*
      *                          * Tipo ultimo elemento letto : Gruppo *
      *                          *-------------------------------------*
           move      02                   to   w-scs-cgs-tip-uel      .
      *                          *-------------------------------------*
      *                          * Ultimo codice Gruppo letto          *
      *                          *-------------------------------------*
           move      rf-zv2-cod-gru       to   w-scs-cgs-cod-ugr      .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se gruppo sud- *
      *                          * diviso in sottogruppi oppure no     *
      *                          *-------------------------------------*
           if        rf-zv2-ult-sud       =    02
                     go to prn-let-seq-280.
       prn-let-seq-270.
      *                          *-------------------------------------*
      *                          * Se gruppo non suddiviso in sotto-   *
      *                          * gruppi                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo del prossimo elemento da   *
      *                              * trattare : Gruppo               *
      *                              *---------------------------------*
           move      02                   to   w-scs-cgs-tip-ele      .
      *                              *---------------------------------*
      *                              * Tipo della prossima operazione  *
      *                              * da eseguire : Read Next         *
      *                              *---------------------------------*
           move      02                   to   w-scs-cgs-tip-ope      .
      *                              *---------------------------------*
      *                              * Uscita per Ok                   *
      *                              *---------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-280.
      *                          *-------------------------------------*
      *                          * Se gruppo suddivisa in sottogruppi  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo del prossimo elemento da   *
      *                              * trattare : Sottogruppo          *
      *                              *---------------------------------*
           move      03                   to   w-scs-cgs-tip-ele      .
      *                              *---------------------------------*
      *                              * Tipo della prossima operazione  *
      *                              * da eseguire : Start             *
      *                              *---------------------------------*
           move      01                   to   w-scs-cgs-tip-ope      .
      *                              *---------------------------------*
      *                              * Numero di sequenza iniziale e   *
      *                              * finale per il sottogruppo       *
      *                              *---------------------------------*
           if        rr-sgr-map           =    zero
                     move  00000          to   w-scs-cgs-sqz-isg
                     move  99999          to   w-scs-cgs-sqz-fsg
           else      move  rr-sgr-map-sqz to   w-scs-cgs-sqz-isg
                     move  rr-sgr-map-sqz to   w-scs-cgs-sqz-fsg      .
      *                              *---------------------------------*
      *                              * Uscita per Ok                   *
      *                              *---------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento da trattare : Sottogruppo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  * da eseguire                                 *
      *                  *---------------------------------------------*
           if        w-scs-cgs-tip-ope    =    01
                     go to prn-let-seq-310
           else if   w-scs-cgs-tip-ope    =    02
                     go to prn-let-seq-340
           else      go to prn-let-seq-900.
       prn-let-seq-310.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Start           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start per numero di sequenza su codice  *
      *                      * Sottogruppo iniziale                    *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-scs-cgs-cod-ucl    to   rf-zv3-cod-cla         .
           move      w-scs-cgs-cod-ugr    to   rf-zv3-cod-gru         .
           move      w-scs-cgs-sqz-isg    to   rf-zv3-sqz-num         .
           move      zero                 to   rf-zv3-cod-sgr         .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * Start                                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-let-seq-320
           else      go to prn-let-seq-330.
       prn-let-seq-320.
      *                      *-----------------------------------------*
      *                      * Se Start errata                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo del prossimo elemento da trat- *
      *                          * tare : Gruppo                       *
      *                          *-------------------------------------*
           move      02                   to   w-scs-cgs-tip-ele      .
      *                          *-------------------------------------*
      *                          * Tipo della prossima operazione da   *
      *                          * eseguire : Read Next                *
      *                          *-------------------------------------*
           move      02                   to   w-scs-cgs-tip-ope      .
      *                          *-------------------------------------*
      *                          * Riciclo ad inizio routine           *
      *                          *-------------------------------------*
           go to     prn-let-seq-000.
       prn-let-seq-330.
      *                      *-----------------------------------------*
      *                      * Se Start Ok                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A Read Next                         *
      *                          *-------------------------------------*
           go to     prn-let-seq-340.
       prn-let-seq-340.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Read Next       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Read Next su Sottogruppo                *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * Read Next                               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-let-seq-350
           else      go to prn-let-seq-360.
       prn-let-seq-350.
      *                      *-----------------------------------------*
      *                      * Se Read Next errata                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Come per Start errata               *
      *                          *-------------------------------------*
           go to     prn-let-seq-320.
       prn-let-seq-360.
      *                      *-----------------------------------------*
      *                      * Se Read Next Ok                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max su Classe                  *
      *                          *-------------------------------------*
           if        rf-zv3-cod-cla       not  = w-scs-cgs-cod-ucl
                     go to prn-let-seq-320.
      *                          *-------------------------------------*
      *                          * Test max su Gruppo                  *
      *                          *-------------------------------------*
           if        rf-zv3-cod-gru       not  = w-scs-cgs-cod-ugr
                     go to prn-let-seq-320.
      *                          *-------------------------------------*
      *                          * Test max su Sottogruppo             *
      *                          *-------------------------------------*
           if        rf-zv3-sqz-num       >    w-scs-cgs-sqz-fsg
                     go to prn-let-seq-320.
      *                          *-------------------------------------*
      *                          * Tipo ultimo elemento letto : Sotto- *
      *                          * gruppo                              *
      *                          *-------------------------------------*
           move      03                   to   w-scs-cgs-tip-uel      .
      *                          *-------------------------------------*
      *                          * Ultimo codice Sottogruppo letto     *
      *                          *-------------------------------------*
           move      rf-zv3-cod-sgr       to   w-scs-cgs-cod-usg      .
      *                          *-------------------------------------*
      *                          * Tipo del prossimo elemento da trat- *
      *                          * tare : Sottogruppo                  *
      *                          *-------------------------------------*
           move      03                   to   w-scs-cgs-tip-ele      .
      *                          *-------------------------------------*
      *                          * Tipo della prossima operazione da   *
      *                          * eseguire : Read Next                *
      *                          *-------------------------------------*
           move      02                   to   w-scs-cgs-tip-ope      .
      *                          *-------------------------------------*
      *                          * Uscita per Ok                       *
      *                          *-------------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-900.
      *              *-------------------------------------------------*
      *              * Flag di uscita a : Eof                          *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-cic-999.
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo dell'ultimo e-  *
      *              * lemento letto                                   *
      *              *-------------------------------------------------*
           if        w-scs-cgs-tip-uel    =    01
                     go to prn-liv-det-100
           else if   w-scs-cgs-tip-uel    =    02
                     go to prn-liv-det-200
           else if   w-scs-cgs-tip-uel    =    03
                     go to prn-liv-det-300
           else      go to prn-liv-det-999.
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Classe                       *
      *              *-------------------------------------------------*
       prn-liv-det-110.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-120.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-120.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-130.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero di sequenza Classe               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      002                  to   p-pos                  .
           move      rf-zv1-sqz-num       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mnemonico Classe                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      012                  to   p-pos                  .
           move      rf-zv1-mne-cla       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione Classe                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      019                  to   p-pos                  .
           move      rf-zv1-des-cla       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Codice Classe                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      rf-zv1-cod-cla       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-140.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Gruppo                       *
      *              *-------------------------------------------------*
       prn-liv-det-210.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-220.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-220.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-230.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero di sequenza Gruppo               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      002                  to   p-pos                  .
           move      rf-zv2-sqz-num       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mnemonico Gruppo                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      012                  to   p-pos                  .
           move      rf-zv2-mne-gru       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione Gruppo                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      024                  to   p-pos                  .
           move      rf-zv2-des-gru       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Codice Gruppo                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      rf-zv2-cod-gru       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-240.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Sottogruppo                  *
      *              *-------------------------------------------------*
       prn-liv-det-310.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-320.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-320.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-330.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero di sequenza Sottogruppo          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      002                  to   p-pos                  .
           move      rf-zv3-sqz-num       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mnemonico Sottogruppo                   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      012                  to   p-pos                  .
           move      rf-zv3-mne-sgr       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione Sottogruppo                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      029                  to   p-pos                  .
           move      rf-zv3-des-sgr       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Codice Sottogruppo                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      rf-zv3-cod-sgr       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-340.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri intestazione standard    *
      *              *-------------------------------------------------*
           move      "CLASSIFICAZIONI MATERIALI VARI"
                                          to   w-cnt-tit-des-tit      .
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-cnt-tit-dat-stp      .
           move      p-pag                to   w-cnt-tit-num-pag      .
           add       1                    to   w-cnt-tit-num-pag      .
      *              *-------------------------------------------------*
      *              * Intestazione standard                           *
      *              *-------------------------------------------------*
           perform   int-pag-std-000      thru int-pag-std-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to  int-pag-sta-999.
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Sequenza   Mnemonico e descrizione per classe, gru
      -              "ppo, o sottogruppo   Cod. num."
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------   ---------------------------------------
      -              "------------------   ---------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Posizionamento fisso                            *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-999.
           exit.
