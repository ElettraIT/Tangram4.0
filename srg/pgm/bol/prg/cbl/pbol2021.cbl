       Identification division.
       Program-Id.                                 pbol2021           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    arc                 *
      *                                   Fase:    bol202              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/11/91    *
      *                       Ultima revisione:    NdK del 20/11/97    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione stampa per il programma bol202   *
      *                    Stampa anagrafica vettori                   *
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
                     "bol"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol202"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol2021"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "        STAMPA ANAGRAFICA VETTORI       "       .

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
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .

      *    *===========================================================*
      *    * Record per le richieste                                   *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-tip-ord                 pic  x(01)                  .
           05  rr-sel-vet                 pic  9(01)                  .
           05  rr-cod-min                 pic  9(07)                  .
           05  rr-cod-max                 pic  9(07)                  .
           05  rr-rag-min                 pic  x(20)                  .
           05  rr-rag-max                 pic  x(20)                  .
           05  rr-mne-min                 pic  x(10)                  .
           05  rr-mne-max                 pic  x(10)                  .
           05  rr-pti-min                 pic  9(11)                  .
           05  rr-pti-max                 pic  9(11)                  .
           05  rr-sel-pti                 pic  x(01)                  .

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
      *    * Area per controllo formale su partita i.v.a.              *
      *    *-----------------------------------------------------------*
       01  w-cnt-piv.
           05  w-cnt-piv-piv              pic  9(11)                  .
           05  w-cnt-piv-pir redefines
               w-cnt-piv-piv.
               10  w-cnt-piv-chr occurs 11
                                          pic  9(01)                  .
           05  w-cnt-piv-flg              pic  x(01)                  .
           05  w-cnt-piv-ctr              pic  9(02)                  .
           05  w-cnt-piv-sum              pic  9(03)                  .
           05  w-cnt-piv-w20              pic  9(02)                  .
           05  w-cnt-piv-w2r redefines
               w-cnt-piv-w20                                          .
               10  w-cnt-piv-w21          pic  9(01)                  .
               10  w-cnt-piv-w22          pic  9(01)                  .

      *    *===========================================================*
      *    * Work area per esecuzione stampa                           *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Sub-work per livello dettaglio stampa di tipo '01' :  *
      *        * senza note aggiuntive                                 *
      *        *-------------------------------------------------------*
           05  w-stp-t01.
      *            *---------------------------------------------------*
      *            * Area di stampa completa per il dettaglio          *
      *            *---------------------------------------------------*
               10  w-stp-t01-cpl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono l'area di stampa *
      *                * del dettaglio                                 *
      *                *-----------------------------------------------*
                   15  w-stp-t01-rig occurs 20.
      *                    *-------------------------------------------*
      *                    * Colonna 1                                 *
      *                    *-------------------------------------------*
                       20  w-stp-t01-col-001
                                          pic  x(13)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 2                                 *
      *                    *-------------------------------------------*
                       20  w-stp-t01-col-002
                                          pic  x(40)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 3                                 *
      *                    *-------------------------------------------*
                       20  w-stp-t01-col-003
                                          pic  x(40)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 4                                 *
      *                    *-------------------------------------------*
                       20  w-stp-t01-col-004
                                          pic  x(28)                  .
      *            *---------------------------------------------------*
      *            * Comodo per anno di riferimento lettera d'intenti  *
      *            *---------------------------------------------------*
               10  w-stp-t01-lic.
                  15  w-stp-t01-lic-ann   pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatori relativi alle colonne, uno per colonna  *
      *            *---------------------------------------------------*
               10  w-stp-t01-cco.
                  15  w-stp-t01-cco-001   pic  9(02)                  .
                  15  w-stp-t01-cco-002   pic  9(02)                  .
                  15  w-stp-t01-cco-003   pic  9(02)                  .
                  15  w-stp-t01-cco-004   pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Indici e contatori per la scansione e la stampa   *
      *            * delle colonne                                     *
      *            *---------------------------------------------------*
               10  w-stp-t01-iec.
      *                *-----------------------------------------------*
      *                * Contatore delle linee effettive da stampare   *
      *                *-----------------------------------------------*
                   15  w-stp-t01-ctr-lin  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Indice di scansione                           *
      *                *-----------------------------------------------*
                   15  w-stp-t01-inx-lin  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Comodo per linee residue                      *
      *                *-----------------------------------------------*
                   15  w-stp-t01-ctr-res  pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
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
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    "C"
                     go to prn-str-ini-200
           else if   rr-tip-ord           =    "R"
                     go to prn-str-ini-300
           else if   rr-tip-ord           =    "M"
                     go to prn-str-ini-400
           else      go to prn-str-ini-500.
       prn-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per codice                            *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODVET    "         to   f-key                  .
           move      rr-cod-min           to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-str-ini-999.
      *                  *---------------------------------------------*
      *                  * Se Ok                                       *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per ragione sociale              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per ragione sociale in uppercase      *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      rr-rag-min           to   rf-vet-rag-key         .
           move      zero                 to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-str-ini-999.
      *                  *---------------------------------------------*
      *                  * Se Ok                                       *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per mnemonico                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per mnemonico                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODMNE    "         to   f-key                  .
           move      rr-mne-min           to   rf-vet-cod-mne         .
           move      zero                 to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-str-ini-999.
      *                  *---------------------------------------------*
      *                  * Se Ok                                       *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per partita iva                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per partita iva                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRTIVA    "         to   f-key                  .
           move      rr-pti-min           to   rf-vet-prt-iva         .
           move      zero                 to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-str-ini-999.
      *                  *---------------------------------------------*
      *                  * Se Ok                                       *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun vettore entro i limiti assegnati !"
                                          to   m-msg                  .
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
      *              * Lettura sequenziale archivio [vet]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub      .
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
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    "C"
                     go to prn-tst-max-200
           else if   rr-tip-ord           =    "R"
                     go to prn-tst-max-300
           else if   rr-tip-ord           =    "M"
                     go to prn-tst-max-400
           else      go to prn-tst-max-500.
       prn-tst-max-200.
      *              *-------------------------------------------------*
      *              * Test su Codice                                  *
      *              *-------------------------------------------------*
           if        rf-vet-cod-vet       >    rr-cod-max
                     move  "#"            to   w-cnt-prn-flg-sub      .
           go to     prn-tst-max-999.
       prn-tst-max-300.
      *              *-------------------------------------------------*
      *              * Test su Ragione sociale                         *
      *              *-------------------------------------------------*
           if        rf-vet-rag-key       >    rr-rag-max
                     move  "#"            to   w-cnt-prn-flg-sub      .
           go to     prn-tst-max-999.
       prn-tst-max-400.
      *              *-------------------------------------------------*
      *              * Test su Mnemonico                               *
      *              *-------------------------------------------------*
           if        rf-vet-cod-mne       >    rr-mne-max
                     move  "#"            to   w-cnt-prn-flg-sub      .
           go to     prn-tst-max-999.
       prn-tst-max-500.
      *              *-------------------------------------------------*
      *              * Test su Partita iva                             *
      *              *-------------------------------------------------*
           if        rf-vet-prt-iva       >    rr-pti-max
                     move  "#"            to   w-cnt-prn-flg-sub      .
           go to     prn-tst-max-999.
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
       prn-sel-rec-010.
      *              *-------------------------------------------------*
      *              * Selezione su minimi e massimi                   *
      *              *-------------------------------------------------*
           if        rf-vet-cod-vet       <    rr-cod-min or
                     rf-vet-cod-vet       >    rr-cod-max or
                     rf-vet-rag-key       <    rr-rag-min or
                     rf-vet-rag-key       >    rr-rag-max or
                     rf-vet-cod-mne       <    rr-mne-min or
                     rf-vet-cod-mne       >    rr-mne-max or
                     rf-vet-prt-iva       <    rr-pti-min or
                     rf-vet-prt-iva       >    rr-pti-max
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to  prn-sel-rec-999.
       prn-sel-rec-050.
      *              *-------------------------------------------------*
      *              * Selezione su tipo di vettore                    *
      *              *-------------------------------------------------*
           if        rr-sel-vet           =    1
                     go to prn-sel-rec-100
           else if   rr-sel-vet           =    2
                     go to prn-sel-rec-052
           else if   rr-sel-vet           =    3
                     go to prn-sel-rec-053
           else if   rr-sel-vet           =    4
                     go to prn-sel-rec-054
           else      go to prn-sel-rec-100.
       prn-sel-rec-052.
      *                  *---------------------------------------------*
      *                  * Solo corrieri o spedizionieri               *
      *                  *---------------------------------------------*
           if        rf-vet-tip-vet       not  = "C"
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999
           else      go to prn-sel-rec-100.
       prn-sel-rec-053.
      *                  *---------------------------------------------*
      *                  * Solo vettori interni                        *
      *                  *---------------------------------------------*
           if        rf-vet-tip-vet       not  = "I"
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999
           else      go to prn-sel-rec-100.
       prn-sel-rec-054.
      *                  *---------------------------------------------*
      *                  * Solo incaricati alla vendita                *
      *                  *---------------------------------------------*
           if        rf-vet-tip-vet       not  = "V"
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999
           else      go to prn-sel-rec-100.
       prn-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Selezione su partita iva                        *
      *              *-------------------------------------------------*
           if        rr-tip-ord           not  = "P"
                     go to  prn-sel-rec-999.
           if        rr-sel-pti           =    "T"
                     go to  prn-sel-rec-999
           else if   rr-sel-pti           =    "E"
                     go to  prn-sel-rec-200
           else if   rr-sel-pti           =    "N"
                     go to  prn-sel-rec-999
           else      go to  prn-sel-rec-999.
       prn-sel-rec-200.
      *                  *---------------------------------------------*
      *                  * Solo se partita iva errata o mancante       *
      *                  *---------------------------------------------*
           if        rf-vet-prt-iva       =    zero
                     go to  prn-sel-rec-999.
           move      rf-vet-prt-iva       to   w-cnt-piv-piv          .
           perform   cnt-prt-iva-000      thru cnt-prt-iva-999        .
           if        w-cnt-piv-flg        =    spaces
                     move   "#"           to   w-cnt-prn-flg-sub      .
           go to     prn-sel-rec-999.
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
      *              *-------------------------------------------------*
      *              * Fincatura dettaglio                             *
      *              *-------------------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
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
       prn-liv-det-200.
      *              *-------------------------------------------------*
      *              * Preparazione colonne di stampa                  *
      *              *-------------------------------------------------*
           perform   prn-liv-det-p01-000  thru prn-liv-det-p01-999    .
       prn-liv-det-420.
      *              *-------------------------------------------------*
      *              * Stampa colonne di dettaglio preparate           *
      *              *-------------------------------------------------*
           perform   prn-liv-det-s01-000  thru prn-liv-det-s01-999    .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-440.
      *              *-------------------------------------------------*
      *              * Sottolineatura dettaglio                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * 1 interlinea                                *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Subroutine di sottolineatura dettaglio      *
      *                  *---------------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Preparazione colonne di stampa senza note aggiuntive      *
      *    *-----------------------------------------------------------*
       prn-liv-det-p01-000.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Area di stampa completamente a spaces       *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-t01-cpl          .
      *                  *---------------------------------------------*
      *                  * Contatori relativi alle colonne tutti a ze- *
      *                  * ro                                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-t01-cco-001      .
           move      zero                 to   w-stp-t01-cco-002      .
           move      zero                 to   w-stp-t01-cco-003      .
           move      zero                 to   w-stp-t01-cco-004      .
       prn-liv-det-p01-100.
      *              *-------------------------------------------------*
      *              * Preparazione vera e propria                     *
      *              *-------------------------------------------------*
       prn-liv-det-p01-110.
      *                  *---------------------------------------------*
      *                  * Colonna 1                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice vettore                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 001    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-001      .
      *                          *-------------------------------------*
      *                          * Editing codice vettore              *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-vet-cod-vet       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Allineamento a destra               *
      *                          *-------------------------------------*
           move      12                   to   w-all-str-lun          .
           move      p-edt                to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-t01-col-001
                                              (w-stp-t01-cco-001)     .
       prn-liv-det-p01-120.
      *                      *-----------------------------------------*
      *                      * Codice mnemonico vettore                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se in questa colonna           *
      *                          *-------------------------------------*
           if        rr-tip-ord           =    "P"
                     go to prn-liv-det-p01-130.
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 001    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-001      .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      rf-vet-cod-mne       to   w-stp-t01-col-001
                                              (w-stp-t01-cco-001)     .
      *                          *-------------------------------------*
      *                          * A fine colonna                      *
      *                          *-------------------------------------*
           go to     prn-liv-det-p01-190.
       prn-liv-det-p01-130.
      *                      *-----------------------------------------*
      *                      * Partita Iva vettore                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 001    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-001      .
      *                          *-------------------------------------*
      *                          * Editing Partita Iva                 *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      rf-vet-prt-iva       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra             *
      *                          *-------------------------------------*
           move      12                   to   w-all-str-lun          .
           move      p-edt                to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-t01-col-001
                                              (w-stp-t01-cco-001)     .
       prn-liv-det-p01-190.
      *                      *-----------------------------------------*
      *                      * Fine colonna 1                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-p01-200.
       prn-liv-det-p01-200.
      *                  *---------------------------------------------*
      *                  * Colonna 2                                   *
      *                  *---------------------------------------------*
       prn-liv-det-p01-210.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 002    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-002      .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      rf-vet-rag-soc       to   w-stp-t01-col-002
                                              (w-stp-t01-cco-002)     .
       prn-liv-det-p01-290.
      *                      *-----------------------------------------*
      *                      * Fine colonna 2                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-p01-300.
       prn-liv-det-p01-300.
      *                  *---------------------------------------------*
      *                  * Colonna 3                                   *
      *                  *---------------------------------------------*
       prn-liv-det-p01-310.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 003    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-003      .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      rf-vet-loc-vet       to   w-stp-t01-col-003
                                              (w-stp-t01-cco-003)     .
       prn-liv-det-p01-320.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 003    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-003      .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      rf-vet-via-vet       to   w-stp-t01-col-003
                                              (w-stp-t01-cco-003)     .
       prn-liv-det-p01-390.
      *                      *-----------------------------------------*
      *                      * Fine colonna 3                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-p01-400.
       prn-liv-det-p01-400.
      *                  *---------------------------------------------*
      *                  * Colonna 4                                   *
      *                  *---------------------------------------------*
       prn-liv-det-p01-410.
      *                      *-----------------------------------------*
      *                      * Codice mnemonico vettore                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se in questa colonna           *
      *                          *-------------------------------------*
           if        rr-tip-ord           not  = "P"
                     go to prn-liv-det-p01-420.
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 004    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-004      .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      rf-vet-cod-mne       to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)     .
      *                          *-------------------------------------*
      *                          * Proseguimento                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-p01-430.
       prn-liv-det-p01-420.
      *                      *-----------------------------------------*
      *                      * Partita Iva vettore                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 004    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-004      .
      *                          *-------------------------------------*
      *                          * Editing Partita Iva                 *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      rf-vet-prt-iva       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra             *
      *                          *-------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      p-edt                to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)     .
      *                          *-------------------------------------*
      *                          * Proseguimento                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-p01-430.
       prn-liv-det-p01-430.
      *                      *-----------------------------------------*
      *                      * Tipo di vettore                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 004    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-t01-cco-004      .
      *                          *-------------------------------------*
      *                          * Test se vettori interni             *
      *                          *-------------------------------------*
           if        rr-sel-vet           =    3   or
                     rr-sel-vet           =    4
                     move  rf-vet-trg-aut to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)
                     go to prn-liv-det-p01-490.
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           if        rf-vet-tip-vet       =    "C"
                     move  "Corriere o spedizioniere    "
                                          to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)
           else if   rf-vet-tip-vet       =    "I"
                     move  "Interno                     "
                                          to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)
           else if   rf-vet-tip-vet       =    "V"
                     move  "Incaricato alla vendita     "
                                          to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)
           else      move  spaces         to   w-stp-t01-col-004
                                              (w-stp-t01-cco-004)     .
       prn-liv-det-p01-490.
      *                      *-----------------------------------------*
      *                      * Fine colonna 4                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-p01-900.
       prn-liv-det-p01-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-p01-999.
       prn-liv-det-p01-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Stampa colonne di dettaglio preparate senza note aggiun-  *
      *    * tive                                                      *
      *    *-----------------------------------------------------------*
       prn-liv-det-s01-000.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del numero di linee effetti- *
      *                  * ve da stampare, pari alla colonna piu' lun- *
      *                  * ga                                          *
      *                  *---------------------------------------------*
           move      w-stp-t01-cco-001    to   w-stp-t01-ctr-lin      .
           if        w-stp-t01-cco-002    >    w-stp-t01-ctr-lin
                     move  w-stp-t01-cco-002
                                          to   w-stp-t01-ctr-lin      .
           if        w-stp-t01-cco-003    >    w-stp-t01-ctr-lin
                     move  w-stp-t01-cco-003
                                          to   w-stp-t01-ctr-lin      .
           if        w-stp-t01-cco-004    >    w-stp-t01-ctr-lin
                     move  w-stp-t01-cco-004
                                          to   w-stp-t01-ctr-lin      .
       prn-liv-det-s01-050.
      *              *-------------------------------------------------*
      *              * Test su linee residue                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Perparazione del numero di linee residue    *
      *                  * tenendo conto della eventuale sottolineatu- *
      *                  * ra del dettaglio                            *
      *                  *---------------------------------------------*
           move      w-stp-t01-ctr-lin    to   w-stp-t01-ctr-res      .
           add       01                   to   w-stp-t01-ctr-res      .
       prn-liv-det-s01-060.
      *                  *---------------------------------------------*
      *                  * Test se linee di stampa residue sufficienti *
      *                  *---------------------------------------------*
           if        p-res                >    w-stp-t01-ctr-res
                     go to prn-liv-det-s01-100.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-s01-999.
      *                  *---------------------------------------------*
      *                  * Fincatura dettaglio                         *
      *                  *---------------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-liv-det-s01-100.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice per la scansione    *
      *                  * sione delle linee effettive da stampare     *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-t01-inx-lin      .
       prn-liv-det-s01-200.
      *                      *-----------------------------------------*
      *                      * Incremento indice per la scansione del- *
      *                      * le linee effettive da stampare          *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-t01-inx-lin      .
      *                      *-----------------------------------------*
      *                      * Se oltre il numero di linee effettive   *
      *                      * da stampare : ad uscita                 *
      *                      *-----------------------------------------*
           if        w-stp-t01-inx-lin    >    w-stp-t01-ctr-lin
                     go to prn-liv-det-s01-260.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Stampa griglia vuota per il dettaglio   *
      *                      *-----------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
       prn-liv-det-s01-230.
      *                      *-----------------------------------------*
      *                      * Composizione della linea di stampa      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Colonna 1                           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      w-stp-t01-col-001
                    (w-stp-t01-inx-lin)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Colonna 2                           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      018                  to   p-pos                  .
           move      w-stp-t01-col-002
                    (w-stp-t01-inx-lin)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Colonna 3                           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      061                  to   p-pos                  .
           move      w-stp-t01-col-003
                    (w-stp-t01-inx-lin)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Colonna 4                           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      104                  to   p-pos                  .
           move      w-stp-t01-col-004
                    (w-stp-t01-inx-lin)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-s01-250.
      *                      *-----------------------------------------*
      *                      * Riciclo alla linea di stampa successiva *
      *                      *-----------------------------------------*
           go to     prn-liv-det-s01-200.
       prn-liv-det-s01-260.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     prn-liv-det-s01-900.
       prn-liv-det-s01-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-s01-999.
       prn-liv-det-s01-999.
           exit.

      *    *===========================================================*
      *    * Griglia vuota per il dettaglio                            *
      *    *-----------------------------------------------------------*
       prn-grv-det-000.
      *              *-------------------------------------------------*
      *              * Se stampa senza note aggiuntive                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|              |                                  
      -              "        |                                         
      -              " |                             |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-grv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-grv-det-999.
       prn-grv-det-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di sottolineatura dettaglio                    *
      *    *-----------------------------------------------------------*
       prn-stl-det-000.
      *              *-------------------------------------------------*
      *              * Se stampa senza note aggiuntive                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+--------------+----------------------------------
      -              "--------+-----------------------------------------
      -              "-+-----------------------------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     prn-stl-det-900.
       prn-stl-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-stl-det-999.
       prn-stl-det-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri intestazione standard    *
      *              *-------------------------------------------------*
           if        rr-sel-vet           =    1
                     move  "ELENCO VETTORI"
                                          to   w-cnt-tit-des-tit
           else if   rr-sel-vet           =    2
                     move  "ELENCO CORRIERI O SPEDIZIONIERI"
                                          to   w-cnt-tit-des-tit
           else if   rr-sel-vet           =    3
                     move  "ELENCO VETTORI INTERNI"
                                          to   w-cnt-tit-des-tit
           else if   rr-sel-vet           =    4
                     move  "ELENCO INCARICATI ALLA VENDITA"
                                          to   w-cnt-tit-des-tit
           else      move  "ELENCO VETTORI"
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
      *                  *---------------------------------------------*
      *                  * Se uscita per interruzione forzata          *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to  int-pag-sta-999.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *                                                           *
      *    * Fincatura per il dettaglio                                *
      *    *-----------------------------------------------------------*
       int-fin-det-000.
      *              *-------------------------------------------------*
      *              * Se stampa senza note aggiuntive                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-fin-det-100.
      *                  *---------------------------------------------*
      *                  * Sopralineatura                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+==============+==================================
      -              "========+=========================================
      -              "=+=============================+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-fin-det-200.
      *                  *---------------------------------------------*
      *                  * Test se ordinamento per Partita Iva         *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = "P"
                     go to int-fin-det-240.
       int-fin-det-220.
      *                  *---------------------------------------------*
      *                  * Fincatura 1 per Partita Iva                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|       Codice |              Ragione sociale     
      -              "        |                 Localita'               
      -              " | Mnemonico                   |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     int-fin-det-300.
       int-fin-det-240.
      *                  *---------------------------------------------*
      *                  * Fincatura 1 per Mnemonico                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|       Codice |              Ragione sociale     
      -              "        |                 Localita'               
      -              " | Partita Iva                 |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     int-fin-det-300.
       int-fin-det-300.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-fin-det-400.
      *                  *---------------------------------------------*
      *                  * Test se ordinamento per Partita Iva         *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = "P"
                     go to int-fin-det-440.
       int-fin-det-420.
      *                  *---------------------------------------------*
      *                  * Fincatura 2 per Partita Iva                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| Partita Iva  |                                  
      -              "        |                 Indirizzo               
      -              " | Note                        |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     int-fin-det-500.
       int-fin-det-440.
      *                  *---------------------------------------------*
      *                  * Fincatura 2 per Mnemonico                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| Mnemonico    |                                  
      -              "        |                 Indirizzo               
      -              " | Note                        |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     int-fin-det-500.
       int-fin-det-500.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-fin-det-800.
      *                  *---------------------------------------------*
      *                  * Sottolineatura                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+--------------+----------------------------------
      -              "--------+-----------------------------------------
      -              "-+-----------------------------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-fin-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-fin-det-999.
       int-fin-det-999.
           exit.

      *    *===========================================================*
      *    * Controllo formale su partita i.v.a.                       *
      *    *                                                           *
      *    * Input  : w-cnt-piv-piv = partita i.v.a. da controllare    *
      *    *                                                           *
      *    * Output : w-cnt-piv-flg = - spaces : p.i. corretta         *
      *    *                          - #      : p.i. errata           *
      *    *-----------------------------------------------------------*
       cnt-prt-iva-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-piv-flg          .
      *              *-------------------------------------------------*
      *              * Se p.i. a zero si esce per OK                   *
      *              *-------------------------------------------------*
           if        w-cnt-piv-piv        =    zero
                     go to cnt-prt-iva-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore caratteri e sommato- *
      *              * ria dei primi dieci caratteri                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-piv-ctr
                                               w-cnt-piv-sum          .
       cnt-prt-iva-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore caratteri                  *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-piv-ctr          .
      *              *-------------------------------------------------*
      *              * Se entro i primi dieci caratteri                *
      *              *-------------------------------------------------*
           if        w-cnt-piv-ctr        >    10
                     go to cnt-prt-iva-500.
      *                  *---------------------------------------------*
      *                  * Se 1. o 3. o 5. o 7. o 9. carattere         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Somma del carattere nella sommatoria    *
      *                      *-----------------------------------------*
           add       w-cnt-piv-chr
                    (w-cnt-piv-ctr)       to   w-cnt-piv-sum          .
      *                  *---------------------------------------------*
      *                  * Se 2. o 4. o 6. o 8. o 10. carattere        *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-piv-ctr          .
      *                      *-----------------------------------------*
      *                      * Moltiplica il carattere per due         *
      *                      *-----------------------------------------*
           multiply  2                    by   w-cnt-piv-chr
                                              (w-cnt-piv-ctr)
                                        giving w-cnt-piv-w20          .
      *                      *-----------------------------------------*
      *                      * Somma nella sommatoria il carattere si- *
      *                      * nistro e il carattere destro del risul- *
      *                      * tato della moltiplicazione              *
      *                      *-----------------------------------------*
           add       w-cnt-piv-w21        to   w-cnt-piv-sum          .
           add       w-cnt-piv-w22        to   w-cnt-piv-sum          .
      *                  *---------------------------------------------*
      *                  * Ricicla su prossimo carattere               *
      *                  *---------------------------------------------*
           go to     cnt-prt-iva-100.
       cnt-prt-iva-500.
      *              *-------------------------------------------------*
      *              * Se undicesimo carattere                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo della differenza tra 100 e la som-  *
      *                  * matoria calcolata                           *
      *                  *---------------------------------------------*
           subtract  w-cnt-piv-sum        from 100
                                        giving w-cnt-piv-w20          .
      *                  *---------------------------------------------*
      *                  * Confronto tra la cifra di destra della dif- *
      *                  * ferenza e l'undicesimo carattere : se  non  *
      *                  * sono uguali c'e' un errore formale          *
      *                  *---------------------------------------------*
           if        w-cnt-piv-w22        not  = w-cnt-piv-chr
                                                (w-cnt-piv-ctr)
                     move  "#"            to   w-cnt-piv-flg          .
       cnt-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

