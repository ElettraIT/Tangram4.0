       Identification division.
       Program-Id.                                 pdtp3731           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dtp                 *
      *                                Settore:    sta                 *
      *                                   Fase:    dtp373              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/12/93    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione stampa per il programma pdtp3730 *
      *                                                                *
      *                    Stampa sub-distinte virtuali con anomalie   *
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
                     "dtp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "sta"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dtp373"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdtp3731"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   SUB-DISTINTE VIRTUALI CON ANOMALIE   "       .

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
      *        * [lgv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"                          .
      *        *-------------------------------------------------------*
      *        * [lgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgt"                          .
      *        *-------------------------------------------------------*
      *        * [lgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgr"                          .
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

      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento                                      *
      *        * - D : Per descrizione                                 *
      *        * - C : Per codice                                      *
      *        * - S : Per sinonimo                                    *
      *        *-------------------------------------------------------*
           05  rr-tip-ord                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione minima e massima                          *
      *        *-------------------------------------------------------*
           05  rr-des-min                 pic  x(40)                  .
           05  rr-des-max                 pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice minimo e massimo                               *
      *        *-------------------------------------------------------*
           05  rr-caf-min                 pic  x(14)                  .
           05  rr-caf-max                 pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Sinonimo minimo e massimo                             *
      *        *-------------------------------------------------------*
           05  rr-sin-min                 pic  x(13)                  .
           05  rr-sin-max                 pic  x(13)                  .

      *    *===========================================================*
      *    * Work per bufferizzazione righe distinta                   *
      *    *-----------------------------------------------------------*
       01  w-buf.
           05  w-buf-num-ele              pic  9(04)                  .
           05  w-buf-inx-ele              pic  9(04)                  .
           05  w-buf-max-ele              pic  9(04) value 999        .
           05  w-buf-tbl-ele.
               10  w-buf-sng-ele occurs 999.
                   15  w-buf-tpm-cpt      pic  9(02)                  .
                   15  w-buf-nrm-cpt      pic  9(07)                  .
                   15  w-buf-afm-cpt      pic  x(14)                  .
                   15  w-buf-qta-ipm      pic  9(08)v9(03)            .
                   15  w-buf-qta-ipd      pic  9(08)v9(03)            .
           05  w-buf-tbl-not.
               10  w-buf-sng-not occurs 999.
                   15  w-buf-not-cpt      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per routine prn-liv-det-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-prn-liv-det.
           05  w-prn-liv-det-ctr          pic  9(03)                  .
           05  w-prn-liv-det-wca          pic  x(14)                  .
           05  w-prn-liv-det-wde          pic  x(40)                  .
           05  w-prn-liv-det-wum          pic  x(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  x(01)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dps.
               10  w-let-arc-dps-flg      pic  x(01)                  .
               10  w-let-arc-dps-num      pic  9(07)                  .
               10  w-let-arc-dps-alf      pic  x(14)                  .
               10  w-let-arc-dps-des      pic  x(40)                  .
               10  w-let-arc-dps-umi      pic  x(03)                  .
               10  w-let-arc-dps-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-num      pic  9(07)                  .
               10  w-let-arc-dpm-alf      pic  x(14)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [lgv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-lgv.
               10  w-let-arc-lgv-flg      pic  x(01)                  .
               10  w-let-arc-lgv-num      pic  9(07)                  .
               10  w-let-arc-lgv-alf      pic  x(14)                  .
               10  w-let-arc-lgv-des      pic  x(40)                  .
               10  w-let-arc-lgv-umi      pic  x(03)                  .
               10  w-let-arc-lgv-deq      pic  9(01)                  .

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
      *    * Area di comunicazione specifica per il modulo 'bzoslgv0'  *
      *    *-----------------------------------------------------------*
       01  l-zos-lgv.
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento                                   *
      *        *  - C : Per codice alfanumerico                        *
      *        *  - D : Per descrizione                                *
      *        *  - S : Per sinonimo                                   *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-tip-ord          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico minimo e massimo                  *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-cod-min          pic  x(14)                  .
           05  l-zos-lgv-cod-max          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione minima e massima                          *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-des-min          pic  x(40)                  .
           05  l-zos-lgv-des-max          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sinonimo minimo e massimo                             *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-sin-min          pic  x(13)                  .
           05  l-zos-lgv-sin-max          pic  x(13)                  .

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
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *              *-------------------------------------------------*
      *              * [lgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
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
      *              * Filtro per selezione ed ordinamento [lgv]       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/prg/obj/bzoslgv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               l-zos-lgv
                                               rf-lgv                 .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *              *-------------------------------------------------*
      *              * [lgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
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
      *              * Filtro per selezione ed ordinamento [lgv]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/prg/obj/bzoslgv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               l-zos-lgv
                                               rf-lgv                 .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           move      "pgm/dtp/prg/obj/bzoslgv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
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
      *              * Start filtro di ordinamento e selezione per il  *
      *              * file [lgv]                                      *
      *              *-------------------------------------------------*
           move      "ST"                 to   f-ope                  .
           move      rr-tip-ord           to   l-zos-lgv-tip-ord      .
           move      rr-caf-min           to   l-zos-lgv-cod-min      .
           move      rr-caf-max           to   l-zos-lgv-cod-max      .
           move      rr-des-min           to   l-zos-lgv-des-min      .
           move      rr-des-max           to   l-zos-lgv-des-max      .
           move      rr-sin-min           to   l-zos-lgv-sin-min      .
           move      rr-sin-max           to   l-zos-lgv-sin-max      .
           move      "pgm/dtp/prg/obj/bzoslgv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               l-zos-lgv
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se errore su Start : uscita con flag a Eof      *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-str-ini-999.
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessuna distinta base da stampare entro i limiti a
      -              "ssegnati !"         to   m-msg                  .
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
      *              * Read Next filtro di ordinamento e selezione per *
      *              * file [lgv]                                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/prg/obj/bzoslgv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               l-zos-lgv
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se At End : uscita con flag a Eof               *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-let-seq-999.
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
       prn-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Determinazione se esiste una distinta base per  *
      *              * la sub-distinta in esame                        *
      *              *-------------------------------------------------*
       prn-sel-rec-210.
      *                  *---------------------------------------------*
      *                  * Lettura record [lgt]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TNMASS    "         to   f-key                  .
           move      99                   to   rf-lgt-tpm-ass         .
           move      rf-lgv-num-lgv       to   rf-lgt-nrm-ass         .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
       prn-sel-rec-220.
      *                  *---------------------------------------------*
      *                  * Se non esiste una distinta base per l'ele-  *
      *                  * mento in esame : selezione non superata     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-sel-rec-900.
       prn-sel-rec-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe distinta base             *
      *              *-------------------------------------------------*
       prn-sel-rec-310.
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero elementi nel buffer *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-num-ele          .
      *                  *---------------------------------------------*
      *                  * Start su file [lgr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "TNMASS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-lgt-tpm-ass       to   rf-lgr-tpm-ass         .
           move      rf-lgt-nrm-ass       to   rf-lgr-nrm-ass         .
           move      zero                 to   rf-lgr-num-prg         .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a lettura anagrafiche     *
      *                  * correlate                                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-sel-rec-400.
       prn-sel-rec-320.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [lgr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                  *---------------------------------------------*
      *                  * Se At End : a lettura anagrafiche correlate *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-sel-rec-400.
       prn-sel-rec-330.
      *                  *---------------------------------------------*
      *                  * Test max, con eventuale salto a lettura a-  *
      *                  * nagrafiche correlata se non superato        *
      *                  *---------------------------------------------*
           if        rf-lgr-tpm-ass       not  = rf-lgt-tpm-ass or
                     rf-lgr-nrm-ass       not  = rf-lgt-nrm-ass
                     go to prn-sel-rec-400.
       prn-sel-rec-340.
      *                  *---------------------------------------------*
      *                  * Incremento contatore elementi nel buffer    *
      *                  *---------------------------------------------*
           if        w-buf-num-ele        <    w-buf-max-ele
                     add 1                to   w-buf-num-ele          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione record [lgr]                *
      *                  *---------------------------------------------*
           move      rf-lgr-tpm-cpt       to   w-buf-tpm-cpt
                                              (w-buf-num-ele)         .
           move      rf-lgr-nrm-cpt       to   w-buf-nrm-cpt
                                              (w-buf-num-ele)         .
           move      rf-lgr-afm-cpt       to   w-buf-afm-cpt
                                              (w-buf-num-ele)         .
           move      rf-lgr-qta-ipm       to   w-buf-qta-ipm
                                              (w-buf-num-ele)         .
           move      rf-lgr-qta-ipd       to   w-buf-qta-ipd
                                              (w-buf-num-ele)         .
           move      rf-lgr-not-cpt       to   w-buf-not-cpt
                                              (w-buf-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [lgr] *
      *                  *---------------------------------------------*
           go to     prn-sel-rec-320.
       prn-sel-rec-400.
      *              *-------------------------------------------------*
      *              * Lettura anagrafiche correlata alla distinta ba- *
      *              * se in esame                                     *
      *              *-------------------------------------------------*
       prn-sel-rec-410.
      *                  *---------------------------------------------*
      *                  * Se numero elementi nel buffer a zero : se-  *
      *                  * lezione comunque superata, in quanto una    *
      *                  * distinta senza righe costituisce una ano-   *
      *                  * malia                                       *
      *                  *---------------------------------------------*
           if        w-buf-num-ele        =    zero
                     go to prn-sel-rec-999.
       prn-sel-rec-420.
      *                  *---------------------------------------------*
      *                  * Altrimenti si leggono tutte le anagrafiche  *
      *                  * correlate, e la selezione e' superata solo  *
      *                  * in presenza di anomalie                     *
      *                  *---------------------------------------------*
       prn-sel-rec-430.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice per scansione   *
      *                      * elementi presenti nel buffer            *
      *                      *-----------------------------------------*
           move      zero                to    w-buf-inx-ele          .
       prn-sel-rec-440.
      *                      *-----------------------------------------*
      *                      * Incremento indice per scansione elemen- *
      *                      * ti presenti nel buffer                  *
      *                      *-----------------------------------------*
           add       1                   to    w-buf-inx-ele          .
      *                      *-----------------------------------------*
      *                      * Se oltre numero elementi presenti nel   *
      *                      * buffer significa che sono stati esami-  *
      *                      * nati tutti gli elementi senza riscon-   *
      *                      * trare alcuna anomalia, pertanto la se-  *
      *                      * lezione non e' superata                 *
      *                      *-----------------------------------------*
           if        w-buf-inx-ele       >     w-buf-num-ele
                     go to prn-sel-rec-900.
       prn-sel-rec-450.
      *                      *-----------------------------------------*
      *                      * Se la quantita' di impiego, moltiplica- *
      *                      * tore, e' a zero : anomali, pertanto se- *
      *                      * lezione superata                        *
      *                      *-----------------------------------------*
           if        w-buf-qta-ipm
                    (w-buf-inx-ele)      =     zero
                     go to prn-sel-rec-900.
       prn-sel-rec-460.
      *                      *-----------------------------------------*
      *                      * Se la quantita' di impiego, divisore,   *
      *                      * e' a zero : anomali, pertanto selezio-  *
      *                      * ne superata                             *
      *                      *-----------------------------------------*
           if        w-buf-qta-ipd
                    (w-buf-inx-ele)      =     zero
                     go to prn-sel-rec-900.
       prn-sel-rec-470.
      *                      *-----------------------------------------*
      *                      * Se non esiste l'anagrafica corrispon-   *
      *                      * dente all'elemento in esame oppure se   *
      *                      * il tipo componente non e' riconosciuto  *
      *                      * come accettabile : anomalia, pertanto   *
      *                      * selezione superata                      *
      *                      *-----------------------------------------*
       prn-sel-rec-472.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * componente                          *
      *                          *-------------------------------------*
           if        w-buf-tpm-cpt
                    (w-buf-inx-ele)      =     02
                     go to prn-sel-rec-474
           else if   w-buf-tpm-cpt
                    (w-buf-inx-ele)      =     03
                     go to prn-sel-rec-476
           else if   w-buf-tpm-cpt
                    (w-buf-inx-ele)      =     99
                     go to prn-sel-rec-478
           else      go to prn-sel-rec-480.
       prn-sel-rec-474.
      *                          *-------------------------------------*
      *                          * Se tipo 02 : semilavorato           *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-buf-inx-ele)       to   w-let-arc-dps-num      .
           move      w-buf-afm-cpt
                    (w-buf-inx-ele)       to   w-let-arc-dps-alf      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
           if        w-let-arc-dps-flg    =    spaces
                     go to prn-sel-rec-490
           else      go to prn-sel-rec-999.
       prn-sel-rec-476.
      *                          *-------------------------------------*
      *                          * Se tipo 03 : materia prima          *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-buf-inx-ele)       to   w-let-arc-dpm-num      .
           move      w-buf-afm-cpt
                    (w-buf-inx-ele)       to   w-let-arc-dpm-alf      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           if        w-let-arc-dpm-flg    =    spaces
                     go to prn-sel-rec-490
           else      go to prn-sel-rec-999.
       prn-sel-rec-478.
      *                          *-------------------------------------*
      *                          * Se tipo 99 : sub-distinta virtuale  *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-buf-inx-ele)       to   w-let-arc-lgv-num      .
           move      w-buf-afm-cpt
                    (w-buf-inx-ele)       to   w-let-arc-lgv-alf      .
           perform   let-arc-lgv-000      thru let-arc-lgv-999        .
           if        w-let-arc-lgv-flg    =    spaces
                     go to prn-sel-rec-490
           else      go to prn-sel-rec-999.
       prn-sel-rec-480.
      *                          *-------------------------------------*
      *                          * Se tipo non previsto come componen- *
      *                          * te                                  *
      *                          *-------------------------------------*
           go to     prn-sel-rec-999.
       prn-sel-rec-490.
      *                      *-----------------------------------------*
      *                      * Se sono stati superati i controlli pre- *
      *                      * cedenti non c'e' anomalia, quindi si    *
      *                      * ricicla all'elemento successivo         *
      *                      *-----------------------------------------*
           go to     prn-sel-rec-440.
       prn-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Se selezione non superata                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
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
      *              * Stampa testata distinta                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    3
                     go to prn-liv-det-100.
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-100.
      *                  *---------------------------------------------*
      *                  * Posizionamento verticale subordinato        *
      *                  *---------------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice sub-distinta                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      rf-lgv-alf-lgv       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Descrizione sub-distinta                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      rf-lgv-des-lgv       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa righe distinta                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione buffer                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prn-liv-det-ctr      .
       prn-liv-det-220.
           add       1                    to   w-prn-liv-det-ctr      .
           if        w-prn-liv-det-ctr    >    w-buf-num-ele
                     go to prn-liv-det-500.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-240.
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-240.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Sigla tipo magazzino componente             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "["                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      018                  to   p-pos                  .
           if        w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    01
                     move  "P"            to   p-alf
           else if   w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    02
                     move  "S"            to   p-alf
           else if   w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    03
                     move  "M"            to   p-alf
           else if   w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    99
                     move  "D"            to   p-alf
           else      move  "?"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      019                  to   p-pos                  .
           move      "]"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-260.
      *                  *---------------------------------------------*
      *                  * Codice e descrizione magazzino componente   *
      *                  *---------------------------------------------*
       prn-liv-det-265.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica magazzino            *
      *                      *-----------------------------------------*
       prn-liv-det-267.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ma- *
      *                          * gazzino                             *
      *                          *-------------------------------------*
           if        w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    01
                     go to prn-liv-det-270
           else if   w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    02
                     go to prn-liv-det-272
           else if   w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    03
                     go to prn-liv-det-274
           else if   w-buf-tpm-cpt
                    (w-prn-liv-det-ctr)   =    99
                     go to prn-liv-det-276
           else      go to prn-liv-det-278.
       prn-liv-det-270.
      *                          *-------------------------------------*
      *                          * Tipo 01 : prodotto finito           *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-dcp-num      .
           move      w-buf-afm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-dcp-alf      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-alf    to   w-prn-liv-det-wca      .
           move      w-let-arc-dcp-des    to   w-prn-liv-det-wde      .
           move      w-let-arc-dcp-umi    to   w-prn-liv-det-wum      .
           go to     prn-liv-det-280.
       prn-liv-det-272.
      *                          *-------------------------------------*
      *                          * Tipo 02 : semilavorato              *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-dps-num      .
           move      w-buf-afm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-dps-alf      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
           move      w-let-arc-dps-alf    to   w-prn-liv-det-wca      .
           move      w-let-arc-dps-des    to   w-prn-liv-det-wde      .
           move      w-let-arc-dps-umi    to   w-prn-liv-det-wum      .
           go to     prn-liv-det-280.
       prn-liv-det-274.
      *                          *-------------------------------------*
      *                          * Tipo 03 : materia prima             *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-dpm-num      .
           move      w-buf-afm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-dpm-alf      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           move      w-let-arc-dpm-alf    to   w-prn-liv-det-wca      .
           move      w-let-arc-dpm-des    to   w-prn-liv-det-wde      .
           move      w-let-arc-dpm-umi    to   w-prn-liv-det-wum      .
           go to     prn-liv-det-280.
       prn-liv-det-276.
      *                          *-------------------------------------*
      *                          * Tipo 99 : sub-distinta virtuale     *
      *                          *-------------------------------------*
           move      w-buf-nrm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-lgv-num      .
           move      w-buf-afm-cpt
                    (w-prn-liv-det-ctr)   to   w-let-arc-lgv-alf      .
           perform   let-arc-lgv-000      thru let-arc-lgv-999        .
           move      w-let-arc-lgv-alf    to   w-prn-liv-det-wca      .
           move      w-let-arc-lgv-des    to   w-prn-liv-det-wde      .
           move      w-let-arc-lgv-umi    to   w-prn-liv-det-wum      .
           go to     prn-liv-det-280.
       prn-liv-det-278.
      *                          *-------------------------------------*
      *                          * Tipo non riconosciuto               *
      *                          *-------------------------------------*
           move      w-buf-afm-cpt
                    (w-prn-liv-det-ctr)   to   w-prn-liv-det-wca      .
           move      spaces               to   w-prn-liv-det-wde      .
           string    "("
                                delimited by   size
                     w-buf-nrm-cpt
                    (w-prn-liv-det-ctr)
                                delimited by   size
                     ")"
                                delimited by   size
                     "..............................."
                                delimited by   size
                                          into w-prn-liv-det-wde      .
           move      spaces               to   w-prn-liv-det-wum      .
           go to     prn-liv-det-280.
       prn-liv-det-280.
      *                      *-----------------------------------------*
      *                      * Codice magazzino componente             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      021                  to   p-pos                  .
           move      w-prn-liv-det-wca    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-290.
      *                      *-----------------------------------------*
      *                      * Descrizione magazzino componente        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      036                  to   p-pos                  .
           move      w-prn-liv-det-wde    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-300.
      *                      *-----------------------------------------*
      *                      * Unita' di misura magazzino componente   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      078                  to   p-pos                  .
           move      w-prn-liv-det-wum    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-310.
      *                  *---------------------------------------------*
      *                  * Quantita' d'impiego                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           if        w-buf-qta-ipm
                    (w-prn-liv-det-ctr)   >    999999
                     move  08             to   p-car
                     move  "D"            to   p-edm
                     move  083            to   p-pos
           else      move  06             to   p-car
                     move  "GD"           to   p-edm
                     move  084            to   p-pos                  .
           move      p-lnr                to   p-lin                  .
           move      w-buf-qta-ipm
                    (w-prn-liv-det-ctr)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-320.
      *                  *---------------------------------------------*
      *                  * Divisore                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a 1 non si stampa                    *
      *                      *-----------------------------------------*
           if        w-buf-qta-ipd
                    (w-prn-liv-det-ctr)   =    1
                     go to prn-liv-det-400.
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           if        w-buf-qta-ipd
                    (w-prn-liv-det-ctr)   >    999999
                     move  08             to   p-car
                     move  "D"            to   p-edm
                     move  097            to   p-pos
           else      move  06             to   p-car
                     move  "GD"           to   p-edm
                     move  098            to   p-pos                  .
           move      p-lnr                to   p-lin                  .
           move      w-buf-qta-ipd
                    (w-prn-liv-det-ctr)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Riciclo su scansione buffer                 *
      *                  *---------------------------------------------*
           go to     prn-liv-det-220.
       prn-liv-det-500.
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
           move      "SUB-DISTINTE VIRTUALI CON ANOMALIE"
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
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "                         Descrizione distinta base
      -              "                           Udm    Quantita'     Di
      -              "visore         Annotazioni      "
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
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------------------------------------------------
      -              "-------------------------  ---  ------------  ----
      -              "--------  ----------------------"
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

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-num    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           if        rf-dcp-tip-pro       =    01
                     move  "M"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    02
                     move  "S"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    03
                     move  "I"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    09
                     move  "X"            to   w-let-arc-dcp-tpr
           else      move  spaces         to   w-let-arc-dcp-tpr      .
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
      *                  *---------------------------------------------*
      *                  * Composizione descrizione con evidenziazione *
      *                  * codice numerico                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
           string    "("
                                delimited by   size
                     w-let-arc-dcp-num
                                delimited by   size
                     ")"
                                delimited by   size
                     "..............................."
                                delimited by   size
                                          into w-let-arc-dcp-des      .
      *                  *---------------------------------------------*
      *                  * A normalizzazione parametri rimanenti tran- *
      *                  * ne il codice alfanumerico                   *
      *                  *---------------------------------------------*
           go to     let-arc-dcp-600.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-alf      .
       let-arc-dcp-600.
           move      spaces               to   w-let-arc-dcp-tpr      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dps]                         *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-num    =    zero  
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-let-arc-dps-num    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-alf-sem       to   w-let-arc-dps-alf      .
           move      rf-dps-des-sem       to   w-let-arc-dps-des      .
           move      rf-dps-umi-prd       to   w-let-arc-dps-umi      .
           move      rf-dps-dec-qta       to   w-let-arc-dps-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dps-999.
       let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dps-flg      .
      *                  *---------------------------------------------*
      *                  * Composizione descrizione con evidenziazione *
      *                  * codice numerico                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
           string    "("
                                delimited by   size
                     w-let-arc-dps-num
                                delimited by   size
                     ")"
                                delimited by   size
                     "..............................."
                                delimited by   size
                                          into w-let-arc-dps-des      .
      *                  *---------------------------------------------*
      *                  * A normalizzazione parametri rimanenti tran- *
      *                  * ne il codice alfanumerico                   *
      *                  *---------------------------------------------*
           go to     let-arc-dps-600.
       let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
           move      spaces               to   w-let-arc-dps-alf      .
       let-arc-dps-600.
           move      spaces               to   w-let-arc-dps-umi      .
           move      zero                 to   w-let-arc-dps-deq      .
       let-arc-dps-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [lgv]                         *
      *    *-----------------------------------------------------------*
       let-arc-lgv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lgv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-lgv-num    =    zero  
                     go to let-arc-lgv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-let-arc-lgv-num    to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-lgv-400.
       let-arc-lgv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-lgv-alf-lgv       to   w-let-arc-lgv-alf      .
           move      rf-lgv-des-lgv       to   w-let-arc-lgv-des      .
           move      rf-lgv-umi-prd       to   w-let-arc-lgv-umi      .
           move      rf-lgv-dec-qta       to   w-let-arc-lgv-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-lgv-999.
       let-arc-lgv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-lgv-flg      .
      *                  *---------------------------------------------*
      *                  * Composizione descrizione con evidenziazione *
      *                  * codice numerico                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-lgv-des      .
           string    "("
                                delimited by   size
                     w-let-arc-lgv-num
                                delimited by   size
                     ")"
                                delimited by   size
                     "..............................."
                                delimited by   size
                                          into w-let-arc-lgv-des      .
      *                  *---------------------------------------------*
      *                  * A normalizzazione parametri rimanenti tran- *
      *                  * ne il codice alfanumerico                   *
      *                  *---------------------------------------------*
           go to     let-arc-lgv-600.
       let-arc-lgv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lgv-des      .
           move      spaces               to   w-let-arc-lgv-alf      .
       let-arc-lgv-600.
           move      spaces               to   w-let-arc-lgv-umi      .
           move      zero                 to   w-let-arc-lgv-deq      .
       let-arc-lgv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dpm]                         *
      *    *-----------------------------------------------------------*
       let-arc-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-num    =    zero  
                     go to let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-let-arc-dpm-num    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dpm-400.
       let-arc-dpm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dpm-alf-map       to   w-let-arc-dpm-alf      .
           move      rf-dpm-des-map       to   w-let-arc-dpm-des      .
           move      rf-dpm-umi-prd       to   w-let-arc-dpm-umi      .
           move      rf-dpm-dec-qta       to   w-let-arc-dpm-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dpm-999.
       let-arc-dpm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dpm-flg      .
      *                  *---------------------------------------------*
      *                  * Composizione descrizione con evidenziazione *
      *                  * codice numerico                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
           string    "("
                                delimited by   size
                     w-let-arc-dpm-num
                                delimited by   size
                     ")"
                                delimited by   size
                     "..............................."
                                delimited by   size
                                          into w-let-arc-dpm-des      .
      *                  *---------------------------------------------*
      *                  * A normalizzazione parametri rimanenti tran- *
      *                  * ne il codice alfanumerico                   *
      *                  *---------------------------------------------*
           go to     let-arc-dpm-600.
       let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
           move      spaces               to   w-let-arc-dpm-alf      .
       let-arc-dpm-600.
           move      spaces               to   w-let-arc-dpm-umi      .
           move      zero                 to   w-let-arc-dpm-deq      .
       let-arc-dpm-999.
           exit.

