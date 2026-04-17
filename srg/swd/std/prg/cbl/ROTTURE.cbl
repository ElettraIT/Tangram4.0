      *================================================================*
      *                                                                *
      * Ciclo con controllo di rottura per max 5 livelli.              *
      *                                                                *
      *  - Working-Storage                                             *
      *                                                                *
      *  - Procedure                                                   *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * N.B. : Il ciclo di controllo puo' essere eseguito anche come   *
      *        output procedure di un sort.                            *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Note sull'utilizzo :                                           *
      *                                                                *
      * 1. Copiare questo file in un comodo per poter tranquillamente  *
      *    eseguire dei 'Change All'                                   *
      *                                                                *
      * 2. Aprire il file ricopiato ed agire su di esso come segue     *
      *                                                                *
      * 3. Assegnare una sigla alla routine di rottura che si intende  *
      *    utilizzare. Esempio: se la routine di rottura riguardasse   *
      *    un 'Aggiornamento Contabilita' da Fatture' si potrebbe as-  *
      *    segnare la sigla corrispondente alle iniziale, ovvero la    *
      *    sigla 'acf'.                                                *
      *                                                                *
      * 4. Eseguire un 'Change All' per sostituire la sigla prescelta  *
      *    alla sigla base, ovvero un 'Change All' come segue:         *
      *                                                                *
      *         Find What = xyz                                        *
      *                                                                *
      *         Change To = Sigla.routine.rottura (esempio: acf)       *
      *                                                                *
      * 5. Eseguire un 'Find' per dare un commento alla routine di     *
      *    rottura, come segue:                                        *
      *                                                                *
      *         Find What = ___                                        *
      *                                                                *
      *         Change To = Sigla.routine.rottura (esempio: acf)       *
      *                                                                *
      * 6. Copiare l'area di work in Working-Storage Section           *
      *                                                                *
      * 7. Copiare la parte procedurale in Procedure Division          *
      *                                                                *
      * 8. Completare l'area di work con i parametri di rottura        *
      *                                                                *
      * 9. Completare la parte procedurale                             *
      *                                                                *
      *================================================================*


      *    *===========================================================*
      *    * Work-area per il controllo del ciclo di rottura (xyz),    *
      *    * relativo a _____                                          *
      *    *-----------------------------------------------------------*
       01  w-cdr-xyz.
      *        *-------------------------------------------------------*
      *        * Area gestita interamente dalla parte procedurale      *
      *        *-------------------------------------------------------*
           05  w-cdr-xyz-pro.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per il salvataggio dei parametri di rottura, *
      *            * per ognuno dei max 5 livelli                      *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-sav-liv.
                   15  w-cdr-xyz-sav-l05  pic  x(64)                  .
                   15  w-cdr-xyz-sav-l04  pic  x(64)                  .
                   15  w-cdr-xyz-sav-l03  pic  x(64)                  .
                   15  w-cdr-xyz-sav-l02  pic  x(64)                  .
                   15  w-cdr-xyz-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per il salvataggio globale dei parametri di  *
      *            * rottura                                           *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area gestita specificamente dall'applicazione         *
      *        *-------------------------------------------------------*
           05  w-cdr-xyz-app.
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-flg-sub      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per la composizione dei parametri di rottura     *
      *        *-------------------------------------------------------*
           05  w-cdr-xyz-rot.
      *            *---------------------------------------------------*
      *            * 5. livello di rottura                             *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-rot-l05.
                   15  filler             pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * 4. livello di rottura                             *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-rot-l04.
                   15  filler             pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * 3. livello di rottura                             *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-rot-l03.
                   15  filler             pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * 2. livello di rottura                             *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-rot-l02.
                   15  w-cdr-xyz-rot-l02-cod-dep
                                          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * 1. livello di rottura                             *
      *            *---------------------------------------------------*
               10  w-cdr-xyz-rot-l01.
                   15  w-cdr-xyz-rot-l01-cod-mag
                                          pic  x(14)                  .


      *    *===========================================================*
      *    * Routine base di controllo del ciclo di rottura (xyz), re- *
      *    * lativo a _____                                            *
      *    *-----------------------------------------------------------*
       cdr-xyz-000.
           move      spaces               to   w-cdr-xyz-mrk-uno      .
           move      spaces               to   w-cdr-xyz-rot          .
           perform   cdr-xyz-str-ini-000  thru cdr-xyz-str-ini-999    .
           if        w-cdr-xyz-flg-sub    not  = spaces
                     move   spaces        to   w-cdr-xyz-flg-sub
                     go to  cdr-xyz-600.
       cdr-xyz-100.
           move      w-cdr-xyz-rot-l05    to   w-cdr-xyz-sav-l05      .
           move      w-cdr-xyz-rot-l04    to   w-cdr-xyz-sav-l04      .
           move      w-cdr-xyz-rot-l03    to   w-cdr-xyz-sav-l03      .
           move      w-cdr-xyz-rot-l02    to   w-cdr-xyz-sav-l02      .
           move      w-cdr-xyz-rot-l01    to   w-cdr-xyz-sav-l01      .
       cdr-xyz-200.
           perform   cdr-xyz-let-seq-000  thru cdr-xyz-let-seq-999    .
           if        w-cdr-xyz-flg-sub    not  = spaces
                     move   spaces        to   w-cdr-xyz-flg-sub
                     go to  cdr-xyz-500.
           perform   cdr-xyz-tst-max-000  thru cdr-xyz-tst-max-999    .
           if        w-cdr-xyz-flg-sub    not  = spaces
                     move   spaces        to   w-cdr-xyz-flg-sub
                     go to  cdr-xyz-500.
           perform   cdr-xyz-sel-rec-000  thru cdr-xyz-sel-rec-999    .
           if        w-cdr-xyz-flg-sub    not  = spaces
                     move   spaces        to   w-cdr-xyz-flg-sub
                     go to  cdr-xyz-200.
           perform   cdr-xyz-cmp-rot-000  thru cdr-xyz-cmp-rot-999    .
           if        w-cdr-xyz-mrk-uno    not  = spaces
                     go to cdr-xyz-300.
       cdr-xyz-250.
           perform   cdr-xyz-790          thru cdr-xyz-791            .
           perform   cdr-xyz-750          thru cdr-xyz-751            .
           perform   cdr-xyz-740          thru cdr-xyz-741            .
           perform   cdr-xyz-730          thru cdr-xyz-731            .
           perform   cdr-xyz-720          thru cdr-xyz-721            .
           perform   cdr-xyz-710          thru cdr-xyz-711            .
           go to     cdr-xyz-400.
       cdr-xyz-300.
           if        w-cdr-xyz-rot-l05    =    w-cdr-xyz-sav-l05
                     go to cdr-xyz-310.
           move      w-cdr-xyz-rot        to   w-cdr-xyz-sav-rot      .
           move      w-cdr-xyz-sav-l05    to   w-cdr-xyz-rot-l05      .
           move      w-cdr-xyz-sav-l04    to   w-cdr-xyz-rot-l04      .
           move      w-cdr-xyz-sav-l03    to   w-cdr-xyz-rot-l03      .
           move      w-cdr-xyz-sav-l02    to   w-cdr-xyz-rot-l02      .
           move      w-cdr-xyz-sav-l01    to   w-cdr-xyz-rot-l01      .
           perform   cdr-xyz-810          thru cdr-xyz-811            .
           perform   cdr-xyz-820          thru cdr-xyz-821            .
           perform   cdr-xyz-830          thru cdr-xyz-831            .
           perform   cdr-xyz-840          thru cdr-xyz-841            .
           perform   cdr-xyz-850          thru cdr-xyz-851            .
           move      w-cdr-xyz-sav-rot    to   w-cdr-xyz-rot          .
           perform   cdr-xyz-750          thru cdr-xyz-751            .
           perform   cdr-xyz-740          thru cdr-xyz-741            .
           perform   cdr-xyz-730          thru cdr-xyz-731            .
           perform   cdr-xyz-720          thru cdr-xyz-721            .
           perform   cdr-xyz-710          thru cdr-xyz-711            .
           go to     cdr-xyz-400.
       cdr-xyz-310.
           if        w-cdr-xyz-rot-l04    =    w-cdr-xyz-sav-l04
                     go to cdr-xyz-320.
           move      w-cdr-xyz-rot        to   w-cdr-xyz-sav-rot      .
           move      w-cdr-xyz-sav-l04    to   w-cdr-xyz-rot-l04      .
           move      w-cdr-xyz-sav-l03    to   w-cdr-xyz-rot-l03      .
           move      w-cdr-xyz-sav-l02    to   w-cdr-xyz-rot-l02      .
           move      w-cdr-xyz-sav-l01    to   w-cdr-xyz-rot-l01      .
           perform   cdr-xyz-810          thru cdr-xyz-811            .
           perform   cdr-xyz-820          thru cdr-xyz-821            .
           perform   cdr-xyz-830          thru cdr-xyz-831            .
           perform   cdr-xyz-840          thru cdr-xyz-841            .
           move      w-cdr-xyz-sav-rot    to   w-cdr-xyz-rot          .
           perform   cdr-xyz-740          thru cdr-xyz-741            .
           perform   cdr-xyz-730          thru cdr-xyz-731            .
           perform   cdr-xyz-720          thru cdr-xyz-721            .
           perform   cdr-xyz-710          thru cdr-xyz-711            .
           go to     cdr-xyz-400.
       cdr-xyz-320.
           if        w-cdr-xyz-rot-l03    =    w-cdr-xyz-sav-l03
                     go to cdr-xyz-330.
           move      w-cdr-xyz-rot        to   w-cdr-xyz-sav-rot      .
           move      w-cdr-xyz-sav-l03    to   w-cdr-xyz-rot-l03      .
           move      w-cdr-xyz-sav-l02    to   w-cdr-xyz-rot-l02      .
           move      w-cdr-xyz-sav-l01    to   w-cdr-xyz-rot-l01      .
           perform   cdr-xyz-810          thru cdr-xyz-811            .
           perform   cdr-xyz-820          thru cdr-xyz-821            .
           perform   cdr-xyz-830          thru cdr-xyz-831            .
           move      w-cdr-xyz-sav-rot    to   w-cdr-xyz-rot          .
           perform   cdr-xyz-730          thru cdr-xyz-731            .
           perform   cdr-xyz-720          thru cdr-xyz-721            .
           perform   cdr-xyz-710          thru cdr-xyz-711            .
           go to     cdr-xyz-400.
       cdr-xyz-330.
           if        w-cdr-xyz-rot-l02    =    w-cdr-xyz-sav-l02
                     go to cdr-xyz-340.
           move      w-cdr-xyz-rot        to   w-cdr-xyz-sav-rot      .
           move      w-cdr-xyz-sav-l02    to   w-cdr-xyz-rot-l02      .
           move      w-cdr-xyz-sav-l01    to   w-cdr-xyz-rot-l01      .
           perform   cdr-xyz-810          thru cdr-xyz-811            .
           perform   cdr-xyz-820          thru cdr-xyz-821            .
           move      w-cdr-xyz-sav-rot    to   w-cdr-xyz-rot          .
           perform   cdr-xyz-720          thru cdr-xyz-721            .
           perform   cdr-xyz-710          thru cdr-xyz-711            .
           go to     cdr-xyz-400.
       cdr-xyz-340.
           if        w-cdr-xyz-rot-l01    =    w-cdr-xyz-sav-l01
                     go to cdr-xyz-400.
           move      w-cdr-xyz-rot        to   w-cdr-xyz-sav-rot      .
           move      w-cdr-xyz-sav-l01    to   w-cdr-xyz-rot-l01      .
           perform   cdr-xyz-810          thru cdr-xyz-811            .
           move      w-cdr-xyz-sav-rot    to   w-cdr-xyz-rot          .
           perform   cdr-xyz-710          thru cdr-xyz-711            .
           go to     cdr-xyz-400.
       cdr-xyz-400.
           perform   cdr-xyz-liv-det-000  thru cdr-xyz-liv-det-999    .
           move      "#"                  to   w-cdr-xyz-mrk-uno      .
           go to     cdr-xyz-100.
       cdr-xyz-500.
           if        w-cdr-xyz-mrk-uno    =    spaces
                     go to cdr-xyz-600.
           perform   cdr-xyz-810          thru cdr-xyz-811            .
           perform   cdr-xyz-820          thru cdr-xyz-821            .
           perform   cdr-xyz-830          thru cdr-xyz-831            .
           perform   cdr-xyz-840          thru cdr-xyz-841            .
           perform   cdr-xyz-850          thru cdr-xyz-851            .
           perform   cdr-xyz-890          thru cdr-xyz-891            .
           go to     cdr-xyz-900.
       cdr-xyz-600.
           perform   cdr-xyz-nes-ela-000  thru cdr-xyz-nes-ela-999    .
           go to     cdr-xyz-900.
       cdr-xyz-710.
           perform   cdr-xyz-ini-lr1-000  thru cdr-xyz-ini-lr1-999    .
       cdr-xyz-711.
           exit.
       cdr-xyz-720.
           perform   cdr-xyz-ini-lr2-000  thru cdr-xyz-ini-lr2-999    .
       cdr-xyz-721.
           exit.
       cdr-xyz-730.
           perform   cdr-xyz-ini-lr3-000  thru cdr-xyz-ini-lr3-999    .
       cdr-xyz-731.
           exit.
       cdr-xyz-740.
           perform   cdr-xyz-ini-lr4-000  thru cdr-xyz-ini-lr4-999    .
       cdr-xyz-741.
           exit.
       cdr-xyz-750.
           perform   cdr-xyz-ini-lr5-000  thru cdr-xyz-ini-lr5-999    .
       cdr-xyz-751.
           exit.
       cdr-xyz-790.
           perform   cdr-xyz-ini-cic-000  thru cdr-xyz-ini-cic-999    .
       cdr-xyz-791.
           exit.
       cdr-xyz-810.
           perform   cdr-xyz-fin-lr1-000  thru cdr-xyz-fin-lr1-999    .
       cdr-xyz-811.
           exit.
       cdr-xyz-820.
           perform   cdr-xyz-fin-lr2-000  thru cdr-xyz-fin-lr2-999    .
       cdr-xyz-821.
           exit.
       cdr-xyz-830.
           perform   cdr-xyz-fin-lr3-000  thru cdr-xyz-fin-lr3-999    .
       cdr-xyz-831.
           exit.
       cdr-xyz-840.
           perform   cdr-xyz-fin-lr4-000  thru cdr-xyz-fin-lr4-999    .
       cdr-xyz-841.
           exit.
       cdr-xyz-850.
           perform   cdr-xyz-fin-lr5-000  thru cdr-xyz-fin-lr5-999    .
       cdr-xyz-851.
           exit.
       cdr-xyz-890.
           perform   cdr-xyz-fin-cic-000  thru cdr-xyz-fin-cic-999    .
       cdr-xyz-891.
           exit.
       cdr-xyz-900.
           go to     cdr-xyz-999.
       cdr-xyz-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Start iniziale                   *
      *    *-----------------------------------------------------------*
       cdr-xyz-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cdr-xyz-flg-sub      .
       cdr-xyz-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Nessuna registrazione elaborata  *
      *    *-----------------------------------------------------------*
       cdr-xyz-nes-ela-000.
       cdr-xyz-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Lettura sequenziale              *
      *    *-----------------------------------------------------------*
       cdr-xyz-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cdr-xyz-flg-sub      .
       cdr-xyz-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Test Max                         *
      *    *-----------------------------------------------------------*
       cdr-xyz-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cdr-xyz-flg-sub      .
       cdr-xyz-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Selezione su lettura sequanziale *
      *    *-----------------------------------------------------------*
       cdr-xyz-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cdr-xyz-flg-sub      .
       cdr-xyz-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Composizione aree di rottura     *
      *    *-----------------------------------------------------------*
       cdr-xyz-cmp-rot-000.
       cdr-xyz-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Inizio ciclo                     *
      *    *-----------------------------------------------------------*
       cdr-xyz-ini-cic-000.
       cdr-xyz-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Fine ciclo                       *
      *    *-----------------------------------------------------------*
       cdr-xyz-fin-cic-000.
       cdr-xyz-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Inizio 5. livello                *
      *    *-----------------------------------------------------------*
       cdr-xyz-ini-lr5-000.
       cdr-xyz-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Fine 5. livello                  *
      *    *-----------------------------------------------------------*
       cdr-xyz-fin-lr5-000.
       cdr-xyz-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Inizio 4. livello                *
      *    *-----------------------------------------------------------*
       cdr-xyz-ini-lr4-000.
       cdr-xyz-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Fine 4. livello                  *
      *    *-----------------------------------------------------------*
       cdr-xyz-fin-lr4-000.
       cdr-xyz-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Inizio 3. livello                *
      *    *-----------------------------------------------------------*
       cdr-xyz-ini-lr3-000.
       cdr-xyz-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Fine 3. livello                  *
      *    *-----------------------------------------------------------*
       cdr-xyz-fin-lr3-000.
       cdr-xyz-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Inizio 2. livello                *
      *    *-----------------------------------------------------------*
       cdr-xyz-ini-lr2-000.
       cdr-xyz-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Fine 2. livello                  *
      *    *-----------------------------------------------------------*
       cdr-xyz-fin-lr2-000.
       cdr-xyz-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Inizio 1. livello                *
      *    *-----------------------------------------------------------*
       cdr-xyz-ini-lr1-000.
       cdr-xyz-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Fine 1. livello                  *
      *    *-----------------------------------------------------------*
       cdr-xyz-fin-lr1-000.
       cdr-xyz-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di rottura (xyz) : Dettaglio                        *
      *    *-----------------------------------------------------------*
       cdr-xyz-liv-det-000.
       cdr-xyz-liv-det-999.
           exit.

