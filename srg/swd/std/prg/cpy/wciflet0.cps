      *    *===========================================================*
      *    * Routine di decodifica da cifre a lettere                  *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : Si entra con il campo 'w-cif-let-imp' contenente *
      *    *          il numero da decodificare; questo deve essere al *
      *    *          massimo di 9 cifre senza segnoe senza decimali   *
      *    *                                                           *
      *    * Output : Si esce con il campo 'w-cif-let-let' contenente  *
      *    *          la decodifica eseguita; questo campo consta al   *
      *    *          massimo di 'w-cif-let-tmx' caratteri; nel caso   *
      *    *          che non fossero sufficienti il campo lettere     *
      *    *          contiene il numero editato (allineato a sinistra)*
      *    *          con la maschera : "*ZZZ.ZZZ.ZZZ*"; nel caso che  *
      *    *          entro 'w-cif-let-tmx' caratteri non sia possibile*
      *    *          sviluppare la decodifica completa le ultime tre  *
      *    *          cifre vengono riportate numericamente, altrimenti*
      *    *          anch'esse vengono decodificate.                  *
      *    *          Se il numero viene editato con la maschera di cui*
      *    *          sopra il flag 'w-cif-let-flg' viene posto = a"#" *
      *    *                                                           *
      *    *          N.B.: richiede 'mvideo' per editing              *
      *    *-----------------------------------------------------------*
       cif-let-imp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cif-let-flg          .
           move      spaces               to   w-cif-let-let          .
      *              *-------------------------------------------------*
      *              * Se l'importo in input e' zero : uscita con      *
      *              * literal 'zero'                                  *
      *              *-------------------------------------------------*
           if        w-cif-let-imp        =    zero
                     move  w-cif-let-zer  to   w-cif-let-let
                     go to cif-let-imp-400.
      *              *-------------------------------------------------*
      *              * Normalizzazioni contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cif-let-ct1          .
      *                    *-------------------------------------------*
      *                    * Sviluppo prima terzina                    *
      *                    *-------------------------------------------*
           move      1                    to   w-cif-let-ct2          .
           perform   cif-let-imp-500      thru cif-let-imp-699        .
      *                    *-------------------------------------------*
      *                    * Sviluppo seconda terzina                  *
      *                    *-------------------------------------------*
           move      2                    to   w-cif-let-ct2          .
           perform   cif-let-imp-500      thru cif-let-imp-699        .
      *                        *---------------------------------------*
      *                        * Test sul flag in uscita               *
      *                        *---------------------------------------*
           if        w-cif-let-flg        not  = spaces
                     go to cif-let-imp-400.
      *                    *-------------------------------------------*
      *                    * Sviluppo terza terzina                    *
      *                    *-------------------------------------------*
           move      3                    to   w-cif-let-ct2          .
           if        w-cif-let-ttz (3)    =    zero
                     go to cif-let-imp-300.
           move      w-cif-let-tmx        to   w-cif-let-ct3          .
           subtract  w-cif-let-ct1        from w-cif-let-ct3          .
           if        w-cif-let-ct3        <    5
                     perform   cif-let-imp-900
                                          thru cif-let-imp-909
                     go to cif-let-imp-400.
           if        w-cif-let-tca (3, 1) =    zero
                     go to cif-let-imp-100.
           if        w-cif-let-ct3        <    28
                     go to cif-let-imp-200.
       cif-let-imp-050.
           perform   cif-let-imp-500      thru cif-let-imp-699        .
           go to     cif-let-imp-300.
       cif-let-imp-100.
           if        w-cif-let-tca (3, 2) >    1
                     go to cif-let-imp-140.
           if        w-cif-let-ct3        <    16
                     go to cif-let-imp-200
           else      go to cif-let-imp-050.
       cif-let-imp-140.
           IF        w-cif-let-ct3        <    11
                     go to cif-let-imp-200
           else      go to cif-let-imp-050.
       cif-let-imp-200.
           if        w-cif-let-ct3        <    3
                     perform  cif-let-imp-900
                                          thru cif-let-imp-909
                     go to    cif-let-imp-400.
           if        w-cif-let-ct3        <    4
                     go to    cif-let-imp-220.
           add       1                    to   w-cif-let-ct1          .
           move      "-"                  to   w-cif-let-tlt
                                              (w-cif-let-ct1)         .
       cif-let-imp-220.
           add       1                    to   w-cif-let-ct1          .
           move      w-cif-let-tca (3, 1) to   w-cif-let-tlt
                                              (w-cif-let-ct1)         .
           add       1                    to   w-cif-let-ct1          .
           move      w-cif-let-tca (3, 2) to   w-cif-let-tlt
                                              (w-cif-let-ct1)         .
           add       1                    to   w-cif-let-ct1          .
           move      w-cif-let-tca (3, 3) to   w-cif-let-tlt
                                              (w-cif-let-ct1)         .
       cif-let-imp-300.
           add       1                    to   w-cif-let-ct1          .
           if        w-cif-let-ct1        >    w-cif-let-tmx
                     go to cif-let-imp-400.
      *                    *-------------------------------------------*
      *                    * Carattere di chiusura                     *
      *                    *-------------------------------------------*
           move      "/"                  to   w-cif-let-tlt
                                              (w-cif-let-ct1)         .
       cif-let-imp-400.
      *                    *-------------------------------------------*
      *                    * Uppercase del primo carattere             *
      *                    *-------------------------------------------*
           move      zero                 to   w-cif-let-ct1          .
           inspect   w-cif-let-low    tallying w-cif-let-ct1
                     for   characters   before
                                       initial w-cif-let-tlt (1)      .
           if        w-cif-let-ct1        not  <    26
                     go to cif-let-imp-450.
           add       1                    to   w-cif-let-ct1          .
           move      w-cif-let-uch
                    (w-cif-let-ct1)       to   w-cif-let-tlt (1)      .
       cif-let-imp-450.
      *                    *-------------------------------------------*
      *                    * Uscita                                    *
      *                    *-------------------------------------------*
           go to     cif-let-imp-999.
       cif-let-imp-500.
      *                    *-------------------------------------------*
      *                    * Sviluppo terzina nr w-cif-let-ct2         *
      *                    *-------------------------------------------*
           if        w-cif-let-ttz
                    (w-cif-let-ct2)       =    zero
                     go to cif-let-imp-699.
           if        w-cif-let-ttz
                    (w-cif-let-ct2)       not  = 1
                     go to cif-let-imp-550.
           if        w-cif-let-ct2        not  = 3
                     go to cif-let-imp-510.
           move      1                    to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
           go to     cif-let-imp-699.
       cif-let-imp-510.
           if        w-cif-let-ct2        not  = 2
                     go to cif-let-imp-520.
           move      29                   to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
           go to     cif-let-imp-699.
       cif-let-imp-520.
           move      1                    to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
           subtract  1                    from w-cif-let-ct1          .
           move      31                   to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
           go to     cif-let-imp-699.
       cif-let-imp-550.
      *                        *---------------------------------------*
      *                        * Centinaia                             *
      *                        *---------------------------------------*
           if        w-cif-let-tca
                    (w-cif-let-ct2, 1)    =    zero
                     go to cif-let-imp-570.
           if        w-cif-let-tca
                    (w-cif-let-ct2, 1)    =    1
                     go to cif-let-imp-560.
           move      w-cif-let-tca
                    (w-cif-let-ct2, 1)    to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
       cif-let-imp-560.
           move      28                   to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
       cif-let-imp-570.
      *                        *---------------------------------------*
      *                        * Decine                                *
      *                        *---------------------------------------*
           if        w-cif-let-tca
                    (w-cif-let-ct2, 2)    >    1
                     go to cif-let-imp-580.
           move      w-cif-let-tca
                    (w-cif-let-ct2, 2)    to   w-cif-let-ct5          .
           multiply  10                   by   w-cif-let-ct5          .
           add       w-cif-let-tca
                    (w-cif-let-ct2, 3)    to   w-cif-let-ct5          .
           if        w-cif-let-ct5        =    zero
                     go to cif-let-imp-600.
           perform   cif-let-imp-800      thru cif-let-imp-899        .
           go to     cif-let-imp-600.
       cif-let-imp-580.
           if        w-cif-let-tca
                    (w-cif-let-ct2, 1)    not  = zero and
                     w-cif-let-tca
                    (w-cif-let-ct2, 2)    =    8
                     subtract  1          from w-cif-let-ct1          .
           move      w-cif-let-tca
                    (w-cif-let-ct2, 2)    to   w-cif-let-ct5          .
           add       18                   to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
      *                        *---------------------------------------*
      *                        * Unita'                                *
      *                        *---------------------------------------*
           if        w-cif-let-tca
                    (w-cif-let-ct2, 3)    =    zero
                     go to cif-let-imp-600.
           if        w-cif-let-tca
                    (w-cif-let-ct2, 3)    =    1 or
                     w-cif-let-tca
                    (w-cif-let-ct2, 3)    =    8
                     subtract  1          from w-cif-let-ct1          .
           move      w-cif-let-tca
                    (w-cif-let-ct2, 3)    to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
       cif-let-imp-600.
      *                        *---------------------------------------*
      *                        * Suffisso 'mila' o 'milioni'           *
      *                        *---------------------------------------*
           if        w-cif-let-ct2        =    3
                     go to cif-let-imp-699.
           if        w-cif-let-tca
                    (w-cif-let-ct2, 3)    not  = 1
                     go to cif-let-imp-610.
           if        w-cif-let-tlt
                    (w-cif-let-ct1)       =    "O"
                     subtract  1          from w-cif-let-ct1          .
       cif-let-imp-610.
           if        w-cif-let-ct2                    =    2
                     move  30             to   w-cif-let-ct5
           else      move  32             to   w-cif-let-ct5          .
           perform   cif-let-imp-800      thru cif-let-imp-899        .
       cif-let-imp-699.
           exit.
       cif-let-imp-800.
      *                *-----------------------------------------------*
      *                * Concateno elemento nr w-cif-let-ct5           *
      *                *-----------------------------------------------*
           move      zero                 to   w-cif-let-ct6          .
       cif-let-imp-810.
           add       1                    to   w-cif-let-ct6          .
           if        w-cif-let-ct6        >    11
                     go to cif-let-imp-899.
           if        w-cif-let-lix
                    (w-cif-let-ct5, w-cif-let-ct6)
                                          =    spaces
                     go to cif-let-imp-899.
           add       1                    to   w-cif-let-ct1          .
           if        w-cif-let-ct1        >    w-cif-let-tmx
                     perform  cif-let-imp-900
                                          thru cif-let-imp-909
                     go to    cif-let-imp-899.
           move      w-cif-let-lix
                    (w-cif-let-ct5, w-cif-let-ct6)
                                          to   w-cif-let-tlt
                                              (w-cif-let-ct1)         .
           go to     cif-let-imp-810.
       cif-let-imp-899.
           exit.
       cif-let-imp-900.
      *                *-----------------------------------------------*
      *                * Superamento w-cif-let-tmx caratteri           *
      *                *-----------------------------------------------*
           move      "#"                  to   w-cif-let-flg          .
           move      spaces               to   w-cif-let-let          .
      *                    *-------------------------------------------*
      *                    * Importo editato in uscita                 *
      *                    *-------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      w-cif-let-imp        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-cif-let-ied          .
           inspect   w-cif-let-ied
                     replacing all spaces by   "*"                    .
       cif-let-imp-909.
           exit.
       cif-let-imp-999.
           exit.

