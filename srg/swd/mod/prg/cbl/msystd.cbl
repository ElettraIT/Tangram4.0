       Identification Division.
       Program-Id.                                 msystd             .
      *================================================================*
      *                                                                *
      * Trattamento data in formato s.aa.mm.gg                         *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - DT : Estrazione data di sistema attuale con il secolo nor-   *
      *        normalizzato                                            *
      *                                                                *
      *        Input  : l-ope = "DT"                                   *
      *                                                                *
      *                 l-cdt = correttivo giorni                      *
      *                                                                *
      *                 l-chh = correttivo ore                         *
      *                                                                *
      *                                                                *
      *        Output : l-det = System date and time attuale           *
      *                                                                *
      *                 - l-dat = System date                          *
      *                 - l-tim = System time                          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - NS : Normalizzazione secolo nella data                       *
      *                                                                *
      *        Input  : l-ope = "NS"                                   *
      *                                                                *
      *                 l-dat = Data s.aa.mm.gg da normalizzare        *
      *                                                                *
      *        Output : l-dat = Data s.aa.mm.gg con s. normalizzato    *
      *                                                                *
      *             -------------------------------------------------- *
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
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Data nel formato s.aa.mm.gg                           *
      *        *-------------------------------------------------------*
           05  w-dat                      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione                                         *
      *        *-------------------------------------------------------*
           05  w-dtr redefines
               w-dat.
      *            *---------------------------------------------------*
      *            * Secolo                                            *
      *            *---------------------------------------------------*
               10  w-sec                  pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Data in formato aa.mm.gg                          *
      *            *---------------------------------------------------*
               10  w-amg                  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione                                     *
      *            *---------------------------------------------------*
               10  w-amr redefines
                   w-amg.
      *                *-----------------------------------------------*
      *                * Anno                                          *
      *                *-----------------------------------------------*
                   15  w-ann              pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Mese                                          *
      *                *-----------------------------------------------*
                   15  w-mes              pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Giorno                                        *
      *                *-----------------------------------------------*
                   15  w-gio              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per correttivo data e ora di sistema           *
      *        *-------------------------------------------------------*
           05  w-chs                      pic s9(09)                  .
           05  w-chh                      pic  9(02)                  .
           05  w-sts                      pic  x(01)                  .
           05  w-pnt                      pic  9(05)                  .
           05  w-ctr                      pic  9(05)                  .
           05  w-tim                      pic  9(08)                  .
           05  w-tir redefines w-tim.
               10  w-ora                  pic  9(02)                  .
               10  w-min                  pic  9(02)                  .
               10  w-scd                  pic  9(02)                  .
               10  w-cen                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tabella giorni del mese                               *
      *        *-------------------------------------------------------*
           05  w-tgm.
               10  w-tgn.
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 28         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
               10  w-tgo redefines
                   w-tgn.
                   15  w-tgp occurs 12    pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di interfaccia per                          "msystd" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/l"                                  .

      ******************************************************************
       Procedure Division                using l                      .
      ******************************************************************

      *================================================================*
      *    Main                                                        *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione data di sistema attuale con secolo   *
      *              * normalizzato                                    *
      *              *-------------------------------------------------*
           if        l-ope                =    "DT"
                     perform est-det-000  thru est-det-999
      *              *-------------------------------------------------*
      *              * Normalizzazione secolo nella data               *
      *              *-------------------------------------------------*
           else if   l-ope                =    "NS"
                     perform nor-sec-000  thru nor-sec-999            .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *----------------------------------------------------------------*

      *    *===========================================================*
      *    * Estrazione data di sistema attuale con secolo normalizza- *
      *    * to, e ora di sistema                                      *
      *    *-----------------------------------------------------------*
       est-det-000.
      *              *-------------------------------------------------*
      *              * Accettazione system date                        *
      *              *-------------------------------------------------*
           accept    w-amg                from date                   .
           move      w-amg                to   l-dat                  .
      *              *-------------------------------------------------*
      *              * Accettazione system time                        *
      *              *-------------------------------------------------*
           accept    l-tim                from time                   .
       est-det-400.
      *              *-------------------------------------------------*
      *              * In presenza di appositi correttivi, desunti     *
      *              * dalle personalizzazioni generali, la data e     *
      *              * l'ora di sistema possono essere ritarate        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Correttivo data di sistema                  *
      *                  *---------------------------------------------*
           if        l-cdt                =    zero
                     go to est-det-600.
           perform   cor-dat-000          thru cor-dat-999            .
       est-det-600.
      *                  *---------------------------------------------*
      *                  * Correttivo ora di sistema                   *
      *                  *---------------------------------------------*
           if        l-chh                =    zero
                     go to est-det-800.
           move      l-tim                to   w-tim                  .
           if        l-chh                >    zero
                     go to est-det-620.
       est-det-610.
      *                  *---------------------------------------------*
      *                  * In meno                                     *
      *                  *---------------------------------------------*
           move      l-chh                to   w-chh                  .
           if        w-chh                >    w-ora
                     add  24              to   w-ora
                     subtract w-chh       from w-ora
           else      subtract w-chh       from w-ora                  .
           move      w-tim                to   l-tim                  .
           go to     est-det-800.
       est-det-620.
      *                  *---------------------------------------------*
      *                  * In piu'                                     *
      *                  *---------------------------------------------*
           move      l-chh                to   w-chh                  .
           add       w-chh                to   w-ora                  .
           if        w-ora                >    24
                     subtract  24         from w-ora                  .
           move      w-tim                to   l-tim                  .
           go to     est-det-800.
       est-det-800.
      *              *-------------------------------------------------*
      *              * Normalizzazione secolo nella data               *
      *              *-------------------------------------------------*
           perform   nor-sec-000          thru nor-sec-999            .
       est-det-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione secolo nella data                         *
      *    *-----------------------------------------------------------*
       nor-sec-000.
      *              *-------------------------------------------------*
      *              *                                                 *
      *              * Si pone la codifica del secolo come segue :     *
      *              *                                                 *
      *              * - 0 : 1900                                      *
      *              * - 1 : 2000                                      *
      *              * - 2 : 2100                                      *
      *              *                                                 *
      *              * ----------------------------------------------- *
      *              *                                                 *
      *              * La normalizzazione del secolo nella data avvie- *
      *              * ne come segue : se l'anno e' minore di 85 si    *
      *              * assume il secolo relativo al 2000, altrimenti   *
      *              * si assume il secolo relativo al 1900. Se pero'  *
      *              * l'intera data in formato aa.mm.gg e' a zero, si *
      *              * pone a zero anche il secolo                     *
      *              *                                                 *
      *              *-------------------------------------------------*
           move      l-dat                to   w-dat                  .
           if        w-ann                <    85 and
                     w-dat                >    zero
                     move   1             to   w-sec
           else      move   zero          to   w-sec                  .
           move      w-dat                to   l-dat                  .
       nor-sec-999.
           exit.

      *    *===========================================================*
      *    * Correzione data                                           *
      *    *                                                           *
      *    * Nota bene : presume che il gap sia al massimo un giorno   *
      *    *             e in meno                                     *
      *    *                                                           *
      *    * ___ DA SISTEMARE ___                                      *
      *    *                                                           *
      *    * MANCA UN TEST SE IL GAP E' IN + E MANCA TUTTA LA ROUTINE  *
      *    * DI CALCOLO PER DATA + GIORNI                              *
      *    *-----------------------------------------------------------*
       cor-dat-000.
           move      w-dat                to   w-det-nrg-dat-dtb      .
           move      l-cdt                to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
           move      w-det-nrg-dat-dtd    to   l-dat                  .
       cor-dat-999.
           exit.

      *    *===========================================================*
      *    * Determinazione (data) - (nr. giorni) = (data)             *
      *    *                                                           *
      *    * N.B.: Questa routine e' identica a quella usata in copy   *
      *    *       dai programmi, salvo per la chiamata di controllo   *
      *    *       della data alla segreteria                          *
      *    *-----------------------------------------------------------*
       det-nrg-dat-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-nrg-dat-dtb    to   w-det-nrg-dat-dtd      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di decremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-ngd    =    zero
                     go to det-nrg-dat-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-nrg-dat-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nrg-dat-pgu      .
       det-nrg-dat-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura data di inizio mese               *
      *                  *---------------------------------------------*
           move      01                   to   w-det-nrg-dat-ddg      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-nrg-dat-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-nrg-dat-fpp    =    spaces
                     go to det-nrg-dat-122
           else      go to det-nrg-dat-124.
       det-nrg-dat-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-nrg-dat-fpp      .
           subtract  w-det-nrg-dat-ddg    from w-det-nrg-dat-dbg
                                        giving w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero giorni del    *
      *                          * mese                                *
      *                          *-------------------------------------*
           move      w-det-nrg-dat-dtd    to   w-dat                  .
           move      31                   to   w-gio                  .
       det-nrg-dat-125.
      *                          *-------------------------------------*
      *                          * Controllo della data                *
      *                          *-------------------------------------*
           perform   cnt-dat-000          thru cnt-dat-999            .
      *                              *---------------------------------*
      *                              * Esito della lettura             *
      *                              *---------------------------------*
           if        w-sts                not  = spaces
                     subtract 1           from w-gio
                     go to det-nrg-dat-125.
           move      w-gio                to   w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di decremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-nrg-dat-pgu    not  < w-det-nrg-dat-ngd
                     go to det-nrg-dat-150.
      *                  *---------------------------------------------*
      *                  * Decremento mese/anno                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-det-nrg-dat-ddm      .
           if        w-det-nrg-dat-ddm    =    zero
                     move  12             to   w-det-nrg-dat-ddm
                     subtract 1           from w-det-nrg-dat-dda      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-nrg-dat-100.
       det-nrg-dat-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-nrg-dat-ngd    from w-det-nrg-dat-pgu
                                        giving w-det-nrg-dat-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data decrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-ddg      .
       det-nrg-dat-999.
           exit.

      *    *===========================================================*
      *    * Controllo data                                            *
      *    *                                                           *
      *    * Copiato da segreteria                                     *
      *    *-----------------------------------------------------------*
       cnt-dat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-sts                  .
      *              *-------------------------------------------------*
      *              * Se data a zero : uscita                         *
      *              *-------------------------------------------------*
           if        w-dat                =    zero
                     go to cnt-dat-999.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari per errori nella data     *
      *              *-------------------------------------------------*
           if        w-sec                >    1    or
                     w-mes                =    zero or
                     w-mes                >    12   or
                     w-gio                =    zero or
                     w-gio                >    31
                     go to cnt-dat-900.
      *              *-------------------------------------------------*
      *              * Test per mese 2 - febbraio                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo per l'anno bisestile                *
      *                  *---------------------------------------------*
           if        w-mes                =    02
                     divide 4             into w-ann
                                        giving w-ctr
                                     remainder w-pnt
      *                  *---------------------------------------------*
      *                  * Se la divisione per 4 ha restituito zero    *
      *                  *---------------------------------------------*
                     if   w-pnt           =    zero
      *                      *-----------------------------------------*
      *                      * Se l'anno e' zero                       *
      *                      *-----------------------------------------*
                          if   w-ann      =    zero
      *                          *-------------------------------------*
      *                          * Se il secolo e' zero                *
      *                          *-------------------------------------*
                               if   w-sec =    zero
                                    move 28
                                          to   w-tgp (w-mes)
                               else move 29
                                          to   w-tgp (w-mes)
                          else move 29    to   w-tgp (w-mes)
                     else move 28         to   w-tgp (w-mes)          .
      *              *-------------------------------------------------*
      *              * Test finale                                     *
      *              *-------------------------------------------------*
           if        w-gio                not  > w-tgp (w-mes)
                     go to cnt-dat-999.
       cnt-dat-900.
      *              *-------------------------------------------------*
      *              * Flag di errore in uscita                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sts                  .
       cnt-dat-999.
           exit.

