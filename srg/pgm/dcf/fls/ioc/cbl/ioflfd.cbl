       Identification Division.
       Program-Id.                                 ioflfd             .
      *================================================================*
      *                                                                *
      *                  Input-Output File lfd                         *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [fil]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fil   assign to disk           f-fil-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fil-k01
                   alternate record key   is fil-k02
                             file status  is                f-fil-sts .

      *    *===========================================================*
      *    * File Control [pul]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pul   assign to disk           f-pul-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pul-k01
                   alternate record key   is pul-k02
                             file status  is                f-pul-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [fil]                                    *
      *    *-----------------------------------------------------------*
       fd  fil       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fil-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fil-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTFNTDVF                      *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-rec        pic  9(02)                  .
                   15  fil-cod-dcf        pic  9(07)       comp-3     .
                   15  fil-sgl-vlt        pic  x(03)                  .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
                   15  fil-fda-pif        pic  x(14)                  .
                   15  fil-dva-fin        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : LSTFNTDFR                      *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-tip-rec-2      pic  9(02)                  .
                   15  fil-cod-dcf-2      pic  9(07)       comp-3     .
                   15  fil-sgl-vlt-2      pic  x(03)                  .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-num-mag-2      pic  9(07)       comp-3     .
                   15  fil-fda-pif-2      pic  x(14)                  .
                   15  fil-dvf-rev        pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-inf-pes.
                   15  fil-tbl-pes.
                       20  fil-ele-pes occurs 06.
                           25  fil-qta-pes
                                          pic  9(06)v9(03) comp-3     .
                           25  fil-prz-pes
                                          pic  9(09)       comp-3     .
                           25  fil-csr-pes
                                          pic  9(05)       comp-3     .
                           25  fil-psr-pes occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  fil-dva-ini            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [pul]                                    *
      *    *-----------------------------------------------------------*
       fd  pul       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  pul-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  pul-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTFNTDVF                      *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-rec        pic  9(02)                  .
                   15  pul-cod-dcf        pic  9(07)       comp-3     .
                   15  pul-sgl-vlt        pic  x(03)                  .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
                   15  pul-fda-pif        pic  x(14)                  .
                   15  pul-dva-fin        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : LSTFNTDFR                      *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-tip-rec-2      pic  9(02)                  .
                   15  pul-cod-dcf-2      pic  9(07)       comp-3     .
                   15  pul-sgl-vlt-2      pic  x(03)                  .
                   15  pul-tip-mag-2      pic  9(02)                  .
                   15  pul-num-mag-2      pic  9(07)       comp-3     .
                   15  pul-fda-pif-2      pic  x(14)                  .
                   15  pul-dvf-rev        pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-inf-pes.
                   15  pul-tbl-pes.
                       20  pul-ele-pes occurs 06.
                           25  pul-qta-pes
                                          pic  9(06)v9(03) comp-3     .
                           25  pul-prz-pes
                                          pic  9(09)       comp-3     .
                           25  pul-csr-pes
                                          pic  9(05)       comp-3     .
                           25  pul-psr-pes occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  pul-dva-ini            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sigla del File                                        *
      *        *-------------------------------------------------------*
           02  i-ide-sdf                  pic  x(04) value
                     "lfd "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcf/fls/ioc/obj/ioflfd              "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work-area fissa per tutti i moduli di gestione i-o        *
      *    *-----------------------------------------------------------*
           copy      "swd/ske/iof/iofcp30"                            .

      *    *===========================================================*
      *    * Area Lunghezza record in bytes ed Elenco chiavi previste  *
      *    *-----------------------------------------------------------*
       01  k.
      *        *-------------------------------------------------------*
      *        * Numero chiavi di accesso                              *
      *        *-------------------------------------------------------*
           05  k-ctr                      pic  9(02) value 2          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "LSTFNTDVF "                             .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "LSTFNTDFR "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    2      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-are-loc.
      *        *-------------------------------------------------------*
      *        * Indici di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-inx-00i                  pic  9(02)                  .
           05  w-inx-00j                  pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [lfd]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rflfd"                          .

      ******************************************************************
       Procedure Division                using f rf-lfd               .
      ******************************************************************

      *    *===========================================================*
      *    * Procedure division fissa per tutti i moduli di i-o        *
      *    *-----------------------------------------------------------*
           copy      "swd/ske/iof/iofcp50"                            .

      *    *===========================================================*
      *    * Start su chiave                                           *
      *    *-----------------------------------------------------------*
       str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status                          *
      *              *-------------------------------------------------*
           move      "00"                 to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Selezione indice sequenza di accesso            *
      *              *-------------------------------------------------*
           if        f-cfr                =    "NG"
                     move   3             to   z-tco
           else if   f-cfr                =    "GT"
                     move   2             to   z-tco
           else      move   1             to   z-tco                  .
      *              *-------------------------------------------------*
      *              * Composizione chiave fisica                      *
      *              *-------------------------------------------------*
           perform   cmp-key-fis-000      thru cmp-key-fis-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'indice z-key        *
      *              *-------------------------------------------------*
           go to     str-100
                     str-200
                     depending            on   z-key                  .
       str-100.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 1                       *
      *              *-------------------------------------------------*
           go to     str-110
                     str-120
                     str-130
                     depending            on   z-tco                  .
       str-110.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 1           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k01
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-120.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 1           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k01
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-130.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 1           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k01
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-200.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 2                       *
      *              *-------------------------------------------------*
           go to     str-210
                     str-220
                     str-230
                     depending            on   z-tco                  .
       str-210.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 2           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k02
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-220.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 2           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k02
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-230.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 2           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k02
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-980.
      *              *-------------------------------------------------*
      *              * Non invalid key                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
      *                  *---------------------------------------------*
      *                  * Ogni i-o error e' considerato fatal error   *
      *                  *---------------------------------------------*
           if        e-sts                =    "00"
                     go to str-999
           else      perform fte-000      thru fte-999                .
       str-990.
      *              *-------------------------------------------------*
      *              * Invalid key                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-end-fil            to   f-sts                  .
       str-999.
           exit.

      *    *===========================================================*
      *    * Read record generica su chiave                            *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave fisica                      *
      *              *-------------------------------------------------*
           perform   cmp-key-fis-000      thru cmp-key-fis-999        .
       rea-010.
      *              *-------------------------------------------------*
      *              * Normalizzazione status                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'indice z-key        *
      *              *-------------------------------------------------*
           go to     rea-100
                     rea-200
                     depending            on   z-key                  .
       rea-100.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 1                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-110.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k01
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-110.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k01
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-200.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 2                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-210.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k02
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-210.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k02
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-980.
      *              *-------------------------------------------------*
      *              * Non invalid key                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
      *                  *---------------------------------------------*
      *                  * Se record locked si esegue una pausa di un  *
      *                  * secondo e poi si ritorna a rileggere        *
      *                  *---------------------------------------------*
           if        e-sts                =    e-use-err
                     perform wai-000      thru wai-999
                     go to   rea-010.
      *                  *---------------------------------------------*
      *                  * Ogni altro i-o error viene considerato un   *
      *                  * fatal error                                 *
      *                  *---------------------------------------------*
           if        e-sts                not  = "00"
                     perform fte-000      thru fte-999                .
      *                  *---------------------------------------------*
      *                  * Se richiesta la decomposizione da record    *
      *                  * fisico a record logico la si esegue         *
      *                  *---------------------------------------------*
           if        z-dec                =    1
                     perform dec-fis-log-000
                        thru dec-fis-log-999.
           go to     rea-999.
       rea-990.
      *              *-------------------------------------------------*
      *              * Invalid key                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   f-sts                 .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-lfd                 .
           move      zero                 to   rf-lfd-tip-rec         .
           move      zero                 to   rf-lfd-cod-dcf         .
           move      spaces               to   rf-lfd-sgl-vlt         .
           move      zero                 to   rf-lfd-dec-vlt         .
           move      zero                 to   rf-lfd-tip-mag         .
           move      zero                 to   rf-lfd-num-mag         .
           move      spaces               to   rf-lfd-fda-pif         .
       nor-rec-log-100.
           move      zero                 to   w-inx-00i              .
       nor-rec-log-110.
           add       1                    to   w-inx-00i              .
           if        w-inx-00i            >    06
                     go to nor-rec-log-500.
           move      zero                 to   rf-lfd-qta-pes
                                              (w-inx-00i)             .
           move      zero                 to   rf-lfd-prz-pes
                                              (w-inx-00i)             .
           move      zero                 to   rf-lfd-csr-pes
                                              (w-inx-00i)             .
       nor-rec-log-120.
           move      zero                 to   w-inx-00j              .
       nor-rec-log-130.
           add       1                    to   w-inx-00j              .
           if        w-inx-00j            >    05
                     go to nor-rec-log-140.
           move      zero                 to   rf-lfd-psr-pes
                                              (w-inx-00i,
                                               w-inx-00j)             .
           go to     nor-rec-log-130.
       nor-rec-log-140.
           go to     nor-rec-log-110.
       nor-rec-log-500.
           move      zero                 to   rf-lfd-dva-ini         .
           move      zero                 to   rf-lfd-dva-fin         .
           move      zero                 to   rf-lfd-dvf-rev         .
           move      spaces               to   rf-lfd-alx-exp         .
       nor-rec-log-999.
           exit.

      *    *===========================================================*
      *    * Composizione record da logico a fisico                    *
      *    *-----------------------------------------------------------*
       cmp-log-fis-000.
      *              *-------------------------------------------------*
      *              * Spaces in tutto il record fisico                *
      *              *-------------------------------------------------*
           move      spaces               to   fil-rec                .
      *              *-------------------------------------------------*
      *              * Composizione area chiavi                        *
      *              *-------------------------------------------------*
           move      zero                 to   z-key                  .
       cmp-log-fis-100.
           if        z-key                <    k-ctr
                     add     1            to   z-key
                     perform cmp-key-fis-000
                        thru cmp-key-fis-999
                     go to   cmp-log-fis-100.
       cmp-log-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione area dati                          *
      *              *-------------------------------------------------*
       cmp-log-fis-210.
           move      zero                 to   z-key                  .
           move      rf-lfd-dec-vlt       to   fil-dec-vlt            .
       cmp-log-fis-220.
           move      zero                 to   w-inx-00i              .
       cmp-log-fis-230.
           add       1                    to   w-inx-00i              .
           if        w-inx-00i            >    06
                     go to cmp-log-fis-400.
           move      rf-lfd-qta-pes
                    (w-inx-00i)           to   fil-qta-pes
                                              (w-inx-00i)             .
           move      rf-lfd-prz-pes
                    (w-inx-00i)           to   fil-prz-pes
                                              (w-inx-00i)             .
           move      rf-lfd-csr-pes
                    (w-inx-00i)           to   fil-csr-pes
                                              (w-inx-00i)             .
       cmp-log-fis-240.
           move      zero                 to   w-inx-00j              .
       cmp-log-fis-250.
           add       1                    to   w-inx-00j              .
           if        w-inx-00j            >    05
                     go to cmp-log-fis-260.
           move      rf-lfd-psr-pes
                    (w-inx-00i,
                     w-inx-00j)           to   fil-psr-pes
                                              (w-inx-00i,
                                               w-inx-00j)             .
           go to     cmp-log-fis-250.
       cmp-log-fis-260.
           go to     cmp-log-fis-230.
       cmp-log-fis-400.
           move      rf-lfd-dva-ini              to   fil-dva-ini     .
           move      rf-lfd-alx-exp       to   fil-alx-exp            .
       cmp-log-fis-999.
           exit.

      *    *===========================================================*
      *    * Composizione chiave da logica a fisica secondo z-key      *
      *    *-----------------------------------------------------------*
       cmp-key-fis-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'indice z-key        *
      *              *-------------------------------------------------*
           go to     cmp-key-fis-100
                     cmp-key-fis-200
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-lfd-tip-rec       to   fil-tip-rec            .
           move      rf-lfd-cod-dcf       to   fil-cod-dcf            .
           move      rf-lfd-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-lfd-tip-mag       to   fil-tip-mag            .
           move      rf-lfd-num-mag       to   fil-num-mag            .
           move      rf-lfd-fda-pif       to   fil-fda-pif            .
           move      rf-lfd-dva-fin       to   fil-dva-fin            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-lfd-tip-rec       to   fil-tip-rec-2          .
           move      rf-lfd-cod-dcf       to   fil-cod-dcf-2          .
           move      rf-lfd-sgl-vlt       to   fil-sgl-vlt-2          .
           move      rf-lfd-tip-mag       to   fil-tip-mag-2          .
           move      rf-lfd-num-mag       to   fil-num-mag-2          .
           move      rf-lfd-fda-pif       to   fil-fda-pif-2          .
           move      rf-lfd-dvf-rev       to   fil-dvf-rev            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-lfd                 .
           move      fil-tip-rec          to   rf-lfd-tip-rec         .
           move      fil-cod-dcf          to   rf-lfd-cod-dcf         .
           move      fil-sgl-vlt          to   rf-lfd-sgl-vlt         .
           move      fil-dec-vlt          to   rf-lfd-dec-vlt         .
           move      fil-tip-mag          to   rf-lfd-tip-mag         .
           move      fil-num-mag          to   rf-lfd-num-mag         .
           move      fil-fda-pif          to   rf-lfd-fda-pif         .
       dec-fis-log-100.
           move      zero                 to   w-inx-00i              .
       dec-fis-log-110.
           add       1                    to   w-inx-00i              .
           if        w-inx-00i            >    06
                     go to dec-fis-log-500.
           move      fil-qta-pes
                    (w-inx-00i)           to   rf-lfd-qta-pes
                                              (w-inx-00i)             .
           move      fil-prz-pes
                    (w-inx-00i)           to   rf-lfd-prz-pes
                                              (w-inx-00i)             .
           move      fil-csr-pes
                    (w-inx-00i)           to   rf-lfd-csr-pes
                                              (w-inx-00i)             .
       dec-fis-log-120.
           move      zero                 to   w-inx-00j              .
       dec-fis-log-130.
           add       1                    to   w-inx-00j              .
           if        w-inx-00j            >    05
                     go to dec-fis-log-140.
           move      fil-psr-pes
                    (w-inx-00i,
                     w-inx-00j)           to   rf-lfd-psr-pes
                                              (w-inx-00i,
                                               w-inx-00j)             .
           go to     dec-fis-log-130.
       dec-fis-log-140.
           go to     dec-fis-log-110.
       dec-fis-log-500.
           move      fil-dva-ini          to   rf-lfd-dva-ini         .
           move      fil-dva-fin          to   rf-lfd-dva-fin         .
           move      fil-dvf-rev          to   rf-lfd-dvf-rev         .
           move      fil-alx-exp          to   rf-lfd-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-lfd               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-lfd
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

