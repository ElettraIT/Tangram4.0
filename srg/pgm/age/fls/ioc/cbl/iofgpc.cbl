       Identification Division.
       Program-Id.                                 iofgpc             .
      *================================================================*
      *                                                                *
      *                  Input-Output File gpc                         *
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
                   alternate record key   is fil-k03
                   alternate record key   is fil-k04
                   alternate record key   is fil-k05
                   alternate record key   is fil-k06
                             file status  is                f-fil-sts .

      *    *===========================================================*
      *    * File Control [pul]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pul   assign to disk           f-pul-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pul-k01
                   alternate record key   is pul-k02
                   alternate record key   is pul-k03
                   alternate record key   is pul-k04
                   alternate record key   is pul-k05
                   alternate record key   is pul-k06
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
      *            * Chiave numero 01 : NUMCTG                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-ctg        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-ctg-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DENDOC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-doc        pic  x(10)                  .
                   15  fil-num-ctg-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ADNDOC                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-age        pic  9(07)       comp-3     .
                   15  fil-dat-doc-4      pic  9(07)       comp-3     .
                   15  fil-num-doc-4      pic  x(10)                  .
                   15  fil-num-ctg-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CDNDOC                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-cli        pic  9(07)       comp-3     .
                   15  fil-dat-doc-5      pic  9(07)       comp-3     .
                   15  fil-num-doc-5      pic  x(10)                  .
                   15  fil-num-ctg-5      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : SDNDOC                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-plf        pic  9(07)       comp-3     .
                   15  fil-dat-doc-6      pic  9(07)       comp-3     .
                   15  fil-num-doc-6      pic  x(10)                  .
                   15  fil-num-ctg-6      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-prt-fcl            pic  9(09)       comp-3     .
               10  fil-tip-ctg            pic  9(02)                  .
               10  fil-tip-vpa            pic  9(02)                  .
               10  fil-dpz-cli            pic  x(04)                  .
               10  fil-ibl-pvg            pic s9(11)       comp-3     .
               10  fil-per-pvg            pic  9(02)v9(01) comp-3     .
               10  fil-amm-pvg            pic s9(11)       comp-3     .
               10  fil-imp-doc            pic s9(11)       comp-3     .
               10  fil-imp-agf            pic s9(11)       comp-3     .
               10  fil-not-ctg            pic  x(40)                  .
               10  fil-tip-mat            pic  9(02)                  .
               10  fil-mat-blo            pic  9(02)                  .
               10  fil-ddm-min            pic  9(07)       comp-3     .
               10  fil-cod-ags            pic  9(07)       comp-3     .
               10  fil-dpz-plf            pic  x(04)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMCTG                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-ctg        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-ctg-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DENDOC                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-doc        pic  x(10)                  .
                   15  pul-num-ctg-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ADNDOC                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-age        pic  9(07)       comp-3     .
                   15  pul-dat-doc-4      pic  9(07)       comp-3     .
                   15  pul-num-doc-4      pic  x(10)                  .
                   15  pul-num-ctg-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CDNDOC                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-cli        pic  9(07)       comp-3     .
                   15  pul-dat-doc-5      pic  9(07)       comp-3     .
                   15  pul-num-doc-5      pic  x(10)                  .
                   15  pul-num-ctg-5      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : SDNDOC                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-plf        pic  9(07)       comp-3     .
                   15  pul-dat-doc-6      pic  9(07)       comp-3     .
                   15  pul-num-doc-6      pic  x(10)                  .
                   15  pul-num-ctg-6      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-prt-fcl            pic  9(09)       comp-3     .
               10  pul-tip-ctg            pic  9(02)                  .
               10  pul-tip-vpa            pic  9(02)                  .
               10  pul-dpz-cli            pic  x(04)                  .
               10  pul-ibl-pvg            pic s9(11)       comp-3     .
               10  pul-per-pvg            pic  9(02)v9(01) comp-3     .
               10  pul-amm-pvg            pic s9(11)       comp-3     .
               10  pul-imp-doc            pic s9(11)       comp-3     .
               10  pul-imp-agf            pic s9(11)       comp-3     .
               10  pul-not-ctg            pic  x(40)                  .
               10  pul-tip-mat            pic  9(02)                  .
               10  pul-mat-blo            pic  9(02)                  .
               10  pul-ddm-min            pic  9(07)       comp-3     .
               10  pul-cod-ags            pic  9(07)       comp-3     .
               10  pul-dpz-plf            pic  x(04)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
                     "gpc "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/age/fls/ioc/obj/iofgpc              "       .

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
           05  k-ctr                      pic  9(02) value 6          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMCTG    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DENDOC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ADNDOC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CDNDOC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "SDNDOC    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    6      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [gpc]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpc"                          .

      ******************************************************************
       Procedure Division                using f rf-gpc               .
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
                     str-300
                     str-400
                     str-500
                     str-600
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
       str-300.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 3                       *
      *              *-------------------------------------------------*
           go to     str-310
                     str-320
                     str-330
                     depending            on   z-tco                  .
       str-310.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 3           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k03
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-320.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 3           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k03
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-330.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 3           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k03
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-400.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 4                       *
      *              *-------------------------------------------------*
           go to     str-410
                     str-420
                     str-430
                     depending            on   z-tco                  .
       str-410.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-420.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-430.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-500.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 5                       *
      *              *-------------------------------------------------*
           go to     str-510
                     str-520
                     str-530
                     depending            on   z-tco                  .
       str-510.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-520.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-530.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-600.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 6                       *
      *              *-------------------------------------------------*
           go to     str-610
                     str-620
                     str-630
                     depending            on   z-tco                  .
       str-610.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-620.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-630.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k06
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
                     rea-300
                     rea-400
                     rea-500
                     rea-600
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
       rea-300.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 3                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-310.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k03
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-310.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k03
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-400.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 4                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-410.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k04
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-410.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k04
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-500.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 5                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-510.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k05
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-510.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k05
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-600.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 6                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-610.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k06
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-610.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k06
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
           move      spaces               to   rf-gpc                 .
           move      zero                 to   rf-gpc-ide-dat         .
           move      spaces               to   rf-gpc-ide-ute         .
           move      spaces               to   rf-gpc-ide-fas         .
           move      zero                 to   rf-gpc-num-ctg         .
           move      zero                 to   rf-gpc-prt-fcl         .
           move      zero                 to   rf-gpc-tip-ctg         .
           move      zero                 to   rf-gpc-tip-vpa         .
           move      zero                 to   rf-gpc-dat-doc         .
           move      spaces               to   rf-gpc-num-doc         .
           move      zero                 to   rf-gpc-cod-age         .
           move      zero                 to   rf-gpc-cod-cli         .
           move      spaces               to   rf-gpc-dpz-cli         .
           move      zero                 to   rf-gpc-ibl-pvg         .
           move      zero                 to   rf-gpc-per-pvg         .
           move      zero                 to   rf-gpc-amm-pvg         .
           move      zero                 to   rf-gpc-imp-doc         .
           move      zero                 to   rf-gpc-imp-agf         .
           move      spaces               to   rf-gpc-not-ctg         .
           move      zero                 to   rf-gpc-tip-mat         .
           move      zero                 to   rf-gpc-mat-blo         .
           move      zero                 to   rf-gpc-ddm-min         .
           move      zero                 to   rf-gpc-cod-ags         .
           move      zero                 to   rf-gpc-cod-plf         .
           move      spaces               to   rf-gpc-dpz-plf         .
           move      spaces               to   rf-gpc-flg-ela         .
           move      spaces               to   rf-gpc-flg-pul         .
           move      spaces               to   rf-gpc-alx-exp         .
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
      *              *-------------------------------------------------*
      *              * Composizione area dati                          *
      *              *-------------------------------------------------*
           move      zero                 to   z-key                  .
           move      rf-gpc-ide-ute       to   fil-ide-ute            .
           move      rf-gpc-ide-fas       to   fil-ide-fas            .
           move      rf-gpc-prt-fcl       to   fil-prt-fcl            .
           move      rf-gpc-tip-ctg       to   fil-tip-ctg            .
           move      rf-gpc-tip-vpa       to   fil-tip-vpa            .
           move      rf-gpc-dpz-cli       to   fil-dpz-cli            .
           move      rf-gpc-ibl-pvg       to   fil-ibl-pvg            .
           move      rf-gpc-per-pvg       to   fil-per-pvg            .
           move      rf-gpc-amm-pvg       to   fil-amm-pvg            .
           move      rf-gpc-imp-doc       to   fil-imp-doc            .
           move      rf-gpc-imp-agf       to   fil-imp-agf            .
           move      rf-gpc-not-ctg       to   fil-not-ctg            .
           move      rf-gpc-tip-mat       to   fil-tip-mat            .
           move      rf-gpc-mat-blo       to   fil-mat-blo            .
           move      rf-gpc-ddm-min       to   fil-ddm-min            .
           move      rf-gpc-cod-ags       to   fil-cod-ags            .
           move      rf-gpc-dpz-plf       to   fil-dpz-plf            .
           move      rf-gpc-flg-ela       to   fil-flg-ela            .
           move      rf-gpc-flg-pul       to   fil-flg-pul            .
           move      rf-gpc-alx-exp       to   fil-alx-exp            .
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
                     cmp-key-fis-300
                     cmp-key-fis-400
                     cmp-key-fis-500
                     cmp-key-fis-600
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-gpc-num-ctg       to   fil-num-ctg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-gpc-ide-dat       to   fil-ide-dat            .
           move      rf-gpc-num-ctg       to   fil-num-ctg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-gpc-dat-doc       to   fil-dat-doc            .
           move      rf-gpc-num-doc       to   fil-num-doc            .
           move      rf-gpc-num-ctg       to   fil-num-ctg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-gpc-cod-age       to   fil-cod-age            .
           move      rf-gpc-dat-doc       to   fil-dat-doc-4          .
           move      rf-gpc-num-doc       to   fil-num-doc-4          .
           move      rf-gpc-num-ctg       to   fil-num-ctg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-gpc-cod-cli       to   fil-cod-cli            .
           move      rf-gpc-dat-doc       to   fil-dat-doc-5          .
           move      rf-gpc-num-doc       to   fil-num-doc-5          .
           move      rf-gpc-num-ctg       to   fil-num-ctg-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-gpc-cod-plf       to   fil-cod-plf            .
           move      rf-gpc-dat-doc       to   fil-dat-doc-6          .
           move      rf-gpc-num-doc       to   fil-num-doc-6          .
           move      rf-gpc-num-ctg       to   fil-num-ctg-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-gpc                 .
           move      fil-ide-dat          to   rf-gpc-ide-dat         .
           move      fil-ide-ute          to   rf-gpc-ide-ute         .
           move      fil-ide-fas          to   rf-gpc-ide-fas         .
           move      fil-num-ctg          to   rf-gpc-num-ctg         .
           move      fil-prt-fcl          to   rf-gpc-prt-fcl         .
           move      fil-tip-ctg          to   rf-gpc-tip-ctg         .
           move      fil-tip-vpa          to   rf-gpc-tip-vpa         .
           move      fil-dat-doc          to   rf-gpc-dat-doc         .
           move      fil-num-doc          to   rf-gpc-num-doc         .
           move      fil-cod-age          to   rf-gpc-cod-age         .
           move      fil-cod-cli          to   rf-gpc-cod-cli         .
           move      fil-dpz-cli          to   rf-gpc-dpz-cli         .
           move      fil-ibl-pvg          to   rf-gpc-ibl-pvg         .
           move      fil-per-pvg          to   rf-gpc-per-pvg         .
           move      fil-amm-pvg          to   rf-gpc-amm-pvg         .
           move      fil-imp-doc          to   rf-gpc-imp-doc         .
           move      fil-imp-agf          to   rf-gpc-imp-agf         .
           move      fil-not-ctg          to   rf-gpc-not-ctg         .
           move      fil-tip-mat          to   rf-gpc-tip-mat         .
           move      fil-mat-blo          to   rf-gpc-mat-blo         .
           move      fil-ddm-min          to   rf-gpc-ddm-min         .
           move      fil-cod-ags          to   rf-gpc-cod-ags         .
           move      fil-cod-plf          to   rf-gpc-cod-plf         .
           move      fil-dpz-plf          to   rf-gpc-dpz-plf         .
           move      fil-flg-ela          to   rf-gpc-flg-ela         .
           move      fil-flg-pul          to   rf-gpc-flg-pul         .
           move      fil-alx-exp          to   rf-gpc-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-gpc               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-gpc
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
