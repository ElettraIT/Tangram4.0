       Identification Division.
       Program-Id.                                 iofage             .
      *================================================================*
      *                                                                *
      *                  Input-Output File age                         *
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
      *            * Chiave numero 01 : CODAGE                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-age        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-cod-age-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-rag-key        pic  x(40)                  .
                   15  fil-cod-age-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-mne        pic  x(10)                  .
                   15  fil-cod-age-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : NOMKEY                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-nom-key        pic  x(20)                  .
                   15  fil-cod-age-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : SUPAGE                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-sup-age        pic  9(07)       comp-3     .
                   15  fil-cod-age-6      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-nom-age            pic  x(20)                  .
               10  fil-rag-soc            pic  x(40)                  .
               10  fil-via-age            pic  x(40)                  .
               10  fil-loc-age            pic  x(40)                  .
               10  fil-cod-naz            pic  x(03)                  .
               10  fil-cod-cmn            pic  9(05)       comp-3     .
               10  fil-cod-fzn            pic  9(03)       comp-3     .
               10  fil-cod-lct            pic  9(03)       comp-3     .
               10  fil-num-tel            pic  x(20)                  .
               10  fil-num-fax            pic  x(20)                  .
               10  fil-num-tlx            pic  x(20)                  .
               10  fil-nom-int            pic  x(30)                  .
               10  fil-prt-iva            pic  9(11)       comp-3     .
               10  fil-cod-fis            pic  x(16)                  .
               10  fil-cod-cge            pic  9(07)       comp-3     .
               10  fil-cat-pvg            pic  9(05)       comp-3     .
               10  fil-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-cla-bdg            pic  9(05)       comp-3     .
               10  fil-tip-mat            pic  x(01)                  .
               10  fil-flg-cpv.
                   15  fil-flg-ele occurs 09
                                          pic  x(01)                  .
               10  fil-ctp-ven            pic  9(07)       comp-3     .
               10  fil-ctp-rsv            pic  9(07)       comp-3     .
               10  fil-sta-tus            pic  9(02)                  .
               10  fil-sta-tud            pic  9(07)                  .
               10  fil-sta-tuc            pic  9(07)                  .
               10  fil-sta-tux            pic  9(02)                  .
               10  fil-dat-iat            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler  occurs 55  pic  x(01)                  .

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
      *            * Chiave numero 01 : CODAGE                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-age        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-cod-age-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-rag-key        pic  x(40)                  .
                   15  pul-cod-age-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-mne        pic  x(10)                  .
                   15  pul-cod-age-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : NOMKEY                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-nom-key        pic  x(20)                  .
                   15  pul-cod-age-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : SUPAGE                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-sup-age        pic  9(07)       comp-3     .
                   15  pul-cod-age-6      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-nom-age            pic  x(20)                  .
               10  pul-rag-soc            pic  x(40)                  .
               10  pul-via-age            pic  x(40)                  .
               10  pul-loc-age            pic  x(40)                  .
               10  pul-cod-naz            pic  x(03)                  .
               10  pul-cod-cmn            pic  9(05)       comp-3     .
               10  pul-cod-fzn            pic  9(03)       comp-3     .
               10  pul-cod-lct            pic  9(03)       comp-3     .
               10  pul-num-tel            pic  x(20)                  .
               10  pul-num-fax            pic  x(20)                  .
               10  pul-num-tlx            pic  x(20)                  .
               10  pul-nom-int            pic  x(30)                  .
               10  pul-prt-iva            pic  9(11)       comp-3     .
               10  pul-cod-fis            pic  x(16)                  .
               10  pul-cod-cge            pic  9(07)       comp-3     .
               10  pul-cat-pvg            pic  9(05)       comp-3     .
               10  pul-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-cla-bdg            pic  9(05)       comp-3     .
               10  pul-tip-mat            pic  x(01)                  .
               10  pul-flg-cpv.
                   15  pul-flg-ele occurs 09
                                          pic  x(01)                  .
               10  pul-ctp-ven            pic  9(07)       comp-3     .
               10  pul-ctp-rsv            pic  9(07)       comp-3     .
               10  pul-sta-tus            pic  9(02)                  .
               10  pul-sta-tud            pic  9(07)                  .
               10  pul-sta-tuc            pic  9(07)                  .
               10  pul-sta-tux            pic  9(02)                  .
               10  pul-dat-iat            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler  occurs 55  pic  x(01)                  .

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
                     "age "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/age/fls/ioc/obj/iofage              "       .

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
                            "CODAGE"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RAGKEY"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODMNE"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NOMKEY"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "SUPAGE"                                  .
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
      *    * Record logico file [age]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      ******************************************************************
       Procedure Division                using f rf-age               .
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
           move      spaces               to   rf-age                 .
           move      zero                 to   rf-age-ide-dat         .
           move      spaces               to   rf-age-ide-ute         .
           move      spaces               to   rf-age-ide-fas         .
           move      zero                 to   rf-age-cod-age         .
           move      spaces               to   rf-age-cod-mne         .
           move      spaces               to   rf-age-nom-age         .
           move      spaces               to   rf-age-nom-key         .
           move      spaces               to   rf-age-rag-key         .
           move      spaces               to   rf-age-rag-soc         .
           move      spaces               to   rf-age-via-age         .
           move      spaces               to   rf-age-loc-age         .
           move      spaces               to   rf-age-cod-naz         .
           move      zero                 to   rf-age-cod-cmn         .
           move      zero                 to   rf-age-cod-fzn         .
           move      zero                 to   rf-age-cod-lct         .
           move      spaces               to   rf-age-num-tel         .
           move      spaces               to   rf-age-num-fax         .
           move      spaces               to   rf-age-num-tlx         .
           move      spaces               to   rf-age-nom-int         .
           move      zero                 to   rf-age-prt-iva         .
           move      spaces               to   rf-age-cod-fis         .
           move      zero                 to   rf-age-cod-cge         .
           move      zero                 to   rf-age-sup-age         .
           move      zero                 to   rf-age-cat-pvg         .
           move      zero                 to   rf-age-per-pvg (1)     .
           move      zero                 to   rf-age-per-pvg (2)     .
           move      zero                 to   rf-age-per-pvg (3)     .
           move      zero                 to   rf-age-cla-bdg         .
           move      spaces               to   rf-age-tip-mat         .
           move      spaces               to   rf-age-flg-cpv         .
           move      zero                 to   rf-age-ctp-ven         .
           move      zero                 to   rf-age-ctp-rsv         .
           move      zero                 to   rf-age-sta-tus         .
           move      zero                 to   rf-age-sta-tud         .
           move      zero                 to   rf-age-sta-tuc         .
           move      zero                 to   rf-age-sta-tux         .
           move      zero                 to   rf-age-dat-iat         .
           move      spaces               to   rf-age-alx-exp         .
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
           move      rf-age-ide-ute       to   fil-ide-ute            .
           move      rf-age-ide-fas       to   fil-ide-fas            .
           move      rf-age-nom-age       to   fil-nom-age            .
           move      rf-age-rag-soc       to   fil-rag-soc            .
           move      rf-age-via-age       to   fil-via-age            .
           move      rf-age-loc-age       to   fil-loc-age            .
           move      rf-age-cod-naz       to   fil-cod-naz            .
           move      rf-age-cod-cmn       to   fil-cod-cmn            .
           move      rf-age-cod-fzn       to   fil-cod-fzn            .
           move      rf-age-cod-lct       to   fil-cod-lct            .
           move      rf-age-num-tel       to   fil-num-tel            .
           move      rf-age-num-fax       to   fil-num-fax            .
           move      rf-age-num-tlx       to   fil-num-tlx            .
           move      rf-age-nom-int       to   fil-nom-int            .
           move      rf-age-prt-iva       to   fil-prt-iva            .
           move      rf-age-cod-fis       to   fil-cod-fis            .
           move      rf-age-cod-cge       to   fil-cod-cge            .
           move      rf-age-cat-pvg       to   fil-cat-pvg            .
           move      rf-age-per-pvg (1)   to   fil-per-pvg (1)        .
           move      rf-age-per-pvg (2)   to   fil-per-pvg (2)        .
           move      rf-age-per-pvg (3)   to   fil-per-pvg (3)        .
           move      rf-age-cla-bdg       to   fil-cla-bdg            .
           move      rf-age-tip-mat       to   fil-tip-mat            .
           move      rf-age-flg-cpv       to   fil-flg-cpv            .
           move      rf-age-ctp-ven       to   fil-ctp-ven            .
           move      rf-age-ctp-rsv       to   fil-ctp-rsv            .
           move      rf-age-sta-tus       to   fil-sta-tus            .
           move      rf-age-sta-tud       to   fil-sta-tud            .
           move      rf-age-sta-tuc       to   fil-sta-tuc            .
           move      rf-age-sta-tux       to   fil-sta-tux            .
           move      rf-age-dat-iat       to   fil-dat-iat            .
           move      rf-age-alx-exp       to   fil-alx-exp            .
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
           move      rf-age-cod-age       to   fil-cod-age            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-age-ide-dat       to   fil-ide-dat            .
           move      rf-age-cod-age       to   fil-cod-age-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-age-rag-key       to   fil-rag-key            .
           move      rf-age-cod-age       to   fil-cod-age-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-age-cod-mne       to   fil-cod-mne            .
           move      rf-age-cod-age       to   fil-cod-age-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-age-nom-key       to   fil-nom-key            .
           move      rf-age-cod-age       to   fil-cod-age-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-age-sup-age       to   fil-sup-age            .
           move      rf-age-cod-age       to   fil-cod-age-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-age                 .
           move      fil-ide-dat          to   rf-age-ide-dat         .
           move      fil-ide-ute          to   rf-age-ide-ute         .
           move      fil-ide-fas          to   rf-age-ide-fas         .
           move      fil-cod-age          to   rf-age-cod-age         .
           move      fil-cod-mne          to   rf-age-cod-mne         .
           move      fil-nom-age          to   rf-age-nom-age         .
           move      fil-nom-key          to   rf-age-nom-key         .
           move      fil-rag-key          to   rf-age-rag-key         .
           move      fil-rag-soc          to   rf-age-rag-soc         .
           move      fil-via-age          to   rf-age-via-age         .
           move      fil-loc-age          to   rf-age-loc-age         .
           move      fil-cod-naz          to   rf-age-cod-naz         .
           move      fil-cod-cmn          to   rf-age-cod-cmn         .
           move      fil-cod-fzn          to   rf-age-cod-fzn         .
           move      fil-cod-lct          to   rf-age-cod-lct         .
           move      fil-num-tel          to   rf-age-num-tel         .
           move      fil-num-fax          to   rf-age-num-fax         .
           move      fil-num-tlx          to   rf-age-num-tlx         .
           move      fil-nom-int          to   rf-age-nom-int         .
           move      fil-prt-iva          to   rf-age-prt-iva         .
           move      fil-cod-fis          to   rf-age-cod-fis         .
           move      fil-cod-cge          to   rf-age-cod-cge         .
           move      fil-sup-age          to   rf-age-sup-age         .
           move      fil-cat-pvg          to   rf-age-cat-pvg         .
           move      fil-per-pvg (1)      to   rf-age-per-pvg (1)     .
           move      fil-per-pvg (2)      to   rf-age-per-pvg (2)     .
           move      fil-per-pvg (3)      to   rf-age-per-pvg (3)     .
           move      fil-cla-bdg          to   rf-age-cla-bdg         .
           move      fil-tip-mat          to   rf-age-tip-mat         .
           move      fil-flg-cpv          to   rf-age-flg-cpv         .
           move      fil-ctp-ven          to   rf-age-ctp-ven         .
           move      fil-ctp-rsv          to   rf-age-ctp-rsv         .
           move      fil-sta-tus          to   rf-age-sta-tus         .
           move      fil-sta-tud          to   rf-age-sta-tud         .
           move      fil-sta-tuc          to   rf-age-sta-tuc         .
           move      fil-sta-tux          to   rf-age-sta-tux         .
           move      fil-dat-iat          to   rf-age-dat-iat         .
           move      fil-alx-exp          to   rf-age-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-age               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-age
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

