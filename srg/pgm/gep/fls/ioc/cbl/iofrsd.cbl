       Identification Division.
       Program-Id.                                 iofrsd             .
      *================================================================*
      *                                                                *
      *                  Input-Output File rsd                         *
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
      *            * Chiave numero 01 : NUMRSD                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-rsd        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-rsd-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DBTRSD                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-dbt        pic  9(02)                  .
                   15  fil-cod-dbt        pic  9(07)       comp-3     .
                   15  fil-num-rsd-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : UORRSD                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-num-uor        pic  9(11)       comp-3     .
                   15  fil-num-rsd-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CBPUOR                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cbp-rsd        pic  x(10)                  .
                   15  fil-num-uor-5      pic  9(11)       comp-3     .
                   15  fil-num-rsd-5      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dtr-rsd            pic  9(07)       comp-3     .
               10  fil-drc-rsd            pic  9(07)       comp-3     .
               10  fil-npc-rsd            pic  9(07)       comp-3     .
               10  fil-mod-rsd            pic  9(02)                  .
               10  fil-tip-rsd            pic  9(02)                  .
               10  fil-tvl-rsd            pic  9(02)                  .
               10  fil-dpz-dbt            pic  x(04)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-tdc-vlt            pic  x(01)                  .
               10  fil-cdc-rsd            pic  9(06)v9(05) comp-3     .
               10  fil-iiv-rsd            pic s9(11)       comp-3     .
               10  fil-imp-rsd            pic s9(11)       comp-3     .
               10  fil-spe-rsd            pic s9(11)       comp-3     .
               10  fil-ddo-rsd            pic  9(07)       comp-3     .
               10  fil-ndo-rsd            pic  x(10)                  .
               10  fil-nsc-rsd            pic  9(11)       comp-3     .
               10  fil-fcs-uno            pic  9(02)                  .
               10  fil-fcs-due            pic  9(02)                  .
               10  fil-fcs-tre            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-cbp-ass            pic  x(10)                  .
               10  fil-abi-ass            pic  9(05)                  .
               10  fil-cab-ass            pic  9(05)                  .
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
      *            * Chiave numero 01 : NUMRSD                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-rsd        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-rsd-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DBTRSD                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-dbt        pic  9(02)                  .
                   15  pul-cod-dbt        pic  9(07)       comp-3     .
                   15  pul-num-rsd-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : UORRSD                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-num-uor        pic  9(11)       comp-3     .
                   15  pul-num-rsd-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CBPUOR                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cbp-rsd        pic  x(10)                  .
                   15  pul-num-uor-5      pic  9(11)       comp-3     .
                   15  pul-num-rsd-5      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dtr-rsd            pic  9(07)       comp-3     .
               10  pul-drc-rsd            pic  9(07)       comp-3     .
               10  pul-npc-rsd            pic  9(07)       comp-3     .
               10  pul-mod-rsd            pic  9(02)                  .
               10  pul-tip-rsd            pic  9(02)                  .
               10  pul-tvl-rsd            pic  9(02)                  .
               10  pul-dpz-dbt            pic  x(04)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-tdc-vlt            pic  x(01)                  .
               10  pul-cdc-rsd            pic  9(06)v9(05) comp-3     .
               10  pul-iiv-rsd            pic s9(11)       comp-3     .
               10  pul-imp-rsd            pic s9(11)       comp-3     .
               10  pul-spe-rsd            pic s9(11)       comp-3     .
               10  pul-ddo-rsd            pic  9(07)       comp-3     .
               10  pul-ndo-rsd            pic  x(10)                  .
               10  pul-nsc-rsd            pic  9(11)       comp-3     .
               10  pul-fcs-uno            pic  9(02)                  .
               10  pul-fcs-due            pic  9(02)                  .
               10  pul-fcs-tre            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-cbp-ass            pic  x(10)                  .
               10  pul-abi-ass            pic  9(05)                  .
               10  pul-cab-ass            pic  9(05)                  .
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
                     "rsd "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/gep/fls/ioc/obj/iofrsd              "       .

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
           05  k-ctr                      pic  9(02) value 5          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMRSD    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DBTRSD    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "UORRSD    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CBPUOR    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    5      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [rsd]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsd"                          .

      ******************************************************************
       Procedure Division                using f rf-rsd               .
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
           move      spaces               to   rf-rsd                 .
           move      zero                 to   rf-rsd-ide-dat         .
           move      spaces               to   rf-rsd-ide-ute         .
           move      spaces               to   rf-rsd-ide-fas         .
           move      zero                 to   rf-rsd-dtr-rsd         .
           move      zero                 to   rf-rsd-drc-rsd         .
           move      zero                 to   rf-rsd-npc-rsd         .
           move      zero                 to   rf-rsd-num-rsd         .
           move      zero                 to   rf-rsd-mod-rsd         .
           move      zero                 to   rf-rsd-tip-rsd         .
           move      zero                 to   rf-rsd-tvl-rsd         .
           move      zero                 to   rf-rsd-tip-dbt         .
           move      zero                 to   rf-rsd-cod-dbt         .
           move      spaces               to   rf-rsd-dpz-dbt         .
           move      spaces               to   rf-rsd-cbp-rsd         .
           move      spaces               to   rf-rsd-sgl-vlt         .
           move      zero                 to   rf-rsd-dec-vlt         .
           move      spaces               to   rf-rsd-tdc-vlt         .
           move      zero                 to   rf-rsd-cdc-rsd         .
           move      zero                 to   rf-rsd-iiv-rsd         .
           move      zero                 to   rf-rsd-imp-rsd         .
           move      zero                 to   rf-rsd-spe-rsd         .
           move      zero                 to   rf-rsd-ddo-rsd         .
           move      spaces               to   rf-rsd-ndo-rsd         .
           move      zero                 to   rf-rsd-nsc-rsd         .
           move      zero                 to   rf-rsd-num-uor         .
           move      zero                 to   rf-rsd-fcs-uno         .
           move      zero                 to   rf-rsd-fcs-due         .
           move      zero                 to   rf-rsd-fcs-tre         .
           move      spaces               to   rf-rsd-flg-ela         .
           move      spaces               to   rf-rsd-flg-pul         .
           move      spaces               to   rf-rsd-cbp-ass         .
           move      zero                 to   rf-rsd-abi-ass         .
           move      zero                 to   rf-rsd-cab-ass         .
           move      spaces               to   rf-rsd-alx-exp         .
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
           move      rf-rsd-ide-ute       to   fil-ide-ute            .
           move      rf-rsd-ide-fas       to   fil-ide-fas            .
           move      rf-rsd-dtr-rsd       to   fil-dtr-rsd            .
           move      rf-rsd-drc-rsd       to   fil-drc-rsd            .
           move      rf-rsd-npc-rsd       to   fil-npc-rsd            .
           move      rf-rsd-mod-rsd       to   fil-mod-rsd            .
           move      rf-rsd-tip-rsd       to   fil-tip-rsd            .
           move      rf-rsd-tvl-rsd       to   fil-tvl-rsd            .
           move      rf-rsd-dpz-dbt       to   fil-dpz-dbt            .
           move      rf-rsd-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-rsd-dec-vlt       to   fil-dec-vlt            .
           move      rf-rsd-tdc-vlt       to   fil-tdc-vlt            .
           move      rf-rsd-cdc-rsd       to   fil-cdc-rsd            .
           move      rf-rsd-iiv-rsd       to   fil-iiv-rsd            .
           move      rf-rsd-imp-rsd       to   fil-imp-rsd            .
           move      rf-rsd-spe-rsd       to   fil-spe-rsd            .
           move      rf-rsd-ddo-rsd       to   fil-ddo-rsd            .
           move      rf-rsd-ndo-rsd       to   fil-ndo-rsd            .
           move      rf-rsd-nsc-rsd       to   fil-nsc-rsd            .
           move      rf-rsd-fcs-uno       to   fil-fcs-uno            .
           move      rf-rsd-fcs-due       to   fil-fcs-due            .
           move      rf-rsd-fcs-tre       to   fil-fcs-tre            .
           move      rf-rsd-flg-ela       to   fil-flg-ela            .
           move      rf-rsd-flg-pul       to   fil-flg-pul            .
           move      rf-rsd-cbp-ass       to   fil-cbp-ass            .
           move      rf-rsd-abi-ass       to   fil-abi-ass            .
           move      rf-rsd-cab-ass       to   fil-cab-ass            .
           move      rf-rsd-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-rsd-num-rsd       to   fil-num-rsd            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-rsd-ide-dat       to   fil-ide-dat            .
           move      rf-rsd-num-rsd       to   fil-num-rsd-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-rsd-tip-dbt       to   fil-tip-dbt            .
           move      rf-rsd-cod-dbt       to   fil-cod-dbt            .
           move      rf-rsd-num-rsd       to   fil-num-rsd-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-rsd-num-uor       to   fil-num-uor            .
           move      rf-rsd-num-rsd       to   fil-num-rsd-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-rsd-cbp-rsd       to   fil-cbp-rsd            .
           move      rf-rsd-num-uor       to   fil-num-uor-5          .
           move      rf-rsd-num-rsd       to   fil-num-rsd-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-rsd                 .
           move      fil-ide-dat          to   rf-rsd-ide-dat         .
           move      fil-ide-ute          to   rf-rsd-ide-ute         .
           move      fil-ide-fas          to   rf-rsd-ide-fas         .
           move      fil-dtr-rsd          to   rf-rsd-dtr-rsd         .
           move      fil-drc-rsd          to   rf-rsd-drc-rsd         .
           move      fil-npc-rsd          to   rf-rsd-npc-rsd         .
           move      fil-num-rsd          to   rf-rsd-num-rsd         .
           move      fil-mod-rsd          to   rf-rsd-mod-rsd         .
           move      fil-tip-rsd          to   rf-rsd-tip-rsd         .
           move      fil-tvl-rsd          to   rf-rsd-tvl-rsd         .
           move      fil-tip-dbt          to   rf-rsd-tip-dbt         .
           move      fil-cod-dbt          to   rf-rsd-cod-dbt         .
           move      fil-dpz-dbt          to   rf-rsd-dpz-dbt         .
           move      fil-cbp-rsd          to   rf-rsd-cbp-rsd         .
           move      fil-sgl-vlt          to   rf-rsd-sgl-vlt         .
           move      fil-dec-vlt          to   rf-rsd-dec-vlt         .
           move      fil-tdc-vlt          to   rf-rsd-tdc-vlt         .
           move      fil-cdc-rsd          to   rf-rsd-cdc-rsd         .
           move      fil-iiv-rsd          to   rf-rsd-iiv-rsd         .
           move      fil-imp-rsd          to   rf-rsd-imp-rsd         .
           move      fil-spe-rsd          to   rf-rsd-spe-rsd         .
           move      fil-ddo-rsd          to   rf-rsd-ddo-rsd         .
           move      fil-ndo-rsd          to   rf-rsd-ndo-rsd         .
           move      fil-nsc-rsd          to   rf-rsd-nsc-rsd         .
           move      fil-num-uor          to   rf-rsd-num-uor         .
           move      fil-fcs-uno          to   rf-rsd-fcs-uno         .
           move      fil-fcs-due          to   rf-rsd-fcs-due         .
           move      fil-fcs-tre          to   rf-rsd-fcs-tre         .
           move      fil-flg-ela          to   rf-rsd-flg-ela         .
           move      fil-flg-pul          to   rf-rsd-flg-pul         .
           move      fil-cbp-ass          to   rf-rsd-cbp-ass         .
           move      fil-abi-ass          to   rf-rsd-abi-ass         .
           move      fil-cab-ass          to   rf-rsd-cab-ass         .
           move      fil-alx-exp          to   rf-rsd-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-rsd               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-rsd
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
