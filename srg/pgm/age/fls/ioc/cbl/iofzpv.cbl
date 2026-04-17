       Identification Division.
       Program-Id.                                 iofzpv             .
      *================================================================*
      *                                                                *
      *                  Input-Output File zpv                         *
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
      *            * Chiave numero 01 : CODCPV                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-cpv        pic  9(02)                  .
                   15  fil-cod-cpv        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-tip-cpv-2      pic  9(02)                  .
                   15  fil-des-key        pic  x(20)                  .
                   15  fil-cod-cpv-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CODMNE                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-cpv-3      pic  9(02)                  .
                   15  fil-mne-cpv        pic  x(05)                  .
                   15  fil-cod-cpv-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-cpv            pic  x(20)                  .
               10  fil-spd-scl occurs 06.
                   15  fil-spd-sco occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  fil-spd-scu        pic  9(02)v9(01) comp-3     .
                   15  fil-spd-pvg occurs 03
                                          pic  9(02)v9(01)            .
               10  fil-trt-pvi            pic  9(02)                  .
               10  fil-spi-scl occurs 06.
                   15  fil-spi-sco occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  fil-spi-scu        pic  9(02)v9(01) comp-3     .
                   15  fil-spi-pvg occurs 03
                                          pic  9(02)v9(01)            .
               10  fil-alx-exp            pic  x(40)                  .

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
      *            * Chiave numero 01 : CODCPV                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-cpv        pic  9(02)                  .
                   15  pul-cod-cpv        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-tip-cpv-2      pic  9(02)                  .
                   15  pul-des-key        pic  x(20)                  .
                   15  pul-cod-cpv-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CODMNE                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-cpv-3      pic  9(02)                  .
                   15  pul-mne-cpv        pic  x(05)                  .
                   15  pul-cod-cpv-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-cpv            pic  x(20)                  .
               10  pul-spd-scl occurs 06.
                   15  pul-spd-sco occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  pul-spd-scu        pic  9(02)v9(01) comp-3     .
                   15  pul-spd-pvg occurs 03
                                          pic  9(02)v9(01)            .
               10  pul-trt-pvi            pic  9(02)                  .
               10  pul-spi-scl occurs 06.
                   15  pul-spi-sco occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  pul-spi-scu        pic  9(02)v9(01) comp-3     .
                   15  pul-spi-pvg occurs 03
                                          pic  9(02)v9(01)            .
               10  pul-alx-exp            pic  x(40)                  .

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
                     "zpv "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/age/fls/ioc/obj/iofzpv              "       .

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
           05  k-ctr                      pic  9(02) value 3          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODCPV"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DESKEY"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODMNE"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    3      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [zpv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpv"                          .

      ******************************************************************
       Procedure Division                using f rf-zpv               .
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
           move      spaces               to   rf-zpv                 .
           move      zero                 to   rf-zpv-tip-cpv         .
           move      zero                 to   rf-zpv-cod-cpv         .
           move      spaces               to   rf-zpv-des-cpv         .
           move      spaces               to   rf-zpv-mne-cpv         .
           move      spaces               to   rf-zpv-des-key         .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-sco (1, 1)  .
           move      zero                 to   rf-zpv-spd-sco (1, 2)  .
           move      zero                 to   rf-zpv-spd-sco (1, 3)  .
           move      zero                 to   rf-zpv-spd-sco (1, 4)  .
           move      zero                 to   rf-zpv-spd-sco (1, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-sco (2, 1)  .
           move      zero                 to   rf-zpv-spd-sco (2, 2)  .
           move      zero                 to   rf-zpv-spd-sco (2, 3)  .
           move      zero                 to   rf-zpv-spd-sco (2, 4)  .
           move      zero                 to   rf-zpv-spd-sco (2, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-sco (3, 1)  .
           move      zero                 to   rf-zpv-spd-sco (3, 2)  .
           move      zero                 to   rf-zpv-spd-sco (3, 3)  .
           move      zero                 to   rf-zpv-spd-sco (3, 4)  .
           move      zero                 to   rf-zpv-spd-sco (3, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-sco (4, 1)  .
           move      zero                 to   rf-zpv-spd-sco (4, 2)  .
           move      zero                 to   rf-zpv-spd-sco (4, 3)  .
           move      zero                 to   rf-zpv-spd-sco (4, 4)  .
           move      zero                 to   rf-zpv-spd-sco (4, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-sco (5, 1)  .
           move      zero                 to   rf-zpv-spd-sco (5, 2)  .
           move      zero                 to   rf-zpv-spd-sco (5, 3)  .
           move      zero                 to   rf-zpv-spd-sco (5, 4)  .
           move      zero                 to   rf-zpv-spd-sco (5, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-sco (6, 1)  .
           move      zero                 to   rf-zpv-spd-sco (6, 2)  .
           move      zero                 to   rf-zpv-spd-sco (6, 3)  .
           move      zero                 to   rf-zpv-spd-sco (6, 4)  .
           move      zero                 to   rf-zpv-spd-sco (6, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-scu (1)     .
           move      zero                 to   rf-zpv-spd-scu (2)     .
           move      zero                 to   rf-zpv-spd-scu (3)     .
           move      zero                 to   rf-zpv-spd-scu (4)     .
           move      zero                 to   rf-zpv-spd-scu (5)     .
           move      zero                 to   rf-zpv-spd-scu (6)     .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-pvg (1, 1)  .
           move      zero                 to   rf-zpv-spd-pvg (1, 2)  .
           move      zero                 to   rf-zpv-spd-pvg (1, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-pvg (2, 1)  .
           move      zero                 to   rf-zpv-spd-pvg (2, 2)  .
           move      zero                 to   rf-zpv-spd-pvg (2, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-pvg (3, 1)  .
           move      zero                 to   rf-zpv-spd-pvg (3, 2)  .
           move      zero                 to   rf-zpv-spd-pvg (3, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-pvg (4, 1)  .
           move      zero                 to   rf-zpv-spd-pvg (4, 2)  .
           move      zero                 to   rf-zpv-spd-pvg (4, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-pvg (5, 1)  .
           move      zero                 to   rf-zpv-spd-pvg (5, 2)  .
           move      zero                 to   rf-zpv-spd-pvg (5, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spd-pvg (6, 1)  .
           move      zero                 to   rf-zpv-spd-pvg (6, 2)  .
           move      zero                 to   rf-zpv-spd-pvg (6, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-trt-pvi         .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-sco (1, 1)  .
           move      zero                 to   rf-zpv-spi-sco (1, 2)  .
           move      zero                 to   rf-zpv-spi-sco (1, 3)  .
           move      zero                 to   rf-zpv-spi-sco (1, 4)  .
           move      zero                 to   rf-zpv-spi-sco (1, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-sco (2, 1)  .
           move      zero                 to   rf-zpv-spi-sco (2, 2)  .
           move      zero                 to   rf-zpv-spi-sco (2, 3)  .
           move      zero                 to   rf-zpv-spi-sco (2, 4)  .
           move      zero                 to   rf-zpv-spi-sco (2, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-sco (3, 1)  .
           move      zero                 to   rf-zpv-spi-sco (3, 2)  .
           move      zero                 to   rf-zpv-spi-sco (3, 3)  .
           move      zero                 to   rf-zpv-spi-sco (3, 4)  .
           move      zero                 to   rf-zpv-spi-sco (3, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-sco (4, 1)  .
           move      zero                 to   rf-zpv-spi-sco (4, 2)  .
           move      zero                 to   rf-zpv-spi-sco (4, 3)  .
           move      zero                 to   rf-zpv-spi-sco (4, 4)  .
           move      zero                 to   rf-zpv-spi-sco (4, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-sco (5, 1)  .
           move      zero                 to   rf-zpv-spi-sco (5, 2)  .
           move      zero                 to   rf-zpv-spi-sco (5, 3)  .
           move      zero                 to   rf-zpv-spi-sco (5, 4)  .
           move      zero                 to   rf-zpv-spi-sco (5, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-sco (6, 1)  .
           move      zero                 to   rf-zpv-spi-sco (6, 2)  .
           move      zero                 to   rf-zpv-spi-sco (6, 3)  .
           move      zero                 to   rf-zpv-spi-sco (6, 4)  .
           move      zero                 to   rf-zpv-spi-sco (6, 5)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-scu (1)     .
           move      zero                 to   rf-zpv-spi-scu (2)     .
           move      zero                 to   rf-zpv-spi-scu (3)     .
           move      zero                 to   rf-zpv-spi-scu (4)     .
           move      zero                 to   rf-zpv-spi-scu (5)     .
           move      zero                 to   rf-zpv-spi-scu (6)     .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-pvg (1, 1)  .
           move      zero                 to   rf-zpv-spi-pvg (1, 2)  .
           move      zero                 to   rf-zpv-spi-pvg (1, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-pvg (2, 1)  .
           move      zero                 to   rf-zpv-spi-pvg (2, 2)  .
           move      zero                 to   rf-zpv-spi-pvg (2, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-pvg (3, 1)  .
           move      zero                 to   rf-zpv-spi-pvg (3, 2)  .
           move      zero                 to   rf-zpv-spi-pvg (3, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-pvg (4, 1)  .
           move      zero                 to   rf-zpv-spi-pvg (4, 2)  .
           move      zero                 to   rf-zpv-spi-pvg (4, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-pvg (5, 1)  .
           move      zero                 to   rf-zpv-spi-pvg (5, 2)  .
           move      zero                 to   rf-zpv-spi-pvg (5, 3)  .
      *    *                                                           *
           move      zero                 to   rf-zpv-spi-pvg (6, 1)  .
           move      zero                 to   rf-zpv-spi-pvg (6, 2)  .
           move      zero                 to   rf-zpv-spi-pvg (6, 3)  .
      *    *                                                           *
           move      spaces               to   rf-zpv-alx-exp         .
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
           move      rf-zpv-des-cpv       to   fil-des-cpv            .
      *    *                                                           *
           move      rf-zpv-spd-sco (1, 1)
                                          to   fil-spd-sco (1, 1)     .
           move      rf-zpv-spd-sco (1, 2)
                                          to   fil-spd-sco (1, 2)     .
           move      rf-zpv-spd-sco (1, 3)
                                          to   fil-spd-sco (1, 3)     .
           move      rf-zpv-spd-sco (1, 4)
                                          to   fil-spd-sco (1, 4)     .
           move      rf-zpv-spd-sco (1, 5)
                                          to   fil-spd-sco (1, 5)     .
      *    *                                                           *
           move      rf-zpv-spd-sco (2, 1)
                                          to   fil-spd-sco (2, 1)     .
           move      rf-zpv-spd-sco (2, 2)
                                          to   fil-spd-sco (2, 2)     .
           move      rf-zpv-spd-sco (2, 3)
                                          to   fil-spd-sco (2, 3)     .
           move      rf-zpv-spd-sco (2, 4)
                                          to   fil-spd-sco (2, 4)     .
           move      rf-zpv-spd-sco (2, 5)
                                          to   fil-spd-sco (2, 5)     .
      *    *                                                           *
           move      rf-zpv-spd-sco (3, 1)
                                          to   fil-spd-sco (3, 1)     .
           move      rf-zpv-spd-sco (3, 2)
                                          to   fil-spd-sco (3, 2)     .
           move      rf-zpv-spd-sco (3, 3)
                                          to   fil-spd-sco (3, 3)     .
           move      rf-zpv-spd-sco (3, 4)
                                          to   fil-spd-sco (3, 4)     .
           move      rf-zpv-spd-sco (3, 5)
                                          to   fil-spd-sco (3, 5)     .
      *    *                                                           *
           move      rf-zpv-spd-sco (4, 1)
                                          to   fil-spd-sco (4, 1)     .
           move      rf-zpv-spd-sco (4, 2)
                                          to   fil-spd-sco (4, 2)     .
           move      rf-zpv-spd-sco (4, 3)
                                          to   fil-spd-sco (4, 3)     .
           move      rf-zpv-spd-sco (4, 4)
                                          to   fil-spd-sco (4, 4)     .
           move      rf-zpv-spd-sco (4, 5)
                                          to   fil-spd-sco (4, 5)     .
      *    *                                                           *
           move      rf-zpv-spd-sco (5, 1)
                                          to   fil-spd-sco (5, 1)     .
           move      rf-zpv-spd-sco (5, 2)
                                          to   fil-spd-sco (5, 2)     .
           move      rf-zpv-spd-sco (5, 3)
                                          to   fil-spd-sco (5, 3)     .
           move      rf-zpv-spd-sco (5, 4)
                                          to   fil-spd-sco (5, 4)     .
           move      rf-zpv-spd-sco (5, 5)
                                          to   fil-spd-sco (5, 5)     .
      *    *                                                           *
           move      rf-zpv-spd-sco (6, 1)
                                          to   fil-spd-sco (6, 1)     .
           move      rf-zpv-spd-sco (6, 2)
                                          to   fil-spd-sco (6, 2)     .
           move      rf-zpv-spd-sco (6, 3)
                                          to   fil-spd-sco (6, 3)     .
           move      rf-zpv-spd-sco (6, 4)
                                          to   fil-spd-sco (6, 4)     .
           move      rf-zpv-spd-sco (6, 5)
                                          to   fil-spd-sco (6, 5)     .
      *    *                                                           *
           move      rf-zpv-spd-scu (1)   to   fil-spd-scu (1)        .
           move      rf-zpv-spd-scu (2)   to   fil-spd-scu (2)        .
           move      rf-zpv-spd-scu (3)   to   fil-spd-scu (3)        .
           move      rf-zpv-spd-scu (4)   to   fil-spd-scu (4)        .
           move      rf-zpv-spd-scu (5)   to   fil-spd-scu (5)        .
           move      rf-zpv-spd-scu (6)   to   fil-spd-scu (6)        .
      *    *                                                           *
           move      rf-zpv-spd-pvg (1, 1)
                                          to   fil-spd-pvg (1, 1)     .
           move      rf-zpv-spd-pvg (1, 2)
                                          to   fil-spd-pvg (1, 2)     .
           move      rf-zpv-spd-pvg (1, 3)
                                          to   fil-spd-pvg (1, 3)     .
      *    *                                                           *
           move      rf-zpv-spd-pvg (2, 1)
                                          to   fil-spd-pvg (2, 1)     .
           move      rf-zpv-spd-pvg (2, 2)
                                          to   fil-spd-pvg (2, 2)     .
           move      rf-zpv-spd-pvg (2, 3)
                                          to   fil-spd-pvg (2, 3)     .
      *    *                                                           *
           move      rf-zpv-spd-pvg (3, 1)
                                          to   fil-spd-pvg (3, 1)     .
           move      rf-zpv-spd-pvg (3, 2)
                                          to   fil-spd-pvg (3, 2)     .
           move      rf-zpv-spd-pvg (3, 3)
                                          to   fil-spd-pvg (3, 3)     .
      *    *                                                           *
           move      rf-zpv-spd-pvg (4, 1)
                                          to   fil-spd-pvg (4, 1)     .
           move      rf-zpv-spd-pvg (4, 2)
                                          to   fil-spd-pvg (4, 2)     .
           move      rf-zpv-spd-pvg (4, 3)
                                          to   fil-spd-pvg (4, 3)     .
      *    *                                                           *
           move      rf-zpv-spd-pvg (5, 1)
                                          to   fil-spd-pvg (5, 1)     .
           move      rf-zpv-spd-pvg (5, 2)
                                          to   fil-spd-pvg (5, 2)     .
           move      rf-zpv-spd-pvg (5, 3)
                                          to   fil-spd-pvg (5, 3)     .
      *    *                                                           *
           move      rf-zpv-spd-pvg (6, 1)
                                          to   fil-spd-pvg (6, 1)     .
           move      rf-zpv-spd-pvg (6, 2)
                                          to   fil-spd-pvg (6, 2)     .
           move      rf-zpv-spd-pvg (6, 3)
                                          to   fil-spd-pvg (6, 3)     .
      *    *                                                           *
           move      rf-zpv-trt-pvi       to   fil-trt-pvi            .
      *    *                                                           *
           move      rf-zpv-spi-sco (1, 1)
                                          to   fil-spi-sco (1, 1)     .
           move      rf-zpv-spi-sco (1, 2)
                                          to   fil-spi-sco (1, 2)     .
           move      rf-zpv-spi-sco (1, 3)
                                          to   fil-spi-sco (1, 3)     .
           move      rf-zpv-spi-sco (1, 4)
                                          to   fil-spi-sco (1, 4)     .
           move      rf-zpv-spi-sco (1, 5)
                                          to   fil-spi-sco (1, 5)     .
      *    *                                                           *
           move      rf-zpv-spi-sco (2, 1)
                                          to   fil-spi-sco (2, 1)     .
           move      rf-zpv-spi-sco (2, 2)
                                          to   fil-spi-sco (2, 2)     .
           move      rf-zpv-spi-sco (2, 3)
                                          to   fil-spi-sco (2, 3)     .
           move      rf-zpv-spi-sco (2, 4)
                                          to   fil-spi-sco (2, 4)     .
           move      rf-zpv-spi-sco (2, 5)
                                          to   fil-spi-sco (2, 5)     .
      *    *                                                           *
           move      rf-zpv-spi-sco (3, 1)
                                          to   fil-spi-sco (3, 1)     .
           move      rf-zpv-spi-sco (3, 2)
                                          to   fil-spi-sco (3, 2)     .
           move      rf-zpv-spi-sco (3, 3)
                                          to   fil-spi-sco (3, 3)     .
           move      rf-zpv-spi-sco (3, 4)
                                          to   fil-spi-sco (3, 4)     .
           move      rf-zpv-spi-sco (3, 5)
                                          to   fil-spi-sco (3, 5)     .
      *    *                                                           *
           move      rf-zpv-spi-sco (4, 1)
                                          to   fil-spi-sco (4, 1)     .
           move      rf-zpv-spi-sco (4, 2)
                                          to   fil-spi-sco (4, 2)     .
           move      rf-zpv-spi-sco (4, 3)
                                          to   fil-spi-sco (4, 3)     .
           move      rf-zpv-spi-sco (4, 4)
                                          to   fil-spi-sco (4, 4)     .
           move      rf-zpv-spi-sco (4, 5)
                                          to   fil-spi-sco (4, 5)     .
      *    *                                                           *
           move      rf-zpv-spi-sco (5, 1)
                                          to   fil-spi-sco (5, 1)     .
           move      rf-zpv-spi-sco (5, 2)
                                          to   fil-spi-sco (5, 2)     .
           move      rf-zpv-spi-sco (5, 3)
                                          to   fil-spi-sco (5, 3)     .
           move      rf-zpv-spi-sco (5, 4)
                                          to   fil-spi-sco (5, 4)     .
           move      rf-zpv-spi-sco (5, 5)
                                          to   fil-spi-sco (5, 5)     .
      *    *                                                           *
           move      rf-zpv-spi-sco (6, 1)
                                          to   fil-spi-sco (6, 1)     .
           move      rf-zpv-spi-sco (6, 2)
                                          to   fil-spi-sco (6, 2)     .
           move      rf-zpv-spi-sco (6, 3)
                                          to   fil-spi-sco (6, 3)     .
           move      rf-zpv-spi-sco (6, 4)
                                          to   fil-spi-sco (6, 4)     .
           move      rf-zpv-spi-sco (6, 5)
                                          to   fil-spi-sco (6, 5)     .
      *    *                                                           *
           move      rf-zpv-spi-scu (1)   to   fil-spi-scu (1)        .
           move      rf-zpv-spi-scu (2)   to   fil-spi-scu (2)        .
           move      rf-zpv-spi-scu (3)   to   fil-spi-scu (3)        .
           move      rf-zpv-spi-scu (4)   to   fil-spi-scu (4)        .
           move      rf-zpv-spi-scu (5)   to   fil-spi-scu (5)        .
           move      rf-zpv-spi-scu (6)   to   fil-spi-scu (6)        .
      *    *                                                           *
           move      rf-zpv-spi-pvg (1, 1)
                                          to   fil-spi-pvg (1, 1)     .
           move      rf-zpv-spi-pvg (1, 2)
                                          to   fil-spi-pvg (1, 2)     .
           move      rf-zpv-spi-pvg (1, 3)
                                          to   fil-spi-pvg (1, 3)     .
      *    *                                                           *
           move      rf-zpv-spi-pvg (2, 1)
                                          to   fil-spi-pvg (2, 1)     .
           move      rf-zpv-spi-pvg (2, 2)
                                          to   fil-spi-pvg (2, 2)     .
           move      rf-zpv-spi-pvg (2, 3)
                                          to   fil-spi-pvg (2, 3)     .
      *    *                                                           *
           move      rf-zpv-spi-pvg (3, 1)
                                          to   fil-spi-pvg (3, 1)     .
           move      rf-zpv-spi-pvg (3, 2)
                                          to   fil-spi-pvg (3, 2)     .
           move      rf-zpv-spi-pvg (3, 3)
                                          to   fil-spi-pvg (3, 3)     .
      *    *                                                           *
           move      rf-zpv-spi-pvg (4, 1)
                                          to   fil-spi-pvg (4, 1)     .
           move      rf-zpv-spi-pvg (4, 2)
                                          to   fil-spi-pvg (4, 2)     .
           move      rf-zpv-spi-pvg (4, 3)
                                          to   fil-spi-pvg (4, 3)     .
      *    *                                                           *
           move      rf-zpv-spi-pvg (5, 1)
                                          to   fil-spi-pvg (5, 1)     .
           move      rf-zpv-spi-pvg (5, 2)
                                          to   fil-spi-pvg (5, 2)     .
           move      rf-zpv-spi-pvg (5, 3)
                                          to   fil-spi-pvg (5, 3)     .
      *    *                                                           *
           move      rf-zpv-spi-pvg (6, 1)
                                          to   fil-spi-pvg (6, 1)     .
           move      rf-zpv-spi-pvg (6, 2)
                                          to   fil-spi-pvg (6, 2)     .
           move      rf-zpv-spi-pvg (6, 3)
                                          to   fil-spi-pvg (6, 3)     .
      *    *                                                           *
           move      rf-zpv-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-zpv-tip-cpv       to   fil-tip-cpv            .
           move      rf-zpv-cod-cpv       to   fil-cod-cpv            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-zpv-tip-cpv       to   fil-tip-cpv-2          .
           move      rf-zpv-des-key       to   fil-des-key            .
           move      rf-zpv-cod-cpv       to   fil-cod-cpv-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-zpv-tip-cpv       to   fil-tip-cpv-3          .
           move      rf-zpv-mne-cpv       to   fil-mne-cpv            .
           move      rf-zpv-cod-cpv       to   fil-cod-cpv-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-zpv                 .
           move      fil-tip-cpv          to   rf-zpv-tip-cpv         .
           move      fil-cod-cpv          to   rf-zpv-cod-cpv         .
           move      fil-des-cpv          to   rf-zpv-des-cpv         .
           move      fil-des-key          to   rf-zpv-des-key         .
           move      fil-mne-cpv          to   rf-zpv-mne-cpv         .
      *    *                                                           *
           move      fil-spd-sco (1, 1)   to   rf-zpv-spd-sco (1, 1)  .
           move      fil-spd-sco (1, 2)   to   rf-zpv-spd-sco (1, 2)  .
           move      fil-spd-sco (1, 3)   to   rf-zpv-spd-sco (1, 3)  .
           move      fil-spd-sco (1, 4)   to   rf-zpv-spd-sco (1, 4)  .
           move      fil-spd-sco (1, 5)   to   rf-zpv-spd-sco (1, 5)  .
      *    *                                                           *
           move      fil-spd-sco (2, 1)   to   rf-zpv-spd-sco (2, 1)  .
           move      fil-spd-sco (2, 2)   to   rf-zpv-spd-sco (2, 2)  .
           move      fil-spd-sco (2, 3)   to   rf-zpv-spd-sco (2, 3)  .
           move      fil-spd-sco (2, 4)   to   rf-zpv-spd-sco (2, 4)  .
           move      fil-spd-sco (2, 5)   to   rf-zpv-spd-sco (2, 5)  .
      *    *                                                           *
           move      fil-spd-sco (3, 1)   to   rf-zpv-spd-sco (3, 1)  .
           move      fil-spd-sco (3, 2)   to   rf-zpv-spd-sco (3, 2)  .
           move      fil-spd-sco (3, 3)   to   rf-zpv-spd-sco (3, 3)  .
           move      fil-spd-sco (3, 4)   to   rf-zpv-spd-sco (3, 4)  .
           move      fil-spd-sco (3, 5)   to   rf-zpv-spd-sco (3, 5)  .
      *    *                                                           *
           move      fil-spd-sco (4, 1)   to   rf-zpv-spd-sco (4, 1)  .
           move      fil-spd-sco (4, 2)   to   rf-zpv-spd-sco (4, 2)  .
           move      fil-spd-sco (4, 3)   to   rf-zpv-spd-sco (4, 3)  .
           move      fil-spd-sco (4, 4)   to   rf-zpv-spd-sco (4, 4)  .
           move      fil-spd-sco (4, 5)   to   rf-zpv-spd-sco (4, 5)  .
      *    *                                                           *
           move      fil-spd-sco (5, 1)   to   rf-zpv-spd-sco (5, 1)  .
           move      fil-spd-sco (5, 2)   to   rf-zpv-spd-sco (5, 2)  .
           move      fil-spd-sco (5, 3)   to   rf-zpv-spd-sco (5, 3)  .
           move      fil-spd-sco (5, 4)   to   rf-zpv-spd-sco (5, 4)  .
           move      fil-spd-sco (5, 5)   to   rf-zpv-spd-sco (5, 5)  .
      *    *                                                           *
           move      fil-spd-sco (6, 1)   to   rf-zpv-spd-sco (6, 1)  .
           move      fil-spd-sco (6, 2)   to   rf-zpv-spd-sco (6, 2)  .
           move      fil-spd-sco (6, 3)   to   rf-zpv-spd-sco (6, 3)  .
           move      fil-spd-sco (6, 4)   to   rf-zpv-spd-sco (6, 4)  .
           move      fil-spd-sco (6, 5)   to   rf-zpv-spd-sco (6, 5)  .
      *    *                                                           *
           move      fil-spd-scu (1)      to   rf-zpv-spd-scu (1)     .
           move      fil-spd-scu (2)      to   rf-zpv-spd-scu (2)     .
           move      fil-spd-scu (3)      to   rf-zpv-spd-scu (3)     .
           move      fil-spd-scu (4)      to   rf-zpv-spd-scu (4)     .
           move      fil-spd-scu (5)      to   rf-zpv-spd-scu (5)     .
           move      fil-spd-scu (6)      to   rf-zpv-spd-scu (6)     .
      *    *                                                           *
           move      fil-spd-pvg (1, 1)   to   rf-zpv-spd-pvg (1, 1)  .
           move      fil-spd-pvg (1, 2)   to   rf-zpv-spd-pvg (1, 2)  .
           move      fil-spd-pvg (1, 3)   to   rf-zpv-spd-pvg (1, 3)  .
      *    *                                                           *
           move      fil-spd-pvg (2, 1)   to   rf-zpv-spd-pvg (2, 1)  .
           move      fil-spd-pvg (2, 2)   to   rf-zpv-spd-pvg (2, 2)  .
           move      fil-spd-pvg (2, 3)   to   rf-zpv-spd-pvg (2, 3)  .
      *    *                                                           *
           move      fil-spd-pvg (3, 1)   to   rf-zpv-spd-pvg (3, 1)  .
           move      fil-spd-pvg (3, 2)   to   rf-zpv-spd-pvg (3, 2)  .
           move      fil-spd-pvg (3, 3)   to   rf-zpv-spd-pvg (3, 3)  .
      *    *                                                           *
           move      fil-spd-pvg (4, 1)   to   rf-zpv-spd-pvg (4, 1)  .
           move      fil-spd-pvg (4, 2)   to   rf-zpv-spd-pvg (4, 2)  .
           move      fil-spd-pvg (4, 3)   to   rf-zpv-spd-pvg (4, 3)  .
      *    *                                                           *
           move      fil-spd-pvg (5, 1)   to   rf-zpv-spd-pvg (5, 1)  .
           move      fil-spd-pvg (5, 2)   to   rf-zpv-spd-pvg (5, 2)  .
           move      fil-spd-pvg (5, 3)   to   rf-zpv-spd-pvg (5, 3)  .
      *    *                                                           *
           move      fil-spd-pvg (6, 1)   to   rf-zpv-spd-pvg (6, 1)  .
           move      fil-spd-pvg (6, 2)   to   rf-zpv-spd-pvg (6, 2)  .
           move      fil-spd-pvg (6, 3)   to   rf-zpv-spd-pvg (6, 3)  .
      *    *                                                           *
           move      fil-trt-pvi          to   rf-zpv-trt-pvi         .
      *    *                                                           *
           move      fil-spi-sco (1, 1)   to   rf-zpv-spi-sco (1, 1)  .
           move      fil-spi-sco (1, 2)   to   rf-zpv-spi-sco (1, 2)  .
           move      fil-spi-sco (1, 3)   to   rf-zpv-spi-sco (1, 3)  .
           move      fil-spi-sco (1, 4)   to   rf-zpv-spi-sco (1, 4)  .
           move      fil-spi-sco (1, 5)   to   rf-zpv-spi-sco (1, 5)  .
      *    *                                                           *
           move      fil-spi-sco (2, 1)   to   rf-zpv-spi-sco (2, 1)  .
           move      fil-spi-sco (2, 2)   to   rf-zpv-spi-sco (2, 2)  .
           move      fil-spi-sco (2, 3)   to   rf-zpv-spi-sco (2, 3)  .
           move      fil-spi-sco (2, 4)   to   rf-zpv-spi-sco (2, 4)  .
           move      fil-spi-sco (2, 5)   to   rf-zpv-spi-sco (2, 5)  .
      *    *                                                           *
           move      fil-spi-sco (3, 1)   to   rf-zpv-spi-sco (3, 1)  .
           move      fil-spi-sco (3, 2)   to   rf-zpv-spi-sco (3, 2)  .
           move      fil-spi-sco (3, 3)   to   rf-zpv-spi-sco (3, 3)  .
           move      fil-spi-sco (3, 4)   to   rf-zpv-spi-sco (3, 4)  .
           move      fil-spi-sco (3, 5)   to   rf-zpv-spi-sco (3, 5)  .
      *    *                                                           *
           move      fil-spi-sco (4, 1)   to   rf-zpv-spi-sco (4, 1)  .
           move      fil-spi-sco (4, 2)   to   rf-zpv-spi-sco (4, 2)  .
           move      fil-spi-sco (4, 3)   to   rf-zpv-spi-sco (4, 3)  .
           move      fil-spi-sco (4, 4)   to   rf-zpv-spi-sco (4, 4)  .
           move      fil-spi-sco (4, 5)   to   rf-zpv-spi-sco (4, 5)  .
      *    *                                                           *
           move      fil-spi-sco (5, 1)   to   rf-zpv-spi-sco (5, 1)  .
           move      fil-spi-sco (5, 2)   to   rf-zpv-spi-sco (5, 2)  .
           move      fil-spi-sco (5, 3)   to   rf-zpv-spi-sco (5, 3)  .
           move      fil-spi-sco (5, 4)   to   rf-zpv-spi-sco (5, 4)  .
           move      fil-spi-sco (5, 5)   to   rf-zpv-spi-sco (5, 5)  .
      *    *                                                           *
           move      fil-spi-sco (6, 1)   to   rf-zpv-spi-sco (6, 1)  .
           move      fil-spi-sco (6, 2)   to   rf-zpv-spi-sco (6, 2)  .
           move      fil-spi-sco (6, 3)   to   rf-zpv-spi-sco (6, 3)  .
           move      fil-spi-sco (6, 4)   to   rf-zpv-spi-sco (6, 4)  .
           move      fil-spi-sco (6, 5)   to   rf-zpv-spi-sco (6, 5)  .
      *    *                                                           *
           move      fil-spi-scu (1)      to   rf-zpv-spi-scu (1)     .
           move      fil-spi-scu (2)      to   rf-zpv-spi-scu (2)     .
           move      fil-spi-scu (3)      to   rf-zpv-spi-scu (3)     .
           move      fil-spi-scu (4)      to   rf-zpv-spi-scu (4)     .
           move      fil-spi-scu (5)      to   rf-zpv-spi-scu (5)     .
           move      fil-spi-scu (6)      to   rf-zpv-spi-scu (6)     .
      *    *                                                           *
           move      fil-spi-pvg (1, 1)   to   rf-zpv-spi-pvg (1, 1)  .
           move      fil-spi-pvg (1, 2)   to   rf-zpv-spi-pvg (1, 2)  .
           move      fil-spi-pvg (1, 3)   to   rf-zpv-spi-pvg (1, 3)  .
      *    *                                                           *
           move      fil-spi-pvg (2, 1)   to   rf-zpv-spi-pvg (2, 1)  .
           move      fil-spi-pvg (2, 2)   to   rf-zpv-spi-pvg (2, 2)  .
           move      fil-spi-pvg (2, 3)   to   rf-zpv-spi-pvg (2, 3)  .
      *    *                                                           *
           move      fil-spi-pvg (3, 1)   to   rf-zpv-spi-pvg (3, 1)  .
           move      fil-spi-pvg (3, 2)   to   rf-zpv-spi-pvg (3, 2)  .
           move      fil-spi-pvg (3, 3)   to   rf-zpv-spi-pvg (3, 3)  .
      *    *                                                           *
           move      fil-spi-pvg (4, 1)   to   rf-zpv-spi-pvg (4, 1)  .
           move      fil-spi-pvg (4, 2)   to   rf-zpv-spi-pvg (4, 2)  .
           move      fil-spi-pvg (4, 3)   to   rf-zpv-spi-pvg (4, 3)  .
      *    *                                                           *
           move      fil-spi-pvg (5, 1)   to   rf-zpv-spi-pvg (5, 1)  .
           move      fil-spi-pvg (5, 2)   to   rf-zpv-spi-pvg (5, 2)  .
           move      fil-spi-pvg (5, 3)   to   rf-zpv-spi-pvg (5, 3)  .
      *    *                                                           *
           move      fil-spi-pvg (6, 1)   to   rf-zpv-spi-pvg (6, 1)  .
           move      fil-spi-pvg (6, 2)   to   rf-zpv-spi-pvg (6, 2)  .
           move      fil-spi-pvg (6, 3)   to   rf-zpv-spi-pvg (6, 3)  .
      *    *                                                           *
           move      fil-alx-exp          to   rf-zpv-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-zpv               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-zpv
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

