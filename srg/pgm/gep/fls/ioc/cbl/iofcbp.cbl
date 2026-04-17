       Identification Division.
       Program-Id.                                 iofcbp             .
      *================================================================*
      *                                                                *
      *                  Input-Output File cbp                         *
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
      *            * Chiave numero 01 : CODCBP                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-cbp        pic  9(02)                  .
                   15  fil-cod-cbp        pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-tip-cbp-2      pic  9(02)                  .
                   15  fil-cod-cbp-2      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-cbp-3      pic  9(02)                  .
                   15  fil-des-key        pic  x(40)                  .
                   15  fil-cod-cbp-3      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ABICAB                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-tip-cbp-4      pic  9(02)                  .
                   15  fil-abi-ban        pic  9(05)       comp-3     .
                   15  fil-cab-ban        pic  9(05)       comp-3     .
                   15  fil-cod-cbp-4      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-des-cbp            pic  x(40)                  .
               10  fil-inf-001.
                   15  fil-stc-csc        pic  9(07)       comp-3     .
                   15  fil-stc-csa        pic  9(07)       comp-3     .
                   15  fil-stc-opc        pic  9(07)       comp-3     .
                   15  fil-alx-001.
                       20  filler occurs 36
                                          pic  x(01)                  .
               10  fil-inf-002.
                   15  fil-sgl-ccb        pic  x(12)                  .
                   15  fil-stc-cco        pic  9(07)       comp-3     .
                   15  fil-stc-bba        pic  9(07)       comp-3     .
                   15  fil-stc-bbp        pic  9(07)       comp-3     .
                   15  fil-stc-psm        pic  9(07)       comp-3     .
                   15  fil-stc-psi        pic  9(07)       comp-3     .
                   15  fil-stc-pdi        pic  9(07)       comp-3     .
                   15  fil-stc-psc        pic  9(07)       comp-3     .
                   15  fil-stc-anv        pic  9(07)       comp-3     .
                   15  fil-stc-dfp        pic  9(07)       comp-3     .
                   15  fil-stc-dfa        pic  9(07)       comp-3     .
                   15  fil-stc-onb        pic  9(07)       comp-3     .
                   15  fil-stc-inb        pic  9(07)       comp-3     .
                   15  fil-cin-ita        pic  x(01)                  .
                   15  fil-cin-eur        pic  x(02)                  .
                   15  fil-ide-cbp        pic  9(07)                  .
                   15  fil-alx-002.
                       20  filler occurs 30
                                          pic  x(01)                  .
               10  fil-inf-003.
                   15  fil-sgl-ccp        pic  x(12)                  .
                   15  fil-stc-bpa        pic  9(07)       comp-3     .
                   15  fil-stc-bpp        pic  9(07)       comp-3     .
                   15  fil-stc-onp        pic  9(07)       comp-3     .
                   15  fil-alx-003.
                       20  filler occurs 36
                                          pic  x(01)                  .

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
      *            * Chiave numero 01 : CODCBP                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-cbp        pic  9(02)                  .
                   15  pul-cod-cbp        pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-tip-cbp-2      pic  9(02)                  .
                   15  pul-cod-cbp-2      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-cbp-3      pic  9(02)                  .
                   15  pul-des-key        pic  x(40)                  .
                   15  pul-cod-cbp-3      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ABICAB                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-tip-cbp-4      pic  9(02)                  .
                   15  pul-abi-ban        pic  9(05)       comp-3     .
                   15  pul-cab-ban        pic  9(05)       comp-3     .
                   15  pul-cod-cbp-4      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-des-cbp            pic  x(40)                  .
               10  pul-inf-001.
                   15  pul-stc-csc        pic  9(07)       comp-3     .
                   15  pul-stc-csa        pic  9(07)       comp-3     .
                   15  pul-stc-opc        pic  9(07)       comp-3     .
                   15  pul-alx-001.
                       20  puller occurs 36
                                          pic  x(01)                  .
               10  pul-inf-002.
                   15  pul-sgl-ccb        pic  x(12)                  .
                   15  pul-stc-cco        pic  9(07)       comp-3     .
                   15  pul-stc-bba        pic  9(07)       comp-3     .
                   15  pul-stc-bbp        pic  9(07)       comp-3     .
                   15  pul-stc-psm        pic  9(07)       comp-3     .
                   15  pul-stc-psi        pic  9(07)       comp-3     .
                   15  pul-stc-pdi        pic  9(07)       comp-3     .
                   15  pul-stc-psc        pic  9(07)       comp-3     .
                   15  pul-stc-anv        pic  9(07)       comp-3     .
                   15  pul-stc-dfp        pic  9(07)       comp-3     .
                   15  pul-stc-dfa        pic  9(07)       comp-3     .
                   15  pul-stc-onb        pic  9(07)       comp-3     .
                   15  pul-stc-inb        pic  9(07)       comp-3     .
                   15  pul-cin-ita        pic  x(01)                  .
                   15  pul-cin-eur        pic  x(02)                  .
                   15  pul-ide-cbp        pic  9(07)                  .
                   15  pul-alx-002.
                       20  puller occurs 30
                                          pic  x(01)                  .
               10  pul-inf-003.
                   15  pul-sgl-ccp        pic  x(12)                  .
                   15  pul-stc-bpa        pic  9(07)       comp-3     .
                   15  pul-stc-bpp        pic  9(07)       comp-3     .
                   15  pul-stc-onp        pic  9(07)       comp-3     .
                   15  pul-alx-003.
                       20  filler occurs 40
                                          pic  x(01)                  .

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
                     "cbp "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/gep/fls/ioc/obj/iofcbp              "       .

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
           05  k-ctr                      pic  9(02) value 4          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODCBP    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DESKEY    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ABICAB    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    4      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [cbp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .

      ******************************************************************
       Procedure Division                using f rf-cbp               .
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
           move      spaces               to   rf-cbp                 .
           move      zero                 to   rf-cbp-ide-dat         .
           move      spaces               to   rf-cbp-ide-ute         .
           move      spaces               to   rf-cbp-ide-fas         .
           move      zero                 to   rf-cbp-tip-cbp         .
           move      spaces               to   rf-cbp-cod-cbp         .
           move      spaces               to   rf-cbp-des-cbp         .
           move      spaces               to   rf-cbp-des-key         .
           move      zero                 to   rf-cbp-stc-csc         .
           move      zero                 to   rf-cbp-stc-csa         .
           move      zero                 to   rf-cbp-stc-opc         .
           move      spaces               to   rf-cbp-alx-001         .
           move      zero                 to   rf-cbp-abi-ban         .
           move      zero                 to   rf-cbp-cab-ban         .
           move      spaces               to   rf-cbp-sgl-ccb         .
           move      zero                 to   rf-cbp-stc-cco         .
           move      zero                 to   rf-cbp-stc-bba         .
           move      zero                 to   rf-cbp-stc-bbp         .
           move      zero                 to   rf-cbp-stc-psm         .
           move      zero                 to   rf-cbp-stc-psi         .
           move      zero                 to   rf-cbp-stc-pdi         .
           move      zero                 to   rf-cbp-stc-psc         .
           move      zero                 to   rf-cbp-stc-anv         .
           move      zero                 to   rf-cbp-stc-dfp         .
           move      zero                 to   rf-cbp-stc-dfa         .
           move      zero                 to   rf-cbp-stc-onb         .
           move      zero                 to   rf-cbp-stc-inb         .
           move      spaces               to   rf-cbp-cin-ita         .
           move      spaces               to   rf-cbp-cin-eur         .
           move      zero                 to   rf-cbp-ide-cbp         .
           move      spaces               to   rf-cbp-alx-002         .
           move      spaces               to   rf-cbp-sgl-ccp         .
           move      zero                 to   rf-cbp-stc-bpa         .
           move      zero                 to   rf-cbp-stc-bpp         .
           move      zero                 to   rf-cbp-stc-onp         .
           move      spaces               to   rf-cbp-alx-003         .
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
           move      rf-cbp-ide-ute       to   fil-ide-ute            .
           move      rf-cbp-ide-fas       to   fil-ide-fas            .
           move      rf-cbp-des-cbp       to   fil-des-cbp            .
           move      rf-cbp-stc-csc       to   fil-stc-csc            .
           move      rf-cbp-stc-csa       to   fil-stc-csa            .
           move      rf-cbp-stc-opc       to   fil-stc-opc            .
           move      rf-cbp-alx-001       to   fil-alx-001            .
           move      rf-cbp-sgl-ccb       to   fil-sgl-ccb            .
           move      rf-cbp-stc-cco       to   fil-stc-cco            .
           move      rf-cbp-stc-bba       to   fil-stc-bba            .
           move      rf-cbp-stc-bbp       to   fil-stc-bbp            .
           move      rf-cbp-stc-psm       to   fil-stc-psm            .
           move      rf-cbp-stc-psi       to   fil-stc-psi            .
           move      rf-cbp-stc-pdi       to   fil-stc-pdi            .
           move      rf-cbp-stc-psc       to   fil-stc-psc            .
           move      rf-cbp-stc-anv       to   fil-stc-anv            .
           move      rf-cbp-stc-dfp       to   fil-stc-dfp            .
           move      rf-cbp-stc-dfa       to   fil-stc-dfa            .
           move      rf-cbp-stc-onb       to   fil-stc-onb            .
           move      rf-cbp-stc-inb       to   fil-stc-inb            .
           move      rf-cbp-cin-ita       to   fil-cin-ita            .
           move      rf-cbp-cin-eur       to   fil-cin-eur            .
           move      rf-cbp-ide-cbp       to   fil-ide-cbp            .
           move      rf-cbp-alx-002       to   fil-alx-002            .
           move      rf-cbp-sgl-ccp       to   fil-sgl-ccp            .
           move      rf-cbp-stc-bpa       to   fil-stc-bpa            .
           move      rf-cbp-stc-bpp       to   fil-stc-bpp            .
           move      rf-cbp-stc-onp       to   fil-stc-onp            .
           move      rf-cbp-alx-003       to   fil-alx-003            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-cbp-tip-cbp       to   fil-tip-cbp            .
           move      rf-cbp-cod-cbp       to   fil-cod-cbp            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-cbp-ide-dat       to   fil-ide-dat            .
           move      rf-cbp-tip-cbp       to   fil-tip-cbp-2          .
           move      rf-cbp-cod-cbp       to   fil-cod-cbp-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-cbp-tip-cbp       to   fil-tip-cbp-3          .
           move      rf-cbp-des-key       to   fil-des-key            .
           move      rf-cbp-cod-cbp       to   fil-cod-cbp-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-cbp-tip-cbp       to   fil-tip-cbp-4          .
           move      rf-cbp-abi-ban       to   fil-abi-ban            .
           move      rf-cbp-cab-ban       to   fil-cab-ban            .
           move      rf-cbp-cod-cbp       to   fil-cod-cbp-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-cbp                 .
           move      fil-ide-dat          to   rf-cbp-ide-dat         .
           move      fil-ide-ute          to   rf-cbp-ide-ute         .
           move      fil-ide-fas          to   rf-cbp-ide-fas         .
           move      fil-tip-cbp          to   rf-cbp-tip-cbp         .
           move      fil-cod-cbp          to   rf-cbp-cod-cbp         .
           move      fil-des-cbp          to   rf-cbp-des-cbp         .
           move      fil-des-key          to   rf-cbp-des-key         .
           move      fil-stc-csc          to   rf-cbp-stc-csc         .
           move      fil-stc-csa          to   rf-cbp-stc-csa         .
           move      fil-stc-opc          to   rf-cbp-stc-opc         .
           move      fil-alx-001          to   rf-cbp-alx-001         .
           move      fil-abi-ban          to   rf-cbp-abi-ban         .
           move      fil-cab-ban          to   rf-cbp-cab-ban         .
           move      fil-sgl-ccb          to   rf-cbp-sgl-ccb         .
           move      fil-stc-cco          to   rf-cbp-stc-cco         .
           move      fil-stc-bba          to   rf-cbp-stc-bba         .
           move      fil-stc-bbp          to   rf-cbp-stc-bbp         .
           move      fil-stc-psm          to   rf-cbp-stc-psm         .
           move      fil-stc-psi          to   rf-cbp-stc-psi         .
           move      fil-stc-pdi          to   rf-cbp-stc-pdi         .
           move      fil-stc-psc          to   rf-cbp-stc-psc         .
           move      fil-stc-anv          to   rf-cbp-stc-anv         .
           move      fil-stc-dfp          to   rf-cbp-stc-dfp         .
           move      fil-stc-dfa          to   rf-cbp-stc-dfa         .
           move      fil-stc-onb          to   rf-cbp-stc-onb         .
           move      fil-stc-inb          to   rf-cbp-stc-inb         .
           move      fil-cin-ita          to   rf-cbp-cin-ita         .
           move      fil-cin-eur          to   rf-cbp-cin-eur         .
           move      fil-ide-cbp          to   rf-cbp-ide-cbp         .
           move      fil-alx-002          to   rf-cbp-alx-002         .
           move      fil-sgl-ccp          to   rf-cbp-sgl-ccp         .
           move      fil-stc-bpa          to   rf-cbp-stc-bpa         .
           move      fil-stc-bpp          to   rf-cbp-stc-bpp         .
           move      fil-stc-onp          to   rf-cbp-stc-onp         .
           move      fil-alx-003          to   rf-cbp-alx-003         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-cbp               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-cbp
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

