       Identification Division.
       Program-Id.                                 iofhlf             .
      *================================================================*
      *                                                                *
      *                  Input-Output File hlf                         *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.        N-d-K-Sia-PD .
       Object-Computer.        N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

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
      *            * Chiave numero 01 : ALFFNT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-alf        pic  x(20)                  .
                   15  fil-cod-fnt        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-des-key        pic  x(40)                  .
                   15  fil-cod-alf-2      pic  x(20)                  .
                   15  fil-cod-fnt-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNTALF                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-fnt-3      pic  9(07)       comp-3     .
                   15  fil-cod-alf-3      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PROCOD                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-num-pro        pic  9(07)       comp-3     .
                   15  fil-cod-alf-4      pic  x(20)                  .
                   15  fil-cod-fnt-4      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-alf            pic  x(40)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-dec-prz            pic  9(01)                  .
               10  fil-prz-lst            pic  9(09)       comp-3     .
               10  fil-prz-spc            pic  9(09)       comp-3     .
               10  fil-dat-spc            pic  9(07)       comp-3     .
               10  fil-qta-prz            pic  9(07)       comp-3     .
               10  fil-qta-cfz            pic  9(07)       comp-3     .
               10  fil-min-acq            pic  9(07)       comp-3     .
               10  fil-dat-agg            pic  9(07)       comp-3     .
               10  fil-lib-lst.
                   15  filler occurs 512  pic  x(01)                  .
               10  fil-exp-num.
                   15  filler  occurs 20  pic  9(01)                  .
               10  fil-exp-alf.
                   15  filler  occurs 80  pic  x(01)                  .

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
      *            * Chiave numero 01 : ALFFNT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-alf        pic  x(20)                  .
                   15  pul-cod-fnt        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-des-key        pic  x(40)                  .
                   15  pul-cod-alf-2      pic  x(20)                  .
                   15  pul-cod-fnt-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNTALF                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-fnt-3      pic  9(07)       comp-3     .
                   15  pul-cod-alf-3      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PROCOD                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-num-pro        pic  9(07)       comp-3     .
                   15  pul-cod-alf-4      pic  x(20)                  .
                   15  pul-cod-fnt-4      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-alf            pic  x(40)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-dec-prz            pic  9(01)                  .
               10  pul-prz-lst            pic  9(09)       comp-3     .
               10  pul-prz-spc            pic  9(09)       comp-3     .
               10  pul-dat-spc            pic  9(07)       comp-3     .
               10  pul-qta-prz            pic  9(07)       comp-3     .
               10  pul-qta-cfz            pic  9(07)       comp-3     .
               10  pul-min-acq            pic  9(07)       comp-3     .
               10  pul-dat-agg            pic  9(07)       comp-3     .
               10  pul-lib-lst.
                   15  filler occurs 512  pic  x(01)                  .
               10  pul-exp-num.
                   15  filler  occurs 20  pic  9(01)                  .
               10  pul-exp-alf.
                   15  filler  occurs 80  pic  x(01)                  .

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
                     "hlf "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "sir/agb/fls/ioc/obj/iofhlf              "       .

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
                            "ALFFNT"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DESKEY"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "FNTALF"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PROCOD"                                  .
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
      *    * Record logico file [hlf]                                  *
      *    *-----------------------------------------------------------*
           copy      "sir/agb/fls/rec/rfhlf"                          .

      ******************************************************************
       Procedure Division                using f rf-hlf               .
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
            if       z-lok                =    1
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
           move      spaces               to   rf-hlf                 .
           move      spaces               to   rf-hlf-cod-alf         .
           move      zero                 to   rf-hlf-cod-fnt         .
           move      spaces               to   rf-hlf-des-alf         .
           move      spaces               to   rf-hlf-des-key         .
           move      zero                 to   rf-hlf-num-pro         .
           move      spaces               to   rf-hlf-sgl-vlt         .
           move      zero                 to   rf-hlf-dec-vlt         .
           move      zero                 to   rf-hlf-dec-prz         .
           move      zero                 to   rf-hlf-prz-lst         .
           move      zero                 to   rf-hlf-prz-spc         .
           move      zero                 to   rf-hlf-dat-spc         .
           move      zero                 to   rf-hlf-qta-prz         .
           move      zero                 to   rf-hlf-qta-cfz         .
           move      zero                 to   rf-hlf-min-acq         .
           move      zero                 to   rf-hlf-dat-agg         .
           move      spaces               to   rf-hlf-lib-lst         .
           move      spaces               to   rf-hlf-exp-alf         .
           move      all zero             to   rf-hlf-exp-num         .
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
           move      rf-hlf-des-alf       to   fil-des-alf            .
           move      rf-hlf-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-hlf-dec-vlt       to   fil-dec-vlt            .
           move      rf-hlf-dec-prz       to   fil-dec-prz            .
           move      rf-hlf-prz-lst       to   fil-prz-lst            .
           move      rf-hlf-prz-spc       to   fil-prz-spc            .
           move      rf-hlf-dat-spc       to   fil-dat-spc            .
           move      rf-hlf-qta-prz       to   fil-qta-prz            .
           move      rf-hlf-qta-cfz       to   fil-qta-cfz            .
           move      rf-hlf-min-acq       to   fil-min-acq            .
           move      rf-hlf-dat-agg       to   fil-dat-agg            .
           move      rf-hlf-lib-lst       to   fil-lib-lst            .
           move      rf-hlf-exp-num       to   fil-exp-num            .
           move      rf-hlf-exp-alf       to   fil-exp-alf            .
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
           move      rf-hlf-cod-alf       to   fil-cod-alf            .
           move      rf-hlf-cod-fnt       to   fil-cod-fnt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-hlf-des-key       to   fil-des-key            .
           move      rf-hlf-cod-alf       to   fil-cod-alf-2          .
           move      rf-hlf-cod-fnt       to   fil-cod-fnt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-hlf-cod-fnt       to   fil-cod-fnt-3          .
           move      rf-hlf-cod-alf       to   fil-cod-alf-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-hlf-num-pro       to   fil-num-pro            .
           move      rf-hlf-cod-alf       to   fil-cod-alf-4          .
           move      rf-hlf-cod-fnt       to   fil-cod-fnt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-hlf                 .
           move      fil-cod-alf          to   rf-hlf-cod-alf         .
           move      fil-cod-fnt          to   rf-hlf-cod-fnt         .
           move      fil-des-alf          to   rf-hlf-des-alf         .
           move      fil-des-key          to   rf-hlf-des-key         .
           move      fil-num-pro          to   rf-hlf-num-pro         .
           move      fil-sgl-vlt          to   rf-hlf-sgl-vlt         .
           move      fil-dec-vlt          to   rf-hlf-dec-vlt         .
           move      fil-dec-prz          to   rf-hlf-dec-prz         .
           move      fil-prz-lst          to   rf-hlf-prz-lst         .
           move      fil-prz-spc          to   rf-hlf-prz-spc         .
           move      fil-dat-spc          to   rf-hlf-dat-spc         .
           move      fil-qta-prz          to   rf-hlf-qta-prz         .
           move      fil-qta-cfz          to   rf-hlf-qta-cfz         .
           move      fil-min-acq          to   rf-hlf-min-acq         .
           move      fil-dat-agg          to   rf-hlf-dat-agg         .
           move      fil-lib-lst          to   rf-hlf-lib-lst         .
           move      fil-exp-num          to   rf-hlf-exp-num         .
           move      fil-exp-alf          to   rf-hlf-exp-alf         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-hlf               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-hlf
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

