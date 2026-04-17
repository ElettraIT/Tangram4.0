       Identification Division.
       Program-Id.                                 iofmim             .
      *================================================================*
      *                                                                *
      *                  Input-Output File mim                         *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

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
      *            * Chiave numero 01 : MAGTGC                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
                   15  fil-var-mag        pic  x(14)                  .
                   15  fil-tip-gia        pic  9(02)                  .
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-dpz-arc        pic  x(04)                  .
                   15  fil-cod-mic        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : TGCARC                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz-2      pic  9(02)                  .
                   15  fil-tip-gia-2      pic  9(02)                  .
                   15  fil-tip-arc-2      pic  x(01)                  .
                   15  fil-cod-arc-2      pic  9(07)       comp-3     .
                   15  fil-dpz-arc-2      pic  x(04)                  .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-alf-mag        pic  x(14)                  .
                   15  fil-num-mag-2      pic  9(07)       comp-3     .
                   15  fil-var-mag-2      pic  x(14)                  .
                   15  fil-cod-mic-2      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : TGCARCMIC                      *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-gia-3      pic  9(02)                  .
                   15  fil-cod-dpz-3      pic  9(02)                  .
                   15  fil-tip-arc-3      pic  x(01)                  .
                   15  fil-cod-arc-3      pic  9(07)       comp-3     .
                   15  fil-dpz-arc-3      pic  x(04)                  .
                   15  fil-cod-mic-3      pic  x(03)                  .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-alf-mag-3      pic  x(14)                  .
                   15  fil-num-mag-3      pic  9(07)       comp-3     .
                   15  fil-var-mag-3      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : UBIMAGTGC                      *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-tip-ubi        pic  x(03)                  .
                   15  fil-prm-ubi occurs 4
                                          pic  x(07)                  .
                   15  fil-tip-mag-4      pic  9(02)                  .
                   15  fil-alf-mag-4      pic  x(14)                  .
                   15  fil-num-mag-4      pic  9(07)       comp-3     .
                   15  fil-var-mag-4      pic  x(14)                  .
                   15  fil-tip-gia-4      pic  9(02)                  .
                   15  fil-tip-arc-4      pic  x(01)                  .
                   15  fil-cod-arc-4      pic  9(07)       comp-3     .
                   15  fil-dpz-arc-4      pic  x(04)                  .
                   15  fil-cod-mic-4      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-mag            pic  x(40)                  .
               10  fil-umi-mag            pic  x(03)                  .
               10  fil-dec-qta            pic  9(01)                  .
               10  fil-qta-prs            pic s9(08)v9(03) comp-3     .
               10  fil-flg-ril            pic  x(01)                  .
               10  fil-qta-rlv            pic s9(08)v9(03) comp-3     .
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
      *            * Chiave numero 01 : MAGTGC                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
                   15  pul-var-mag        pic  x(14)                  .
                   15  pul-tip-gia        pic  9(02)                  .
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-dpz-arc        pic  x(04)                  .
                   15  pul-cod-mic        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : TGCARC                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-cod-dpz-2      pic  9(02)                  .
                   15  pul-tip-gia-2      pic  9(02)                  .
                   15  pul-tip-arc-2      pic  x(01)                  .
                   15  pul-cod-arc-2      pic  9(07)       comp-3     .
                   15  pul-dpz-arc-2      pic  x(04)                  .
                   15  pul-tip-mag-2      pic  9(02)                  .
                   15  pul-alf-mag        pic  x(14)                  .
                   15  pul-num-mag-2      pic  9(07)       comp-3     .
                   15  pul-var-mag-2      pic  x(14)                  .
                   15  pul-cod-mic-2      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : TGCARCMIC                      *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-gia-3      pic  9(02)                  .
                   15  pul-cod-dpz-3      pic  9(02)                  .
                   15  pul-tip-arc-3      pic  x(01)                  .
                   15  pul-cod-arc-3      pic  9(07)       comp-3     .
                   15  pul-dpz-arc-3      pic  x(04)                  .
                   15  pul-cod-mic-3      pic  x(03)                  .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-alf-mag-3      pic  x(14)                  .
                   15  pul-num-mag-3      pic  9(07)       comp-3     .
                   15  pul-var-mag-3      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : UBIMAGTGC                      *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-tip-ubi        pic  x(03)                  .
                   15  pul-prm-ubi occurs 4
                                          pic  x(07)                  .
                   15  pul-tip-mag-4      pic  9(02)                  .
                   15  pul-alf-mag-4      pic  x(14)                  .
                   15  pul-num-mag-4      pic  9(07)       comp-3     .
                   15  pul-var-mag-4      pic  x(14)                  .
                   15  pul-tip-gia-4      pic  9(02)                  .
                   15  pul-tip-arc-4      pic  x(01)                  .
                   15  pul-cod-arc-4      pic  9(07)       comp-3     .
                   15  pul-dpz-arc-4      pic  x(04)                  .
                   15  pul-cod-mic-4      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-mag            pic  x(40)                  .
               10  pul-umi-mag            pic  x(03)                  .
               10  pul-dec-qta            pic  9(01)                  .
               10  pul-qta-prs            pic s9(08)v9(03) comp-3     .
               10  pul-flg-ril            pic  x(01)                  .
               10  pul-qta-rlv            pic s9(08)v9(03) comp-3     .
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
      *        * Sigla del file                                        *
      *        *-------------------------------------------------------*
           02  i-ide-sdf                  pic  x(04) value
                     "mim "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/mag/fls/ioc/obj/iofmim              "       .

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
      *    * Area per elenco chiavi previste                           *
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
                            "MAGTGC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "TGCARC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "TGCARCMIC "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "UBIMAGTGC "                              .
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
      *    * Record logico file [mim]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmim"                          .

      ******************************************************************
       Procedure Division                using f rf-mim               .
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
            if       z-lok                =    1
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
            if       z-lok                =    1
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
           move      e-not-fnd            to   f-sts                  .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-mim                 .
           move      zero                 to   rf-mim-cod-dpz         .
           move      zero                 to   rf-mim-tip-mag         .
           move      zero                 to   rf-mim-num-mag         .
           move      spaces               to   rf-mim-var-mag         .
           move      spaces               to   rf-mim-alf-mag         .
           move      spaces               to   rf-mim-des-mag         .
           move      spaces               to   rf-mim-umi-mag         .
           move      zero                 to   rf-mim-dec-qta         .
           move      spaces               to   rf-mim-tip-ubi         .
           move      spaces               to   rf-mim-prm-ubi (1)     .
           move      spaces               to   rf-mim-prm-ubi (2)     .
           move      spaces               to   rf-mim-prm-ubi (3)     .
           move      spaces               to   rf-mim-prm-ubi (4)     .
           move      zero                 to   rf-mim-tip-gia         .
           move      spaces               to   rf-mim-cod-mic         .
           move      spaces               to   rf-mim-tip-arc         .
           move      zero                 to   rf-mim-cod-arc         .
           move      spaces               to   rf-mim-dpz-arc         .
           move      zero                 to   rf-mim-qta-prs         .
           move      spaces               to   rf-mim-flg-ril         .
           move      zero                 to   rf-mim-qta-rlv         .
           move      spaces               to   rf-mim-alx-exp         .
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
           move      rf-mim-des-mag       to   fil-des-mag            .
           move      rf-mim-umi-mag       to   fil-umi-mag            .
           move      rf-mim-dec-qta       to   fil-dec-qta            .
           move      rf-mim-qta-prs       to   fil-qta-prs            .
           move      rf-mim-flg-ril       to   fil-flg-ril            .
           move      rf-mim-qta-rlv       to   fil-qta-rlv            .
           move      rf-mim-alx-exp       to   fil-alx-exp            .
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
           move      rf-mim-cod-dpz       to   fil-cod-dpz            .
           move      rf-mim-tip-mag       to   fil-tip-mag            .
           move      rf-mim-num-mag       to   fil-num-mag            .
           move      rf-mim-var-mag       to   fil-var-mag            .
           move      rf-mim-tip-gia       to   fil-tip-gia            .
           move      rf-mim-tip-arc       to   fil-tip-arc            .
           move      rf-mim-cod-arc       to   fil-cod-arc            .
           move      rf-mim-dpz-arc       to   fil-dpz-arc            .
           move      rf-mim-cod-mic       to   fil-cod-mic            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-mim-cod-dpz       to   fil-cod-dpz-2          .
           move      rf-mim-tip-gia       to   fil-tip-gia-2          .
           move      rf-mim-tip-arc       to   fil-tip-arc-2          .
           move      rf-mim-cod-arc       to   fil-cod-arc-2          .
           move      rf-mim-dpz-arc       to   fil-dpz-arc-2          .
           move      rf-mim-tip-mag       to   fil-tip-mag-2          .
           move      rf-mim-alf-mag       to   fil-alf-mag            .
           move      rf-mim-num-mag       to   fil-num-mag-2          .
           move      rf-mim-var-mag       to   fil-var-mag-2          .
           move      rf-mim-cod-mic       to   fil-cod-mic-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-mim-tip-gia       to   fil-tip-gia-3          .
           move      rf-mim-cod-dpz       to   fil-cod-dpz-3          .
           move      rf-mim-tip-arc       to   fil-tip-arc-3          .
           move      rf-mim-cod-arc       to   fil-cod-arc-3          .
           move      rf-mim-dpz-arc       to   fil-dpz-arc-3          .
           move      rf-mim-cod-mic       to   fil-cod-mic-3          .
           move      rf-mim-tip-mag       to   fil-tip-mag-3          .
           move      rf-mim-alf-mag       to   fil-alf-mag-3          .
           move      rf-mim-num-mag       to   fil-num-mag-3          .
           move      rf-mim-var-mag       to   fil-var-mag-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-mim-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-mim-tip-ubi       to   fil-tip-ubi            .
           move      rf-mim-prm-ubi (1)   to   fil-prm-ubi (1)        .
           move      rf-mim-prm-ubi (2)   to   fil-prm-ubi (2)        .
           move      rf-mim-prm-ubi (3)   to   fil-prm-ubi (3)        .
           move      rf-mim-prm-ubi (4)   to   fil-prm-ubi (4)        .
           move      rf-mim-tip-mag       to   fil-tip-mag-4          .
           move      rf-mim-alf-mag       to   fil-alf-mag-4          .
           move      rf-mim-num-mag       to   fil-num-mag-4          .
           move      rf-mim-var-mag       to   fil-var-mag-4          .
           move      rf-mim-tip-gia       to   fil-tip-gia-4          .
           move      rf-mim-tip-arc       to   fil-tip-arc-4          .
           move      rf-mim-cod-arc       to   fil-cod-arc-4          .
           move      rf-mim-dpz-arc       to   fil-dpz-arc-4          .
           move      rf-mim-cod-mic       to   fil-cod-mic-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-mim                 .
           move      fil-cod-dpz          to   rf-mim-cod-dpz         .
           move      fil-tip-mag          to   rf-mim-tip-mag         .
           move      fil-num-mag          to   rf-mim-num-mag         .
           move      fil-var-mag          to   rf-mim-var-mag         .
           move      fil-alf-mag          to   rf-mim-alf-mag         .
           move      fil-des-mag          to   rf-mim-des-mag         .
           move      fil-umi-mag          to   rf-mim-umi-mag         .
           move      fil-dec-qta          to   rf-mim-dec-qta         .
           move      fil-tip-ubi          to   rf-mim-tip-ubi         .
           move      fil-prm-ubi (1)      to   rf-mim-prm-ubi (1)     .
           move      fil-prm-ubi (2)      to   rf-mim-prm-ubi (2)     .
           move      fil-prm-ubi (3)      to   rf-mim-prm-ubi (3)     .
           move      fil-prm-ubi (4)      to   rf-mim-prm-ubi (4)     .
           move      fil-tip-gia          to   rf-mim-tip-gia         .
           move      fil-cod-mic          to   rf-mim-cod-mic         .
           move      fil-tip-arc          to   rf-mim-tip-arc         .
           move      fil-cod-arc          to   rf-mim-cod-arc         .
           move      fil-dpz-arc          to   rf-mim-dpz-arc         .
           move      fil-qta-prs          to   rf-mim-qta-prs         .
           move      fil-flg-ril          to   rf-mim-flg-ril         .
           move      fil-qta-rlv          to   rf-mim-qta-rlv         .
           move      fil-alx-exp          to   rf-mim-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-mim               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-mim
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

