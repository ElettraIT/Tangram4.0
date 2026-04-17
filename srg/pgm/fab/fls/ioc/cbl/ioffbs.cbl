       Identification Division.
       Program-Id.                                 ioffbs             .
      *================================================================*
      *                                                                *
      *                  Input-Output File fbs                         *
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
      *            * Chiave numero 01 : MAGDPZ                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
                   15  fil-var-mag        pic  x(14)                  .
                   15  fil-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DPZMAG                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz-2      pic  9(02)                  .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-num-mag-2      pic  9(07)       comp-3     .
                   15  fil-var-mag-2      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tip-stg            pic  x(03)                  .
               10  fil-sco-stg occurs 4.
                   15  fil-sco-min        pic  9(10)v9(03) comp-3     .
                   15  fil-dua-min        pic  9(07)       comp-3     .
                   15  fil-sco-sic        pic  9(10)v9(03) comp-3     .
                   15  fil-dua-sic        pic  9(07)       comp-3     .
                   15  fil-sco-max        pic  9(10)v9(03) comp-3     .
                   15  fil-dua-max        pic  9(07)       comp-3     .
               10  fil-sco-not            pic  x(20)                  .
               10  fil-exp-alf.
                   15  filler occurs 20   pic  x(01)                  .
               10  fil-exp-num.
                   15  filler occurs 20   pic  9(01)                  .

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
      *            * Chiave numero 01 : MAGDPZ                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
                   15  pul-var-mag        pic  x(14)                  .
                   15  pul-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DPZMAG                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-cod-dpz-2      pic  9(02)                  .
                   15  pul-tip-mag-2      pic  9(02)                  .
                   15  pul-num-mag-2      pic  9(07)       comp-3     .
                   15  pul-var-mag-2      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tip-stg            pic  x(03)                  .
               10  pul-sco-stg occurs 4.
                   15  pul-sco-min        pic  9(10)v9(03) comp-3     .
                   15  pul-dua-min        pic  9(07)       comp-3     .
                   15  pul-sco-sic        pic  9(10)v9(03) comp-3     .
                   15  pul-dua-sic        pic  9(07)       comp-3     .
                   15  pul-sco-max        pic  9(10)v9(03) comp-3     .
                   15  pul-dua-max        pic  9(07)       comp-3     .
               10  pul-sco-not            pic  x(20)                  .
               10  pul-exp-alf.
                   15  filler occurs 20   pic  x(01)                  .
               10  pul-exp-num.
                   15  filler occurs 20   pic  9(01)                  .

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
                     "fbs "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/fab/fls/ioc/obj/ioffbs              "       .

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
           05  k-ctr                      pic  9(02) value 2          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "MAGDPZ    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZMAG    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    2      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [fbs]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rffbs"                          .

      ******************************************************************
       Procedure Division                using f rf-fbs               .
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
           move      e-not-fnd            to   f-sts                  .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-fbs                 .
           move      zero                 to   rf-fbs-cod-dpz         .
           move      zero                 to   rf-fbs-tip-mag         .
           move      zero                 to   rf-fbs-num-mag         .
           move      spaces               to   rf-fbs-var-mag         .
           move      spaces               to   rf-fbs-tip-stg         .
      *              *                                                 *
           move      zero                 to   rf-fbs-sco-min (1)     .
           move      zero                 to   rf-fbs-sco-min (2)     .
           move      zero                 to   rf-fbs-sco-min (3)     .
           move      zero                 to   rf-fbs-sco-min (4)     .
      *              *                                                 *
           move      zero                 to   rf-fbs-dua-min (1)     .
           move      zero                 to   rf-fbs-dua-min (2)     .
           move      zero                 to   rf-fbs-dua-min (3)     .
           move      zero                 to   rf-fbs-dua-min (4)     .
      *              *                                                 *
           move      zero                 to   rf-fbs-sco-sic (1)     .
           move      zero                 to   rf-fbs-sco-sic (2)     .
           move      zero                 to   rf-fbs-sco-sic (3)     .
           move      zero                 to   rf-fbs-sco-sic (4)     .
      *              *                                                 *
           move      zero                 to   rf-fbs-dua-sic (1)     .
           move      zero                 to   rf-fbs-dua-sic (2)     .
           move      zero                 to   rf-fbs-dua-sic (3)     .
           move      zero                 to   rf-fbs-dua-sic (4)     .
      *              *                                                 *
           move      zero                 to   rf-fbs-sco-max (1)     .
           move      zero                 to   rf-fbs-sco-max (2)     .
           move      zero                 to   rf-fbs-sco-max (3)     .
           move      zero                 to   rf-fbs-sco-max (4)     .
      *              *                                                 *
           move      zero                 to   rf-fbs-dua-max (1)     .
           move      zero                 to   rf-fbs-dua-max (2)     .
           move      zero                 to   rf-fbs-dua-max (3)     .
           move      zero                 to   rf-fbs-dua-max (4)     .
      *              *                                                 *
           move      spaces               to   rf-fbs-exp-alf         .
           move      all zero             to   rf-fbs-exp-num         .
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
           move      rf-fbs-tip-stg       to   fil-tip-stg            .
      *              *                                                 *
           move      rf-fbs-sco-min (1)   to   fil-sco-min (1)        .
           move      rf-fbs-sco-min (2)   to   fil-sco-min (2)        .
           move      rf-fbs-sco-min (3)   to   fil-sco-min (3)        .
           move      rf-fbs-sco-min (4)   to   fil-sco-min (4)        .
      *              *                                                 *
           move      rf-fbs-dua-min (1)   to   fil-dua-min (1)        .
           move      rf-fbs-dua-min (2)   to   fil-dua-min (2)        .
           move      rf-fbs-dua-min (3)   to   fil-dua-min (3)        .
           move      rf-fbs-dua-min (4)   to   fil-dua-min (4)        .
      *              *                                                 *
           move      rf-fbs-sco-sic (1)   to   fil-sco-sic (1)        .
           move      rf-fbs-sco-sic (2)   to   fil-sco-sic (2)        .
           move      rf-fbs-sco-sic (3)   to   fil-sco-sic (3)        .
           move      rf-fbs-sco-sic (4)   to   fil-sco-sic (4)        .
      *              *                                                 *
           move      rf-fbs-dua-sic (1)   to   fil-dua-sic (1)        .
           move      rf-fbs-dua-sic (2)   to   fil-dua-sic (2)        .
           move      rf-fbs-dua-sic (3)   to   fil-dua-sic (3)        .
           move      rf-fbs-dua-sic (4)   to   fil-dua-sic (4)        .
      *              *                                                 *
           move      rf-fbs-sco-max (1)   to   fil-sco-max (1)        .
           move      rf-fbs-sco-max (2)   to   fil-sco-max (2)        .
           move      rf-fbs-sco-max (3)   to   fil-sco-max (3)        .
           move      rf-fbs-sco-max (4)   to   fil-sco-max (4)        .
      *              *                                                 *
           move      rf-fbs-dua-max (1)   to   fil-dua-max (1)        .
           move      rf-fbs-dua-max (2)   to   fil-dua-max (2)        .
           move      rf-fbs-dua-max (3)   to   fil-dua-max (3)        .
           move      rf-fbs-dua-max (4)   to   fil-dua-max (4)        .
      *              *                                                 *
           move      rf-fbs-sco-not       to   fil-sco-not            .
           move      rf-fbs-exp-alf       to   fil-exp-alf            .
           move      rf-fbs-exp-num       to   fil-exp-num            .
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
           move      rf-fbs-tip-mag       to   fil-tip-mag            .
           move      rf-fbs-num-mag       to   fil-num-mag            .
           move      rf-fbs-var-mag       to   fil-var-mag            .
           move      rf-fbs-cod-dpz       to   fil-cod-dpz            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-fbs-cod-dpz       to   fil-cod-dpz-2          .
           move      rf-fbs-tip-mag       to   fil-tip-mag-2          .
           move      rf-fbs-num-mag       to   fil-num-mag-2          .
           move      rf-fbs-var-mag       to   fil-var-mag-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-fbs                 .
           move      fil-cod-dpz          to   rf-fbs-cod-dpz         .
           move      fil-tip-mag          to   rf-fbs-tip-mag         .
           move      fil-num-mag          to   rf-fbs-num-mag         .
           move      fil-var-mag          to   rf-fbs-var-mag         .
           move      fil-tip-stg          to   rf-fbs-tip-stg         .
      *              *                                                 *
           move      fil-sco-min (1)      to   rf-fbs-sco-min (1)     .
           move      fil-sco-min (2)      to   rf-fbs-sco-min (2)     .
           move      fil-sco-min (3)      to   rf-fbs-sco-min (3)     .
           move      fil-sco-min (4)      to   rf-fbs-sco-min (4)     .
      *              *                                                 *
           move      fil-dua-min (1)      to   rf-fbs-dua-min (1)     .
           move      fil-dua-min (2)      to   rf-fbs-dua-min (2)     .
           move      fil-dua-min (3)      to   rf-fbs-dua-min (3)     .
           move      fil-dua-min (4)      to   rf-fbs-dua-min (4)     .
      *              *                                                 *
           move      fil-sco-sic (1)      to   rf-fbs-sco-sic (1)     .
           move      fil-sco-sic (2)      to   rf-fbs-sco-sic (2)     .
           move      fil-sco-sic (3)      to   rf-fbs-sco-sic (3)     .
           move      fil-sco-sic (4)      to   rf-fbs-sco-sic (4)     .
      *              *                                                 *
           move      fil-dua-sic (1)      to   rf-fbs-dua-sic (1)     .
           move      fil-dua-sic (2)      to   rf-fbs-dua-sic (2)     .
           move      fil-dua-sic (3)      to   rf-fbs-dua-sic (3)     .
           move      fil-dua-sic (4)      to   rf-fbs-dua-sic (4)     .
      *              *                                                 *
           move      fil-sco-max (1)      to   rf-fbs-sco-max (1)     .
           move      fil-sco-max (2)      to   rf-fbs-sco-max (2)     .
           move      fil-sco-max (3)      to   rf-fbs-sco-max (3)     .
           move      fil-sco-max (4)      to   rf-fbs-sco-max (4)     .
      *              *                                                 *
           move      fil-dua-max (1)      to   rf-fbs-dua-max (1)     .
           move      fil-dua-max (2)      to   rf-fbs-dua-max (2)     .
           move      fil-dua-max (3)      to   rf-fbs-dua-max (3)     .
           move      fil-dua-max (4)      to   rf-fbs-dua-max (4)     .
      *              *                                                 *
           move      fil-sco-not          to   rf-fbs-sco-not         .
           move      fil-exp-alf          to   rf-fbs-exp-alf         .
           move      fil-exp-num          to   rf-fbs-exp-num         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-fbs               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-fbs
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

