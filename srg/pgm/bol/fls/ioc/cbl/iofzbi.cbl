       Identification Division.
       Program-Id.                                 iofzbi             .
      *================================================================*
      *                                                                *
      *                  Input-Output File zbi                         *
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
      *            * Chiave numero 01 : CODTMB                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-tmb        pic  x(05)                  .
                   15  fil-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-des-key        pic  x(30)                  .
                   15  fil-cod-tmb-2      pic  x(05)                  .
                   15  fil-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-dti-gen.
                   15  fil-des-tmb        pic  x(30)                  .
                   15  fil-des-stp        pic  x(25)                  .
                   15  fil-pwd-tmb        pic  x(08)                  .
                   15  fil-int-ftr        pic  9(02)                  .
                   15  fil-tmo-ftr        pic  x(05)                  .
                   15  fil-snx-acm        pic  x(01)                  .
                   15  fil-def-tac        pic  9(02)                  .
                   15  fil-def-ctr        pic  x(03)                  .
                   15  fil-cau-mag        pic  9(05)       comp-3     .
                   15  fil-cod-mic        pic  x(03)                  .
                   15  fil-cam-agg        pic  9(05)       comp-3     .
                   15  fil-def-tar        pic  x(01)                  .
                   15  fil-snv-tar        pic  x(01)                  .
                   15  fil-lst-tar        pic  x(04)                  .
                   15  fil-org-doc        pic  9(02)                  .
                   15  fil-prv-doc        pic  9(02)                  .
                   15  fil-sgl-num        pic  x(03)                  .
                   15  fil-mov-afd        pic  9(02)                  .
                   15  fil-def-tmf        pic  x(05)                  .
                   15  fil-snx-prz        pic  9(02)                  .
                   15  fil-snx-sco        pic  9(02)                  .
                   15  fil-snx-imp        pic  9(02)                  .
                   15  fil-snx-civ        pic  9(02)                  .
                   15  fil-snx-ttd        pic  9(02)                  .
                   15  fil-snx-dct        pic  9(02)                  .
                   15  fil-des-dct.
                       20  fil-rig-dct    occurs 04
                                          pic  x(60)                  .
                   15  fil-tip-sql        pic  9(02)                  .
                   15  fil-pos-sql        pic  9(03)       comp-3     .
                   15  fil-snx-par        pic  x(01)                  .
                   15  fil-vld-dpz        pic  x(01)                  .
                   15  fil-snx-age        pic  x(01)                  .
                   15  fil-snx-ndp        pic  x(01)                  .
                   15  fil-snx-fop        pic  x(01)                  .
                   15  fil-snx-lib        pic  x(01)                  .
                   15  fil-def-tpr        pic  x(05)                  .
                   15  fil-snx-nmm        pic  x(01)                  .
                   15  fil-tmo-ft2        pic  x(05)                  .
                   15  fil-snx-ncv        pic  x(01)                  .
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)       comp-3     .
                   15  fil-sta-tuc        pic  x(05)                  .
                   15  fil-sta-tux        pic  9(02)                  .
                   15  fil-cod-lst        pic  x(03)                  .
                   15  fil-snx-nmc        pic  x(01)                  .
                   15  fil-alx-gen.
                       20  filler  occurs 36
                                          pic  x(01)                  .
               10  fil-dti-dpz.
                   15  fil-cod-dsl        pic  x(07)                  .
                   15  fil-alx-dpz.
                       20  filler  occurs 40
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
      *            * Chiave numero 01 : CODTMB                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-tmb        pic  x(05)                  .
                   15  pul-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-des-key        pic  x(30)                  .
                   15  pul-cod-tmb-2      pic  x(05)                  .
                   15  pul-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-dti-gen.
                   15  pul-des-tmb        pic  x(30)                  .
                   15  pul-des-stp        pic  x(25)                  .
                   15  pul-pwd-tmb        pic  x(08)                  .
                   15  pul-int-ftr        pic  9(02)                  .
                   15  pul-tmo-ftr        pic  x(05)                  .
                   15  pul-snx-acm        pic  x(01)                  .
                   15  pul-def-tac        pic  9(02)                  .
                   15  pul-def-ctr        pic  x(03)                  .
                   15  pul-cau-mag        pic  9(05)       comp-3     .
                   15  pul-cod-mic        pic  x(03)                  .
                   15  pul-cam-agg        pic  9(05)       comp-3     .
                   15  pul-def-tar        pic  x(01)                  .
                   15  pul-snv-tar        pic  x(01)                  .
                   15  pul-lst-tar        pic  x(04)                  .
                   15  pul-org-doc        pic  9(02)                  .
                   15  pul-prv-doc        pic  9(02)                  .
                   15  pul-sgl-num        pic  x(03)                  .
                   15  pul-mov-afd        pic  9(02)                  .
                   15  pul-def-tmf        pic  x(05)                  .
                   15  pul-snx-prz        pic  9(02)                  .
                   15  pul-snx-sco        pic  9(02)                  .
                   15  pul-snx-imp        pic  9(02)                  .
                   15  pul-snx-civ        pic  9(02)                  .
                   15  pul-snx-ttd        pic  9(02)                  .
                   15  pul-snx-dct        pic  9(02)                  .
                   15  pul-des-dct.
                       20  pul-rig-dct    occurs 04
                                          pic  x(60)                  .
                   15  pul-tip-sql        pic  9(02)                  .
                   15  pul-pos-sql        pic  9(03)       comp-3     .
                   15  pul-snx-par        pic  x(01)                  .
                   15  pul-vld-dpz        pic  x(01)                  .
                   15  pul-snx-age        pic  x(01)                  .
                   15  pul-snx-ndp        pic  x(01)                  .
                   15  pul-snx-fop        pic  x(01)                  .
                   15  pul-snx-lib        pic  x(01)                  .
                   15  pul-def-tpr        pic  x(05)                  .
                   15  pul-snx-nmm        pic  x(01)                  .
                   15  pul-tmo-ft2        pic  x(05)                  .
                   15  pul-snx-ncv        pic  x(01)                  .
                   15  pul-sta-tus        pic  9(02)                  .
                   15  pul-sta-tud        pic  9(07)       comp-3     .
                   15  pul-sta-tuc        pic  x(05)                  .
                   15  pul-sta-tux        pic  9(02)                  .
                   15  pul-cod-lst        pic  x(03)                  .
                   15  pul-snx-nmc        pic  x(01)                  .
                   15  pul-alx-gen.
                       20  filler  occurs 36
                                          pic  x(01)                  .
               10  pul-dti-dpz.
                   15  pul-cod-dsl        pic  x(07)                  .
                   15  pul-alx-dpz.
                       20  puller  occurs 40
                                          pic  x(01)                  .

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
                     "zbi "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bol/fls/ioc/obj/iofzbi              "       .

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
                            "CODTMB    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DESKEY    "                              .
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
      *    * Record logico file [zbi]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .

      ******************************************************************
       Procedure Division                using f rf-zbi               .
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
           move      spaces               to   rf-zbi                 .
           move      spaces               to   rf-zbi-cod-tmb         .
           move      zero                 to   rf-zbi-cod-dpz         .
           move      spaces               to   rf-zbi-des-key         .
           move      spaces               to   rf-zbi-des-tmb         .
           move      spaces               to   rf-zbi-des-stp         .
           move      spaces               to   rf-zbi-pwd-tmb         .
           move      zero                 to   rf-zbi-int-ftr         .
           move      spaces               to   rf-zbi-tmo-ftr         .
           move      spaces               to   rf-zbi-snx-acm         .
           move      zero                 to   rf-zbi-def-tac         .
           move      spaces               to   rf-zbi-def-ctr         .
           move      zero                 to   rf-zbi-cau-mag         .
           move      spaces               to   rf-zbi-cod-mic         .
           move      zero                 to   rf-zbi-cam-agg         .
           move      spaces               to   rf-zbi-def-tar         .
           move      spaces               to   rf-zbi-snv-tar         .
           move      spaces               to   rf-zbi-lst-tar         .
           move      zero                 to   rf-zbi-org-doc         .
           move      zero                 to   rf-zbi-prv-doc         .
           move      spaces               to   rf-zbi-sgl-num         .
           move      zero                 to   rf-zbi-mov-afd         .
           move      spaces               to   rf-zbi-def-tmf         .
           move      zero                 to   rf-zbi-snx-prz         .
           move      zero                 to   rf-zbi-snx-sco         .
           move      zero                 to   rf-zbi-snx-imp         .
           move      zero                 to   rf-zbi-snx-civ         .
           move      zero                 to   rf-zbi-snx-ttd         .
           move      zero                 to   rf-zbi-snx-dct         .
           move      spaces               to   rf-zbi-des-dct         .
           move      zero                 to   rf-zbi-tip-sql         .
           move      zero                 to   rf-zbi-pos-sql         .
           move      spaces               to   rf-zbi-snx-par         .
           move      spaces               to   rf-zbi-vld-dpz         .
           move      spaces               to   rf-zbi-snx-age         .
           move      spaces               to   rf-zbi-snx-ndp         .
           move      spaces               to   rf-zbi-snx-fop         .
           move      spaces               to   rf-zbi-snx-lib         .
           move      spaces               to   rf-zbi-def-tpr         .
           move      spaces               to   rf-zbi-snx-nmm         .
           move      spaces               to   rf-zbi-tmo-ft2         .
           move      spaces               to   rf-zbi-snx-ncv         .
           move      zero                 to   rf-zbi-sta-tus         .
           move      zero                 to   rf-zbi-sta-tud         .
           move      spaces               to   rf-zbi-sta-tuc         .
           move      zero                 to   rf-zbi-sta-tux         .
           move      spaces               to   rf-zbi-cod-lst         .
           move      spaces               to   rf-zbi-snx-nmc         .
           move      spaces               to   rf-zbi-alx-gen         .
           move      spaces               to   rf-zbi-cod-dsl         .
           move      spaces               to   rf-zbi-alx-dpz         .
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
           move      rf-zbi-des-tmb       to   fil-des-tmb            .
           move      rf-zbi-des-stp       to   fil-des-stp            .
           move      rf-zbi-pwd-tmb       to   fil-pwd-tmb            .
           move      rf-zbi-int-ftr       to   fil-int-ftr            .
           move      rf-zbi-tmo-ftr       to   fil-tmo-ftr            .
           move      rf-zbi-snx-acm       to   fil-snx-acm            .
           move      rf-zbi-def-tac       to   fil-def-tac            .
           move      rf-zbi-def-ctr       to   fil-def-ctr            .
           move      rf-zbi-cau-mag       to   fil-cau-mag            .
           move      rf-zbi-cod-mic       to   fil-cod-mic            .
           move      rf-zbi-cam-agg       to   fil-cam-agg            .
           move      rf-zbi-def-tar       to   fil-def-tar            .
           move      rf-zbi-snv-tar       to   fil-snv-tar            .
           move      rf-zbi-lst-tar       to   fil-lst-tar            .
           move      rf-zbi-org-doc       to   fil-org-doc            .
           move      rf-zbi-prv-doc       to   fil-prv-doc            .
           move      rf-zbi-sgl-num       to   fil-sgl-num            .
           move      rf-zbi-mov-afd       to   fil-mov-afd            .
           move      rf-zbi-def-tmf       to   fil-def-tmf            .
           move      rf-zbi-snx-prz       to   fil-snx-prz            .
           move      rf-zbi-snx-sco       to   fil-snx-sco            .
           move      rf-zbi-snx-imp       to   fil-snx-imp            .
           move      rf-zbi-snx-civ       to   fil-snx-civ            .
           move      rf-zbi-snx-ttd       to   fil-snx-ttd            .
           move      rf-zbi-snx-dct       to   fil-snx-dct            .
           move      rf-zbi-des-dct       to   fil-des-dct            .
           move      rf-zbi-tip-sql       to   fil-tip-sql            .
           move      rf-zbi-pos-sql       to   fil-pos-sql            .
           move      rf-zbi-snx-par       to   fil-snx-par            .
           move      rf-zbi-vld-dpz       to   fil-vld-dpz            .
           move      rf-zbi-snx-age       to   fil-snx-age            .
           move      rf-zbi-snx-ndp       to   fil-snx-ndp            .
           move      rf-zbi-snx-fop       to   fil-snx-fop            .
           move      rf-zbi-snx-lib       to   fil-snx-lib            .
           move      rf-zbi-def-tpr       to   fil-def-tpr            .
           move      rf-zbi-snx-nmm       to   fil-snx-nmm            .
           move      rf-zbi-tmo-ft2       to   fil-tmo-ft2            .
           move      rf-zbi-snx-ncv       to   fil-snx-ncv            .
           move      rf-zbi-sta-tus       to   fil-sta-tus            .
           move      rf-zbi-sta-tud       to   fil-sta-tud            .
           move      rf-zbi-sta-tuc       to   fil-sta-tuc            .
           move      rf-zbi-sta-tux       to   fil-sta-tux            .
           move      rf-zbi-cod-lst       to   fil-cod-lst            .
           move      rf-zbi-snx-nmc       to   fil-snx-nmc            .
           move      rf-zbi-alx-gen       to   fil-alx-gen            .
           move      rf-zbi-cod-dsl       to   fil-cod-dsl            .
           move      rf-zbi-alx-dpz       to   fil-alx-dpz            .
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
           move      rf-zbi-cod-tmb       to   fil-cod-tmb            .
           move      rf-zbi-cod-dpz       to   fil-cod-dpz            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-zbi-des-key       to   fil-des-key            .
           move      rf-zbi-cod-tmb       to   fil-cod-tmb-2          .
           move      rf-zbi-cod-dpz       to   fil-cod-dpz-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-zbi                 .
           move      fil-cod-tmb          to   rf-zbi-cod-tmb         .
           move      fil-cod-dpz          to   rf-zbi-cod-dpz         .
           move      fil-des-key          to   rf-zbi-des-key         .
           move      fil-des-tmb          to   rf-zbi-des-tmb         .
           move      fil-des-stp          to   rf-zbi-des-stp         .
           move      fil-pwd-tmb          to   rf-zbi-pwd-tmb         .
           move      fil-int-ftr          to   rf-zbi-int-ftr         .
           move      fil-tmo-ftr          to   rf-zbi-tmo-ftr         .
           move      fil-snx-acm          to   rf-zbi-snx-acm         .
           move      fil-def-tac          to   rf-zbi-def-tac         .
           move      fil-def-ctr          to   rf-zbi-def-ctr         .
           move      fil-cau-mag          to   rf-zbi-cau-mag         .
           move      fil-cod-mic          to   rf-zbi-cod-mic         .
           move      fil-cam-agg          to   rf-zbi-cam-agg         .
           move      fil-def-tar          to   rf-zbi-def-tar         .
           move      fil-snv-tar          to   rf-zbi-snv-tar         .
           move      fil-lst-tar          to   rf-zbi-lst-tar         .
           move      fil-org-doc          to   rf-zbi-org-doc         .
           move      fil-prv-doc          to   rf-zbi-prv-doc         .
           move      fil-sgl-num          to   rf-zbi-sgl-num         .
           move      fil-mov-afd          to   rf-zbi-mov-afd         .
           move      fil-def-tmf          to   rf-zbi-def-tmf         .
           move      fil-snx-prz          to   rf-zbi-snx-prz         .
           move      fil-snx-sco          to   rf-zbi-snx-sco         .
           move      fil-snx-imp          to   rf-zbi-snx-imp         .
           move      fil-snx-civ          to   rf-zbi-snx-civ         .
           move      fil-snx-ttd          to   rf-zbi-snx-ttd         .
           move      fil-snx-dct          to   rf-zbi-snx-dct         .
           move      fil-des-dct          to   rf-zbi-des-dct         .
           move      fil-tip-sql          to   rf-zbi-tip-sql         .
           move      fil-pos-sql          to   rf-zbi-pos-sql         .
           move      fil-snx-par          to   rf-zbi-snx-par         .
           move      fil-vld-dpz          to   rf-zbi-vld-dpz         .
           move      fil-snx-age          to   rf-zbi-snx-age         .
           move      fil-snx-ndp          to   rf-zbi-snx-ndp         .
           move      fil-snx-fop          to   rf-zbi-snx-fop         .
           move      fil-snx-lib          to   rf-zbi-snx-lib         .
           move      fil-def-tpr          to   rf-zbi-def-tpr         .
           move      fil-snx-nmm          to   rf-zbi-snx-nmm         .
           move      fil-tmo-ft2          to   rf-zbi-tmo-ft2         .
           move      fil-snx-ncv          to   rf-zbi-snx-ncv         .
           move      fil-sta-tus          to   rf-zbi-sta-tus         .
           move      fil-sta-tud          to   rf-zbi-sta-tud         .
           move      fil-sta-tuc          to   rf-zbi-sta-tuc         .
           move      fil-sta-tux          to   rf-zbi-sta-tux         .
           move      fil-cod-lst          to   rf-zbi-cod-lst         .
           move      fil-snx-nmc          to   rf-zbi-snx-nmc         .
           move      fil-alx-gen          to   rf-zbi-alx-gen         .
           move      fil-cod-dsl          to   rf-zbi-cod-dsl         .
           move      fil-alx-dpz          to   rf-zbi-alx-dpz         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-zbi               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-zbi
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

