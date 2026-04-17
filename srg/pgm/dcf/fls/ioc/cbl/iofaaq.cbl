       Identification Division.
       Program-Id.                                 iofaaq             .
      *================================================================*
      *                                                                *
      *                  Input-Output File aaq                         *
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
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CDPPDT                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cdp-pdt        pic  x(40)                  .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PDTCDP                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-pdt        pic  9(07)       comp-3     .
                   15  fil-cdp-pdt-4      pic  x(40)                  .
                   15  fil-tip-mag-4      pic  9(02)                  .
                   15  fil-num-pro-4      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-cod-iva            pic  9(05)       comp-3     .
               10  fil-ctp-acq            pic  9(07)       comp-3     .
               10  fil-dcf-pfz            pic  9(07)       comp-3     .
               10  fil-dpz-pfz            pic  x(04)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-dec-prz            pic  9(01)                  .
               10  fil-prz-acr            pic  9(09)       comp-3     .
               10  fil-uda-par            pic  9(07)       comp-3     .
               10  fil-tmp-apv            pic  9(03)       comp-3     .
               10  fil-epz-rgf            pic  9(01)                  .
               10  fil-snx-2qt            pic  9(01)                  .
               10  fil-dec-2qt            pic  9(01)                  .
               10  fil-snx-3qt            pic  9(01)                  .
               10  fil-dec-3qt            pic  9(01)                  .
               10  fil-snx-2pz            pic  9(01)                  .
               10  fil-dec-2pz            pic  9(01)                  .
               10  fil-aut-lst            pic  x(03)                  .
               10  fil-tip-vac            pic  x(03)                  .
               10  fil-cdp-aqt            pic  9(05)       comp-3     .
               10  fil-pdp-aqt  occurs 03
                                          pic  9(02)v9(01)            .
               10  fil-lot-acq            pic  9(10)v9(03) comp-3     .
               10  fil-cla-bdg            pic  9(05)       comp-3     .
               10  fil-alx-exp.
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
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-tip-mag-2      pic  9(02)                  .
                   15  pul-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CDPPDT                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cdp-pdt        pic  x(40)                  .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PDTCDP                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-pdt        pic  9(07)       comp-3     .
                   15  pul-cdp-pdt-4      pic  x(40)                  .
                   15  pul-tip-mag-4      pic  9(02)                  .
                   15  pul-num-pro-4      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-cod-iva            pic  9(05)       comp-3     .
               10  pul-ctp-acq            pic  9(07)       comp-3     .
               10  pul-dcf-pfz            pic  9(07)       comp-3     .
               10  pul-dpz-pfz            pic  x(04)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-dec-prz            pic  9(01)                  .
               10  pul-prz-acr            pic  9(09)       comp-3     .
               10  pul-uda-par            pic  9(07)       comp-3     .
               10  pul-tmp-apv            pic  9(03)       comp-3     .
               10  pul-epz-rgf            pic  9(01)                  .
               10  pul-snx-2qt            pic  9(01)                  .
               10  pul-dec-2qt            pic  9(01)                  .
               10  pul-snx-3qt            pic  9(01)                  .
               10  pul-dec-3qt            pic  9(01)                  .
               10  pul-snx-2pz            pic  9(01)                  .
               10  pul-dec-2pz            pic  9(01)                  .
               10  pul-aut-lst            pic  x(03)                  .
               10  pul-tip-vac            pic  x(03)                  .
               10  pul-cdp-aqt            pic  9(05)       comp-3     .
               10  pul-pdp-aqt  occurs 03
                                          pic  9(02)v9(01)            .
               10  pul-lot-acq            pic  9(10)v9(03) comp-3     .
               10  pul-cla-bdg            pic  9(05)       comp-3     .
               10  pul-alx-exp.
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
                     "aaq "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcf/fls/ioc/obj/iofaaq              "       .

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
                            "NUMPRO"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CDPPDT"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PDTCDP"                                  .
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
      *    * Record logico file [aaq]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .

      ******************************************************************
       Procedure Division                using f rf-aaq               .
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
           move      spaces               to   rf-aaq                 .
           move      zero                 to   rf-aaq-ide-dat         .
           move      spaces               to   rf-aaq-ide-ute         .
           move      spaces               to   rf-aaq-ide-fas         .
           move      zero                 to   rf-aaq-tip-mag         .
           move      zero                 to   rf-aaq-num-pro         .
           move      zero                 to   rf-aaq-cod-iva         .
           move      zero                 to   rf-aaq-ctp-acq         .
           move      zero                 to   rf-aaq-dcf-pfz         .
           move      spaces               to   rf-aaq-dpz-pfz         .
           move      spaces               to   rf-aaq-sgl-vlt         .
           move      zero                 to   rf-aaq-dec-vlt         .
           move      zero                 to   rf-aaq-dec-prz         .
           move      zero                 to   rf-aaq-prz-acr         .
           move      zero                 to   rf-aaq-uda-par         .
           move      zero                 to   rf-aaq-cod-pdt         .
           move      spaces               to   rf-aaq-cdp-pdt         .
           move      zero                 to   rf-aaq-tmp-apv         .
           move      zero                 to   rf-aaq-epz-rgf         .
           move      zero                 to   rf-aaq-snx-2qt         .
           move      zero                 to   rf-aaq-dec-2qt         .
           move      zero                 to   rf-aaq-snx-3qt         .
           move      zero                 to   rf-aaq-dec-3qt         .
           move      zero                 to   rf-aaq-snx-2pz         .
           move      zero                 to   rf-aaq-dec-2pz         .
           move      spaces               to   rf-aaq-aut-lst         .
           move      spaces               to   rf-aaq-tip-vac         .
           move      zero                 to   rf-aaq-cdp-aqt         .
           move      zero                 to   rf-aaq-pdp-aqt (1)     .
           move      zero                 to   rf-aaq-pdp-aqt (2)     .
           move      zero                 to   rf-aaq-pdp-aqt (3)     .
           move      zero                 to   rf-aaq-lot-acq         .
           move      zero                 to   rf-aaq-cla-bdg         .
           move      spaces               to   rf-aaq-alx-exp         .
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
           move      rf-aaq-ide-ute       to   fil-ide-ute            .
           move      rf-aaq-ide-fas       to   fil-ide-fas            .
           move      rf-aaq-cod-iva       to   fil-cod-iva            .
           move      rf-aaq-ctp-acq       to   fil-ctp-acq            .
           move      rf-aaq-dcf-pfz       to   fil-dcf-pfz            .
           move      rf-aaq-dpz-pfz       to   fil-dpz-pfz            .
           move      rf-aaq-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-aaq-dec-vlt       to   fil-dec-vlt            .
           move      rf-aaq-dec-prz       to   fil-dec-prz            .
           move      rf-aaq-prz-acr       to   fil-prz-acr            .
           move      rf-aaq-uda-par       to   fil-uda-par            .
           move      rf-aaq-tmp-apv       to   fil-tmp-apv            .
           move      rf-aaq-epz-rgf       to   fil-epz-rgf            .
           move      rf-aaq-snx-2qt       to   fil-snx-2qt            .
           move      rf-aaq-dec-2qt       to   fil-dec-2qt            .
           move      rf-aaq-snx-3qt       to   fil-snx-3qt            .
           move      rf-aaq-dec-3qt       to   fil-dec-3qt            .
           move      rf-aaq-snx-2pz       to   fil-snx-2pz            .
           move      rf-aaq-dec-2pz       to   fil-dec-2pz            .
           move      rf-aaq-aut-lst       to   fil-aut-lst            .
           move      rf-aaq-tip-vac       to   fil-tip-vac            .
           move      rf-aaq-cdp-aqt       to   fil-cdp-aqt            .
           move      rf-aaq-pdp-aqt (1)   to   fil-pdp-aqt (1)        .
           move      rf-aaq-pdp-aqt (2)   to   fil-pdp-aqt (2)        .
           move      rf-aaq-pdp-aqt (3)   to   fil-pdp-aqt (3)        .
           move      rf-aaq-lot-acq       to   fil-lot-acq            .
           move      rf-aaq-cla-bdg       to   fil-cla-bdg            .
           move      rf-aaq-alx-exp       to   fil-alx-exp            .
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
           move      rf-aaq-tip-mag       to   fil-tip-mag            .
           move      rf-aaq-num-pro       to   fil-num-pro            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-aaq-ide-dat       to   fil-ide-dat            .
           move      rf-aaq-tip-mag       to   fil-tip-mag-2          .
           move      rf-aaq-num-pro       to   fil-num-pro-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-aaq-cdp-pdt       to   fil-cdp-pdt            .
           move      rf-aaq-tip-mag       to   fil-tip-mag-3          .
           move      rf-aaq-num-pro       to   fil-num-pro-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-aaq-cod-pdt       to   fil-cod-pdt            .
           move      rf-aaq-cdp-pdt       to   fil-cdp-pdt-4          .
           move      rf-aaq-tip-mag       to   fil-tip-mag-4          .
           move      rf-aaq-num-pro       to   fil-num-pro-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-aaq                 .
           move      fil-ide-dat          to   rf-aaq-ide-dat         .
           move      fil-ide-ute          to   rf-aaq-ide-ute         .
           move      fil-ide-fas          to   rf-aaq-ide-fas         .
           move      fil-tip-mag          to   rf-aaq-tip-mag         .
           move      fil-num-pro          to   rf-aaq-num-pro         .
           move      fil-cod-iva          to   rf-aaq-cod-iva         .
           move      fil-ctp-acq          to   rf-aaq-ctp-acq         .
           move      fil-dcf-pfz          to   rf-aaq-dcf-pfz         .
           move      fil-dpz-pfz          to   rf-aaq-dpz-pfz         .
           move      fil-sgl-vlt          to   rf-aaq-sgl-vlt         .
           move      fil-dec-vlt          to   rf-aaq-dec-vlt         .
           move      fil-dec-prz          to   rf-aaq-dec-prz         .
           move      fil-prz-acr          to   rf-aaq-prz-acr         .
           move      fil-uda-par          to   rf-aaq-uda-par         .
           move      fil-cod-pdt          to   rf-aaq-cod-pdt         .
           move      fil-cdp-pdt          to   rf-aaq-cdp-pdt         .
           move      fil-tmp-apv          to   rf-aaq-tmp-apv         .
           move      fil-epz-rgf          to   rf-aaq-epz-rgf         .
           move      fil-snx-2qt          to   rf-aaq-snx-2qt         .
           move      fil-dec-2qt          to   rf-aaq-dec-2qt         .
           move      fil-snx-3qt          to   rf-aaq-snx-3qt         .
           move      fil-dec-3qt          to   rf-aaq-dec-3qt         .
           move      fil-snx-2pz          to   rf-aaq-snx-2pz         .
           move      fil-dec-2pz          to   rf-aaq-dec-2pz         .
           move      fil-aut-lst          to   rf-aaq-aut-lst         .
           move      fil-tip-vac          to   rf-aaq-tip-vac         .
           move      fil-cdp-aqt          to   rf-aaq-cdp-aqt         .
           move      fil-pdp-aqt (1)      to   rf-aaq-pdp-aqt (1)     .
           move      fil-pdp-aqt (2)      to   rf-aaq-pdp-aqt (2)     .
           move      fil-pdp-aqt (3)      to   rf-aaq-pdp-aqt (3)     .
           move      fil-lot-acq          to   rf-aaq-lot-acq         .
           move      fil-cla-bdg          to   rf-aaq-cla-bdg         .
           move      fil-alx-exp          to   rf-aaq-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-aaq               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-aaq
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

