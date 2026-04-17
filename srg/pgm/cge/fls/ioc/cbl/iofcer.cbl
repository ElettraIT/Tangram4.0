       Identification Division.
       Program-Id.                                 iofcer             .
      *================================================================*
      *                                                                *
      *                  Input-Output File cer                         *
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
      *            * Chiave numero 01 : CODCSP                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-csp        pic  9(07)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATCSP                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-dat-mov        pic  9(07)       comp-3     .
                   15  fil-cod-csp-2      pic  9(07)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : ARCCSP                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-cod-csp-3      pic  9(07)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DOCCSP                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-doc        pic  x(10)                  .
                   15  fil-cod-csp-4      pic  9(07)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-cod-cau            pic  9(05)       comp-3     .
               10  fil-des-cau.
                   15  fil-cau-rig occurs 10
                                          pic  x(40)                  .
               10  fil-prt-doc            pic  9(11)       comp-3     .
               10  fil-imp-doc            pic s9(13)       comp-3     .
               10  fil-dtr-cge            pic  9(07)       comp-3     .
               10  fil-prt-cge            pic  9(07)       comp-3     .
               10  fil-sgn-mov            pic  x(01)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-imp-mov            pic s9(13)       comp-3     .
               10  fil-imp-au1            pic s9(13)       comp-3     .
               10  fil-imp-au2            pic s9(13)       comp-3     .
               10  fil-per-amr            pic  9(03)v9(02) comp-3     .
               10  fil-per-au1            pic  9(03)v9(02) comp-3     .
               10  fil-per-au2            pic  9(03)v9(02) comp-3     .
               10  fil-mtr-acc            pic  x(40)                  .
               10  fil-not-mov.
                   15  fil-rig-not occurs 10
                                          pic  x(40)                  .
               10  fil-flg-trt.
                   15  fil-flg-snx
                              occurs 10   pic  x(01)                  .
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
      *            * Chiave numero 01 : CODCSP                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-csp        pic  9(07)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATCSP                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-dat-mov        pic  9(07)       comp-3     .
                   15  pul-cod-csp-2      pic  9(07)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : ARCCSP                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-cod-csp-3      pic  9(07)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DOCCSP                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-doc        pic  x(10)                  .
                   15  pul-cod-csp-4      pic  9(07)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-cod-cau            pic  9(05)       comp-3     .
               10  pul-des-cau.
                   15  pul-cau-rig occurs 10
                                          pic  x(40)                  .
               10  pul-prt-doc            pic  9(11)       comp-3     .
               10  pul-imp-doc            pic s9(13)       comp-3     .
               10  pul-dtr-cge            pic  9(07)       comp-3     .
               10  pul-prt-cge            pic  9(07)       comp-3     .
               10  pul-sgn-mov            pic  x(01)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-imp-mov            pic s9(13)       comp-3     .
               10  pul-imp-au1            pic s9(13)       comp-3     .
               10  pul-imp-au2            pic s9(13)       comp-3     .
               10  pul-per-amr            pic  9(03)v9(02) comp-3     .
               10  pul-per-au1            pic  9(03)v9(02) comp-3     .
               10  pul-per-au2            pic  9(03)v9(02) comp-3     .
               10  pul-mtr-acc            pic  x(40)                  .
               10  pul-not-mov.
                   15  pul-rig-not occurs 10
                                          pic  x(40)                  .
               10  pul-flg-trt.
                   15  pul-flg-snx
                              occurs 10   pic  x(01)                  .
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
                     "cer "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/cge/fls/ioc/obj/iofcer              "       .

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
                            "CODCSP"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATCSP"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCCSP"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DOCCSP"                                  .
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
      *    * Record logico file [cer]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcer"                          .

      ******************************************************************
       Procedure Division                using f rf-cer               .
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
           move      spaces               to   rf-cer                 .
           move      zero                 to   rf-cer-cod-csp         .
           move      zero                 to   rf-cer-num-prg         .
           move      zero                 to   rf-cer-cod-cau         .
           move      spaces               to   rf-cer-des-cau         .
           move      zero                 to   rf-cer-dat-doc         .
           move      spaces               to   rf-cer-num-doc         .
           move      zero                 to   rf-cer-prt-doc         .
           move      zero                 to   rf-cer-imp-doc         .
           move      zero                 to   rf-cer-dtr-cge         .
           move      zero                 to   rf-cer-prt-cge         .
           move      spaces               to   rf-cer-sgn-mov         .
           move      spaces               to   rf-cer-tip-arc         .
           move      zero                 to   rf-cer-cod-arc         .
           move      spaces               to   rf-cer-dpz-arc         .
           move      zero                 to   rf-cer-dat-mov         .
           move      zero                 to   rf-cer-imp-mov         .
           move      zero                 to   rf-cer-imp-au1         .
           move      zero                 to   rf-cer-imp-au2         .
           move      zero                 to   rf-cer-per-amr         .
           move      zero                 to   rf-cer-per-au1         .
           move      zero                 to   rf-cer-per-au2         .
           move      spaces               to   rf-cer-mtr-acc         .
           move      spaces               to   rf-cer-not-mov         .
           move      spaces               to   rf-cer-flg-trt         .
           move      spaces               to   rf-cer-alx-exp         .
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
           move      rf-cer-cod-cau       to   fil-cod-cau            .
           move      rf-cer-des-cau       to   fil-des-cau            .
           move      rf-cer-prt-doc       to   fil-prt-doc            .
           move      rf-cer-imp-doc       to   fil-imp-doc            .
           move      rf-cer-dtr-cge       to   fil-dtr-cge            .
           move      rf-cer-prt-cge       to   fil-prt-cge            .
           move      rf-cer-sgn-mov       to   fil-sgn-mov            .
           move      rf-cer-dpz-arc       to   fil-dpz-arc            .
           move      rf-cer-imp-mov       to   fil-imp-mov            .
           move      rf-cer-imp-au1       to   fil-imp-au1            .
           move      rf-cer-imp-au2       to   fil-imp-au2            .
           move      rf-cer-per-amr       to   fil-per-amr            .
           move      rf-cer-per-au1       to   fil-per-au1            .
           move      rf-cer-per-au2       to   fil-per-au2            .
           move      rf-cer-mtr-acc       to   fil-mtr-acc            .
           move      rf-cer-not-mov       to   fil-not-mov            .
           move      rf-cer-flg-trt       to   fil-flg-trt            .
           move      rf-cer-alx-exp       to   fil-alx-exp            .
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
           move      rf-cer-cod-csp       to   fil-cod-csp            .
           move      rf-cer-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-cer-dat-mov       to   fil-dat-mov            .
           move      rf-cer-cod-csp       to   fil-cod-csp-2          .
           move      rf-cer-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-cer-tip-arc       to   fil-tip-arc            .
           move      rf-cer-cod-arc       to   fil-cod-arc            .
           move      rf-cer-cod-csp       to   fil-cod-csp-3          .
           move      rf-cer-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-cer-dat-doc       to   fil-dat-doc            .
           move      rf-cer-num-doc       to   fil-num-doc            .
           move      rf-cer-cod-csp       to   fil-cod-csp-4          .
           move      rf-cer-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-cer                 .
           move      fil-cod-csp          to   rf-cer-cod-csp         .
           move      fil-num-prg          to   rf-cer-num-prg         .
           move      fil-cod-cau          to   rf-cer-cod-cau         .
           move      fil-des-cau          to   rf-cer-des-cau         .
           move      fil-dat-doc          to   rf-cer-dat-doc         .
           move      fil-num-doc          to   rf-cer-num-doc         .
           move      fil-prt-doc          to   rf-cer-prt-doc         .
           move      fil-imp-doc          to   rf-cer-imp-doc         .
           move      fil-dtr-cge          to   rf-cer-dtr-cge         .
           move      fil-prt-cge          to   rf-cer-prt-cge         .
           move      fil-sgn-mov          to   rf-cer-sgn-mov         .
           move      fil-tip-arc          to   rf-cer-tip-arc         .
           move      fil-cod-arc          to   rf-cer-cod-arc         .
           move      fil-dpz-arc          to   rf-cer-dpz-arc         .
           move      fil-dat-mov          to   rf-cer-dat-mov         .
           move      fil-imp-mov          to   rf-cer-imp-mov         .
           move      fil-imp-au1          to   rf-cer-imp-au1         .
           move      fil-imp-au2          to   rf-cer-imp-au2         .
           move      fil-per-amr          to   rf-cer-per-amr         .
           move      fil-per-au1          to   rf-cer-per-au1         .
           move      fil-per-au2          to   rf-cer-per-au2         .
           move      fil-mtr-acc          to   rf-cer-mtr-acc         .
           move      fil-not-mov          to   rf-cer-not-mov         .
           move      fil-flg-trt          to   rf-cer-flg-trt         .
           move      fil-alx-exp          to   rf-cer-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-cer               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-cer
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

