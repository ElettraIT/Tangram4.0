       Identification Division.
       Program-Id.                                 iofmmr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File mmr                         *
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
      *            * Chiave numero 01 : DATREG                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-dat-reg        pic  9(07)       comp-3     .
                   15  fil-num-prt        pic  9(11)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATMAG                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-dat-reg-2      pic  9(07)       comp-3     .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DPZMAG                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-mag-3      pic  9(07)       comp-3     .
                   15  fil-dat-reg-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-tip-mag-4      pic  9(02)                  .
                   15  fil-num-mag-4      pic  9(07)       comp-3     .
                   15  fil-dat-reg-4      pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-cod-cau            pic  9(05)       comp-3     .
               10  fil-trt-val            pic  x(01)                  .
               10  fil-tip-mdm            pic  9(02)                  .
               10  fil-trt-mic            pic  9(02)                  .
               10  fil-tip-mic            pic  x(01)                  .
               10  fil-cod-ctm            pic  x(03)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-doc            pic  x(10)                  .
               10  fil-cod-dsl            pic  x(07)                  .
               10  fil-cod-dsd            pic  x(07)                  .
               10  fil-tip-arc            pic  x(01)                  .
               10  fil-cod-arc            pic  9(07)       comp-3     .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-alf-mag            pic  x(14)                  .
               10  fil-var-mag            pic  x(14)                  .
               10  fil-udm-ana            pic  x(03)                  .
               10  fil-udm-mov            pic  x(03)                  .
               10  fil-num-pmg            pic  9(11)                  .
               10  fil-ddo-rif            pic  9(07)                  .
               10  fil-ndo-rif            pic  x(10)                  .
               10  fil-qta-mov            pic s9(08)v9(03) comp-3     .
               10  fil-cun-mov            pic  9(11)       comp-3     .
               10  fil-val-mov            pic s9(11)       comp-3     .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 30  pic  x(01)                  .

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
      *            * Chiave numero 01 : DATREG                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-dat-reg        pic  9(07)       comp-3     .
                   15  pul-num-prt        pic  9(11)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATMAG                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-dat-reg-2      pic  9(07)       comp-3     .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DPZMAG                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-mag-3      pic  9(07)       comp-3     .
                   15  pul-dat-reg-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-tip-mag-4      pic  9(02)                  .
                   15  pul-num-mag-4      pic  9(07)       comp-3     .
                   15  pul-dat-reg-4      pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-cod-cau            pic  9(05)       comp-3     .
               10  pul-trt-val            pic  x(01)                  .
               10  pul-tip-mdm            pic  9(02)                  .
               10  pul-trt-mic            pic  9(02)                  .
               10  pul-tip-mic            pic  x(01)                  .
               10  pul-cod-ctm            pic  x(03)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-doc            pic  x(10)                  .
               10  pul-cod-dsl            pic  x(07)                  .
               10  pul-cod-dsd            pic  x(07)                  .
               10  pul-tip-arc            pic  x(01)                  .
               10  pul-cod-arc            pic  9(07)       comp-3     .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-alf-mag            pic  x(14)                  .
               10  pul-var-mag            pic  x(14)                  .
               10  pul-udm-ana            pic  x(03)                  .
               10  pul-udm-mov            pic  x(03)                  .
               10  pul-num-pmg            pic  9(11)                  .
               10  pul-ddo-rif            pic  9(07)                  .
               10  pul-ndo-rif            pic  x(10)                  .
               10  pul-qta-mov            pic s9(08)v9(03) comp-3     .
               10  pul-cun-mov            pic  9(11)       comp-3     .
               10  pul-val-mov            pic s9(11)       comp-3     .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 30  pic  x(01)                  .

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
                     "mmr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/mag/fls/ioc/obj/iofmmr              "       .

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
                            "DATREG"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATMAG"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZMAG"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "MAGDAT"                                  .
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
      *    * Record logico file [mmr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .

      ******************************************************************
       Procedure Division                using f rf-mmr               .
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
           move      spaces               to   rf-mmr                 .
           move      zero                 to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      zero                 to   rf-mmr-cod-dpz         .
           move      zero                 to   rf-mmr-cod-cau         .
           move      spaces               to   rf-mmr-trt-val         .
           move      zero                 to   rf-mmr-tip-mdm         .
           move      zero                 to   rf-mmr-trt-mic         .
           move      spaces               to   rf-mmr-tip-mic         .
           move      spaces               to   rf-mmr-cod-ctm         .
           move      zero                 to   rf-mmr-dat-doc         .
           move      spaces               to   rf-mmr-num-doc         .
           move      spaces               to   rf-mmr-cod-dsl         .
           move      spaces               to   rf-mmr-cod-dsd         .
           move      spaces               to   rf-mmr-tip-arc         .
           move      zero                 to   rf-mmr-cod-arc         .
           move      spaces               to   rf-mmr-dpz-arc         .
           move      zero                 to   rf-mmr-tip-mag         .
           move      zero                 to   rf-mmr-num-mag         .
           move      spaces               to   rf-mmr-alf-mag         .
           move      spaces               to   rf-mmr-var-mag         .
           move      spaces               to   rf-mmr-udm-ana         .
           move      spaces               to   rf-mmr-udm-mov         .
           move      zero                 to   rf-mmr-num-pmg         .
           move      zero                 to   rf-mmr-ddo-rif         .
           move      spaces               to   rf-mmr-ndo-rif         .
           move      zero                 to   rf-mmr-qta-mov         .
           move      zero                 to   rf-mmr-cun-mov         .
           move      zero                 to   rf-mmr-val-mov         .
           move      spaces               to   rf-mmr-flg-ela         .
           move      spaces               to   rf-mmr-flg-pul         .
           move      spaces               to   rf-mmr-alx-exp         .
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
           move      rf-mmr-cod-cau       to   fil-cod-cau            .
           move      rf-mmr-trt-val       to   fil-trt-val            .
           move      rf-mmr-tip-mdm       to   fil-tip-mdm            .
           move      rf-mmr-trt-mic       to   fil-trt-mic            .
           move      rf-mmr-tip-mic       to   fil-tip-mic            .
           move      rf-mmr-cod-ctm       to   fil-cod-ctm            .
           move      rf-mmr-dat-doc       to   fil-dat-doc            .
           move      rf-mmr-num-doc       to   fil-num-doc            .
           move      rf-mmr-cod-dsl       to   fil-cod-dsl            .
           move      rf-mmr-cod-dsd       to   fil-cod-dsd            .
           move      rf-mmr-tip-arc       to   fil-tip-arc            .
           move      rf-mmr-cod-arc       to   fil-cod-arc            .
           move      rf-mmr-dpz-arc       to   fil-dpz-arc            .
           move      rf-mmr-alf-mag       to   fil-alf-mag            .
           move      rf-mmr-var-mag       to   fil-var-mag            .
           move      rf-mmr-udm-ana       to   fil-udm-ana            .
           move      rf-mmr-udm-mov       to   fil-udm-mov            .
           move      rf-mmr-num-pmg       to   fil-num-pmg            .
           move      rf-mmr-ddo-rif       to   fil-ddo-rif            .
           move      rf-mmr-ndo-rif       to   fil-ndo-rif            .
           move      rf-mmr-qta-mov       to   fil-qta-mov            .
           move      rf-mmr-cun-mov       to   fil-cun-mov            .
           move      rf-mmr-val-mov       to   fil-val-mov            .
           move      rf-mmr-flg-ela       to   fil-flg-ela            .
           move      rf-mmr-flg-pul       to   fil-flg-pul            .
           move      rf-mmr-alx-exp       to   fil-alx-exp            .
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
           move      rf-mmr-dat-reg       to   fil-dat-reg            .
           move      rf-mmr-num-prt       to   fil-num-prt            .
           move      rf-mmr-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-mmr-dat-reg       to   fil-dat-reg-2          .
           move      rf-mmr-tip-mag       to   fil-tip-mag            .
           move      rf-mmr-num-mag       to   fil-num-mag            .
           move      rf-mmr-num-prt       to   fil-num-prt-2          .
           move      rf-mmr-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-mmr-cod-dpz       to   fil-cod-dpz            .
           move      rf-mmr-tip-mag       to   fil-tip-mag-3          .
           move      rf-mmr-num-mag       to   fil-num-mag-3          .
           move      rf-mmr-dat-reg       to   fil-dat-reg-3          .
           move      rf-mmr-num-prt       to   fil-num-prt-3          .
           move      rf-mmr-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-mmr-tip-mag       to   fil-tip-mag-4          .
           move      rf-mmr-num-mag       to   fil-num-mag-4          .
           move      rf-mmr-dat-reg       to   fil-dat-reg-4          .
           move      rf-mmr-num-prt       to   fil-num-prt-4          .
           move      rf-mmr-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-mmr                 .
           move      fil-dat-reg          to   rf-mmr-dat-reg         .
           move      fil-num-prt          to   rf-mmr-num-prt         .
           move      fil-num-prg          to   rf-mmr-num-prg         .
           move      fil-cod-dpz          to   rf-mmr-cod-dpz         .
           move      fil-cod-cau          to   rf-mmr-cod-cau         .
           move      fil-trt-val          to   rf-mmr-trt-val         .
           move      fil-tip-mdm          to   rf-mmr-tip-mdm         .
           move      fil-trt-mic          to   rf-mmr-trt-mic         .
           move      fil-tip-mic          to   rf-mmr-tip-mic         .
           move      fil-cod-ctm          to   rf-mmr-cod-ctm         .
           move      fil-dat-doc          to   rf-mmr-dat-doc         .
           move      fil-num-doc          to   rf-mmr-num-doc         .
           move      fil-cod-dsl          to   rf-mmr-cod-dsl         .
           move      fil-cod-dsd          to   rf-mmr-cod-dsd         .
           move      fil-tip-arc          to   rf-mmr-tip-arc         .
           move      fil-cod-arc          to   rf-mmr-cod-arc         .
           move      fil-dpz-arc          to   rf-mmr-dpz-arc         .
           move      fil-tip-mag          to   rf-mmr-tip-mag         .
           move      fil-num-mag          to   rf-mmr-num-mag         .
           move      fil-alf-mag          to   rf-mmr-alf-mag         .
           move      fil-var-mag          to   rf-mmr-var-mag         .
           move      fil-udm-ana          to   rf-mmr-udm-ana         .
           move      fil-udm-mov          to   rf-mmr-udm-mov         .
           move      fil-num-pmg          to   rf-mmr-num-pmg         .
           move      fil-ddo-rif          to   rf-mmr-ddo-rif         .
           move      fil-ndo-rif          to   rf-mmr-ndo-rif         .
           move      fil-qta-mov          to   rf-mmr-qta-mov         .
           move      fil-cun-mov          to   rf-mmr-cun-mov         .
           move      fil-val-mov          to   rf-mmr-val-mov         .
           move      fil-flg-ela          to   rf-mmr-flg-ela         .
           move      fil-flg-pul          to   rf-mmr-flg-pul         .
           move      fil-alx-exp          to   rf-mmr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-mmr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-mmr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
