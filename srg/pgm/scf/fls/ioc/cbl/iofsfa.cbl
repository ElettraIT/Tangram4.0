       Identification Division.
       Program-Id.                                 iofsfa             .
      *================================================================*
      *                                                                *
      *                  Input-Output File sfa                         *
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
      *            * Chiave numero 01 : NUMADP                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-adp        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-adp-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CBPADP                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cbp-adp        pic  x(10)                  .
                   15  fil-num-adp-3      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dtr-adp            pic  9(07)       comp-3     .
               10  fil-drc-adp            pic  9(07)       comp-3     .
               10  fil-npc-adp            pic  9(07)       comp-3     .
               10  fil-mod-adp            pic  9(02)                  .
               10  fil-tvl-adp            pic  9(02)                  .
               10  fil-tip-adp            pic  9(02)                  .
               10  fil-cas-adp            pic  x(10)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-tdc-vlt            pic  x(01)                  .
               10  fil-cdc-adp            pic  9(06)v9(05) comp-3     .
               10  fil-iiv-adp            pic s9(11)       comp-3     .
               10  fil-imp-adp            pic s9(11)       comp-3     .
               10  fil-ddo-adp            pic  9(07)       comp-3     .
               10  fil-ndo-adp            pic  x(10)                  .
               10  fil-spe-adp            pic s9(11)       comp-3     .
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
               10  fil-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMADP                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-adp        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-adp-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CBPADP                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cbp-adp        pic  x(10)                  .
                   15  pul-num-adp-3      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dtr-adp            pic  9(07)       comp-3     .
               10  pul-drc-adp            pic  9(07)       comp-3     .
               10  pul-npc-adp            pic  9(07)       comp-3     .
               10  pul-mod-adp            pic  9(02)                  .
               10  pul-tvl-adp            pic  9(02)                  .
               10  pul-tip-adp            pic  9(02)                  .
               10  pul-cas-adp            pic  x(10)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-tdc-vlt            pic  x(01)                  .
               10  pul-cdc-adp            pic  9(06)v9(05) comp-3     .
               10  pul-iiv-adp            pic s9(11)       comp-3     .
               10  pul-imp-adp            pic s9(11)       comp-3     .
               10  pul-ddo-adp            pic  9(07)       comp-3     .
               10  pul-ndo-adp            pic  x(10)                  .
               10  pul-spe-adp            pic s9(11)       comp-3     .
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
               10  pul-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

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
                     "sfa "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/scf/fls/ioc/obj/iofsfa              "       .

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
                            "NUMADP    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CBPADP    "                              .
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
      *    * Record logico file [sfa]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfa"                          .

      ******************************************************************
       Procedure Division                using f rf-sfa               .
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
           move      spaces               to   rf-sfa                 .
           move      zero                 to   rf-sfa-ide-dat         .
           move      spaces               to   rf-sfa-ide-ute         .
           move      spaces               to   rf-sfa-ide-fas         .
           move      zero                 to   rf-sfa-dtr-adp         .
           move      zero                 to   rf-sfa-drc-adp         .
           move      zero                 to   rf-sfa-npc-adp         .
           move      zero                 to   rf-sfa-num-adp         .
           move      zero                 to   rf-sfa-mod-adp         .
           move      zero                 to   rf-sfa-tvl-adp         .
           move      spaces               to   rf-sfa-cbp-adp         .
           move      zero                 to   rf-sfa-tip-adp         .
           move      spaces               to   rf-sfa-cas-adp         .
           move      spaces               to   rf-sfa-sgl-vlt         .
           move      zero                 to   rf-sfa-dec-vlt         .
           move      spaces               to   rf-sfa-tdc-vlt         .
           move      zero                 to   rf-sfa-cdc-adp         .
           move      zero                 to   rf-sfa-iiv-adp         .
           move      zero                 to   rf-sfa-imp-adp         .
           move      zero                 to   rf-sfa-ddo-adp         .
           move      spaces               to   rf-sfa-ndo-adp         .
           move      zero                 to   rf-sfa-spe-adp         .
           move      zero                 to   rf-sfa-fcs-uno         .
           move      zero                 to   rf-sfa-fcs-due         .
           move      zero                 to   rf-sfa-fcs-tre         .
           move      spaces               to   rf-sfa-flg-ela         .
           move      spaces               to   rf-sfa-flg-pul         .
           move      spaces               to   rf-sfa-alx-exp         .
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
           move      rf-sfa-ide-ute       to   fil-ide-ute            .
           move      rf-sfa-ide-fas       to   fil-ide-fas            .
           move      rf-sfa-dtr-adp       to   fil-dtr-adp            .
           move      rf-sfa-drc-adp       to   fil-drc-adp            .
           move      rf-sfa-npc-adp       to   fil-npc-adp            .
           move      rf-sfa-mod-adp       to   fil-mod-adp            .
           move      rf-sfa-tvl-adp       to   fil-tvl-adp            .
           move      rf-sfa-tip-adp       to   fil-tip-adp            .
           move      rf-sfa-cas-adp       to   fil-cas-adp            .
           move      rf-sfa-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-sfa-dec-vlt       to   fil-dec-vlt            .
           move      rf-sfa-tdc-vlt       to   fil-tdc-vlt            .
           move      rf-sfa-cdc-adp       to   fil-cdc-adp            .
           move      rf-sfa-iiv-adp       to   fil-iiv-adp            .
           move      rf-sfa-imp-adp       to   fil-imp-adp            .
           move      rf-sfa-ddo-adp       to   fil-ddo-adp            .
           move      rf-sfa-ndo-adp       to   fil-ndo-adp            .
           move      rf-sfa-spe-adp       to   fil-spe-adp            .
           move      rf-sfa-fcs-uno       to   fil-fcs-uno            .
           move      rf-sfa-fcs-due       to   fil-fcs-due            .
           move      rf-sfa-fcs-tre       to   fil-fcs-tre            .
           move      rf-sfa-flg-ela       to   fil-flg-ela            .
           move      rf-sfa-flg-pul       to   fil-flg-pul            .
           move      rf-sfa-alx-exp       to   fil-alx-exp            .
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
           move      rf-sfa-num-adp       to   fil-num-adp            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-sfa-ide-dat       to   fil-ide-dat            .
           move      rf-sfa-num-adp       to   fil-num-adp-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-sfa-cbp-adp       to   fil-cbp-adp            .
           move      rf-sfa-num-adp       to   fil-num-adp-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-sfa                 .
           move      fil-ide-dat          to   rf-sfa-ide-dat         .
           move      fil-ide-ute          to   rf-sfa-ide-ute         .
           move      fil-ide-fas          to   rf-sfa-ide-fas         .
           move      fil-dtr-adp          to   rf-sfa-dtr-adp         .
           move      fil-drc-adp          to   rf-sfa-drc-adp         .
           move      fil-npc-adp          to   rf-sfa-npc-adp         .
           move      fil-num-adp          to   rf-sfa-num-adp         .
           move      fil-mod-adp          to   rf-sfa-mod-adp         .
           move      fil-tvl-adp          to   rf-sfa-tvl-adp         .
           move      fil-cbp-adp          to   rf-sfa-cbp-adp         .
           move      fil-tip-adp          to   rf-sfa-tip-adp         .
           move      fil-cas-adp          to   rf-sfa-cas-adp         .
           move      fil-sgl-vlt          to   rf-sfa-sgl-vlt         .
           move      fil-dec-vlt          to   rf-sfa-dec-vlt         .
           move      fil-tdc-vlt          to   rf-sfa-tdc-vlt         .
           move      fil-cdc-adp          to   rf-sfa-cdc-adp         .
           move      fil-iiv-adp          to   rf-sfa-iiv-adp         .
           move      fil-imp-adp          to   rf-sfa-imp-adp         .
           move      fil-ddo-adp          to   rf-sfa-ddo-adp         .
           move      fil-ndo-adp          to   rf-sfa-ndo-adp         .
           move      fil-spe-adp          to   rf-sfa-spe-adp         .
           move      fil-fcs-uno          to   rf-sfa-fcs-uno         .
           move      fil-fcs-due          to   rf-sfa-fcs-due         .
           move      fil-fcs-tre          to   rf-sfa-fcs-tre         .
           move      fil-flg-ela          to   rf-sfa-flg-ela         .
           move      fil-flg-pul          to   rf-sfa-flg-pul         .
           move      fil-alx-exp          to   rf-sfa-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-sfa               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-sfa
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
