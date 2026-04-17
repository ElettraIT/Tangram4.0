       Identification Division.
       Program-Id.                                 iofddp             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ddp                         *
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
      *            * Chiave numero 01 : NUMDDP                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-ddp        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-ddp-2      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dtr-com            pic  9(07)       comp-3     .
               10  fil-tip-ddp            pic  9(02)                  .
               10  fil-tav-ric            pic  9(02)                  .
               10  fil-tsc-das            pic  9(04)                  .
               10  fil-dsc-min            pic  9(07)       comp-3     .
               10  fil-dsc-max            pic  9(07)       comp-3     .
               10  fil-dsc-vis            pic  x(01)                  .
               10  fil-cir-itb.
                   15  fil-cod-cib occurs 05
                                          pic  x(02)                  .
               10  fil-imp-max            pic s9(13)       comp-3     .
               10  fil-imp-ddp            pic s9(13)       comp-3     .
               10  fil-dtr-pre            pic  9(07)       comp-3     .
               10  fil-drc-pre            pic  9(07)       comp-3     .
               10  fil-npc-pre            pic  9(07)       comp-3     .
               10  fil-tip-pre            pic  9(02)                  .
               10  fil-cod-cbp            pic  x(10)                  .
               10  fil-snx-stp            pic  x(01)                  .
               10  fil-flc-stp            pic  9(02)                  .
               10  fil-snx-ads            pic  x(01)                  .
               10  fil-flc-ads            pic  9(02)                  .
               10  fil-tip-ias            pic  9(02)                  .
               10  fil-flc-ias            pic  9(02)                  .
               10  fil-dtr-act            pic  9(07)       comp-3     .
               10  fil-drc-act            pic  9(07)       comp-3     .
               10  fil-npc-act            pic  9(07)       comp-3     .
               10  fil-dcb-act            pic  9(07)       comp-3     .
               10  fil-ncb-act            pic  x(10)                  .
               10  fil-spe-act            pic s9(11)       comp-3     .
               10  fil-int-act            pic s9(11)       comp-3     .
               10  fil-dtr-acd            pic  9(07)       comp-3     .
               10  fil-drc-acd            pic  9(07)       comp-3     .
               10  fil-npc-acd            pic  9(07)       comp-3     .
               10  fil-dcb-acd            pic  9(07)       comp-3     .
               10  fil-ncb-acd            pic  x(10)                  .
               10  fil-spe-acd            pic s9(11)       comp-3     .
               10  fil-int-acd            pic s9(11)       comp-3     .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
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
      *            * Chiave numero 01 : NUMDDP                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-ddp        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-ddp-2      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dtr-com            pic  9(07)       comp-3     .
               10  pul-tip-ddp            pic  9(02)                  .
               10  pul-tav-ric            pic  9(02)                  .
               10  pul-tsc-das            pic  9(04)                  .
               10  pul-dsc-min            pic  9(07)       comp-3     .
               10  pul-dsc-max            pic  9(07)       comp-3     .
               10  pul-dsc-vis            pic  x(01)                  .
               10  pul-cir-itb.
                   15  pul-cod-cib occurs 05
                                          pic  x(02)                  .
               10  pul-imp-max            pic s9(13)       comp-3     .
               10  pul-imp-ddp            pic s9(13)       comp-3     .
               10  pul-dtr-pre            pic  9(07)       comp-3     .
               10  pul-drc-pre            pic  9(07)       comp-3     .
               10  pul-npc-pre            pic  9(07)       comp-3     .
               10  pul-tip-pre            pic  9(02)                  .
               10  pul-cod-cbp            pic  x(10)                  .
               10  pul-snx-stp            pic  x(01)                  .
               10  pul-flc-stp            pic  9(02)                  .
               10  pul-snx-ads            pic  x(01)                  .
               10  pul-flc-ads            pic  9(02)                  .
               10  pul-tip-ias            pic  9(02)                  .
               10  pul-flc-ias            pic  9(02)                  .
               10  pul-dtr-act            pic  9(07)       comp-3     .
               10  pul-drc-act            pic  9(07)       comp-3     .
               10  pul-npc-act            pic  9(07)       comp-3     .
               10  pul-dcb-act            pic  9(07)       comp-3     .
               10  pul-ncb-act            pic  x(10)                  .
               10  pul-spe-act            pic s9(11)       comp-3     .
               10  pul-int-act            pic s9(11)       comp-3     .
               10  pul-dtr-acd            pic  9(07)       comp-3     .
               10  pul-drc-acd            pic  9(07)       comp-3     .
               10  pul-npc-acd            pic  9(07)       comp-3     .
               10  pul-dcb-acd            pic  9(07)       comp-3     .
               10  pul-ncb-acd            pic  x(10)                  .
               10  pul-spe-acd            pic s9(11)       comp-3     .
               10  pul-int-acd            pic s9(11)       comp-3     .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-alx-exp.
                   15  puller occurs 20   pic  x(01)                  .

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
                     "ddp "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/gep/fls/ioc/obj/iofddp              "       .

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
           05  k-ctr                      pic  9(02) value 2          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMDDP    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
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
      *    * Record logico file [ddp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .

      ******************************************************************
       Procedure Division                using f rf-ddp               .
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
           move      e-not-fnd            to   f-sts                 .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-ddp                 .
           move      zero                 to   rf-ddp-ide-dat         .
           move      spaces               to   rf-ddp-ide-ute         .
           move      spaces               to   rf-ddp-ide-fas         .
           move      zero                 to   rf-ddp-dtr-com         .
           move      zero                 to   rf-ddp-num-ddp         .
           move      zero                 to   rf-ddp-tip-ddp         .
           move      zero                 to   rf-ddp-tav-ric         .
           move      zero                 to   rf-ddp-tsc-das         .
           move      zero                 to   rf-ddp-dsc-min         .
           move      zero                 to   rf-ddp-dsc-max         .
           move      spaces               to   rf-ddp-dsc-vis         .
           move      spaces               to   rf-ddp-cod-cib (01)    .         .
           move      spaces               to   rf-ddp-cod-cib (02)    .         .
           move      spaces               to   rf-ddp-cod-cib (03)    .         .
           move      spaces               to   rf-ddp-cod-cib (04)    .         .
           move      spaces               to   rf-ddp-cod-cib (05)    .         .
           move      zero                 to   rf-ddp-imp-max         .
           move      zero                 to   rf-ddp-imp-ddp         .
           move      zero                 to   rf-ddp-dtr-pre         .
           move      zero                 to   rf-ddp-drc-pre         .
           move      zero                 to   rf-ddp-npc-pre         .
           move      zero                 to   rf-ddp-tip-pre         .
           move      spaces               to   rf-ddp-cod-cbp         .
           move      spaces               to   rf-ddp-snx-stp         .
           move      zero                 to   rf-ddp-flc-stp         .
           move      spaces               to   rf-ddp-snx-ads         .
           move      zero                 to   rf-ddp-flc-ads         .
           move      zero                 to   rf-ddp-tip-ias         .
           move      zero                 to   rf-ddp-flc-ias         .
           move      zero                 to   rf-ddp-dtr-act         .
           move      zero                 to   rf-ddp-drc-act         .
           move      zero                 to   rf-ddp-npc-act         .
           move      zero                 to   rf-ddp-dcb-act         .
           move      zero                 to   rf-ddp-ncb-act         .
           move      zero                 to   rf-ddp-spe-act         .
           move      zero                 to   rf-ddp-int-act         .
           move      zero                 to   rf-ddp-dtr-acd         .
           move      zero                 to   rf-ddp-drc-acd         .
           move      zero                 to   rf-ddp-npc-acd         .
           move      zero                 to   rf-ddp-dcb-acd         .
           move      zero                 to   rf-ddp-ncb-acd         .
           move      zero                 to   rf-ddp-spe-acd         .
           move      zero                 to   rf-ddp-int-acd         .
           move      spaces               to   rf-ddp-flg-ela         .
           move      spaces               to   rf-ddp-flg-pul         .
           move      spaces               to   rf-ddp-alx-exp         .
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
           move      rf-ddp-ide-ute       to   fil-ide-ute            .
           move      rf-ddp-ide-fas       to   fil-ide-fas            .
           move      rf-ddp-dtr-com       to   fil-dtr-com            .
           move      rf-ddp-tip-ddp       to   fil-tip-ddp            .
           move      rf-ddp-tav-ric       to   fil-tav-ric            .
           move      rf-ddp-tsc-das       to   fil-tsc-das            .
           move      rf-ddp-dsc-min       to   fil-dsc-min            .
           move      rf-ddp-dsc-max       to   fil-dsc-max            .
           move      rf-ddp-dsc-vis       to   fil-dsc-vis            .
           move      rf-ddp-cod-cib (01)  to   fil-cod-cib (01)       .
           move      rf-ddp-cod-cib (02)  to   fil-cod-cib (02)       .
           move      rf-ddp-cod-cib (03)  to   fil-cod-cib (03)       .
           move      rf-ddp-cod-cib (04)  to   fil-cod-cib (04)       .
           move      rf-ddp-cod-cib (05)  to   fil-cod-cib (05)       .
           move      rf-ddp-imp-max       to   fil-imp-max            .
           move      rf-ddp-imp-ddp       to   fil-imp-ddp            .
           move      rf-ddp-dtr-pre       to   fil-dtr-pre            .
           move      rf-ddp-drc-pre       to   fil-drc-pre            .
           move      rf-ddp-npc-pre       to   fil-npc-pre            .
           move      rf-ddp-tip-pre       to   fil-tip-pre            .
           move      rf-ddp-cod-cbp       to   fil-cod-cbp            .
           move      rf-ddp-snx-stp       to   fil-snx-stp            .
           move      rf-ddp-flc-stp       to   fil-flc-stp            .
           move      rf-ddp-snx-ads       to   fil-snx-ads            .
           move      rf-ddp-flc-ads       to   fil-flc-ads            .
           move      rf-ddp-tip-ias       to   fil-tip-ias            .
           move      rf-ddp-flc-ias       to   fil-flc-ias            .
           move      rf-ddp-dtr-act       to   fil-dtr-act            .
           move      rf-ddp-drc-act       to   fil-drc-act            .
           move      rf-ddp-npc-act       to   fil-npc-act            .
           move      rf-ddp-dcb-act       to   fil-dcb-act            .
           move      rf-ddp-ncb-act       to   fil-ncb-act            .
           move      rf-ddp-spe-act       to   fil-spe-act            .
           move      rf-ddp-int-act       to   fil-int-act            .
           move      rf-ddp-dtr-acd       to   fil-dtr-acd            .
           move      rf-ddp-drc-acd       to   fil-drc-acd            .
           move      rf-ddp-npc-acd       to   fil-npc-acd            .
           move      rf-ddp-dcb-acd       to   fil-dcb-acd            .
           move      rf-ddp-ncb-acd       to   fil-ncb-acd            .
           move      rf-ddp-spe-acd       to   fil-spe-acd            .
           move      rf-ddp-int-acd       to   fil-int-acd            .
           move      rf-ddp-flg-ela       to   fil-flg-ela            .
           move      rf-ddp-flg-pul       to   fil-flg-pul            .
           move      rf-ddp-alx-exp       to   fil-alx-exp            .
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
           move      rf-ddp-num-ddp       to   fil-num-ddp            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-ddp-ide-dat       to   fil-ide-dat            .
           move      rf-ddp-num-ddp       to   fil-num-ddp-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ddp                 .
           move      fil-ide-dat          to   rf-ddp-ide-dat         .
           move      fil-ide-ute          to   rf-ddp-ide-ute         .
           move      fil-ide-fas          to   rf-ddp-ide-fas         .
           move      fil-dtr-com          to   rf-ddp-dtr-com         .
           move      fil-num-ddp          to   rf-ddp-num-ddp         .
           move      fil-tip-ddp          to   rf-ddp-tip-ddp         .
           move      fil-tav-ric          to   rf-ddp-tav-ric         .
           move      fil-tsc-das          to   rf-ddp-tsc-das         .
           move      fil-dsc-min          to   rf-ddp-dsc-min         .
           move      fil-dsc-max          to   rf-ddp-dsc-max         .
           move      fil-dsc-vis          to   rf-ddp-dsc-vis         .
           move      fil-cod-cib (01)     to   rf-ddp-cod-cib (01)    .
           move      fil-cod-cib (02)     to   rf-ddp-cod-cib (02)    .
           move      fil-cod-cib (03)     to   rf-ddp-cod-cib (03)    .
           move      fil-cod-cib (04)     to   rf-ddp-cod-cib (04)    .
           move      fil-cod-cib (05)     to   rf-ddp-cod-cib (05)    .
           move      fil-imp-max          to   rf-ddp-imp-max         .
           move      fil-imp-ddp          to   rf-ddp-imp-ddp         .
           move      fil-dtr-pre          to   rf-ddp-dtr-pre         .
           move      fil-drc-pre          to   rf-ddp-drc-pre         .
           move      fil-npc-pre          to   rf-ddp-npc-pre         .
           move      fil-tip-pre          to   rf-ddp-tip-pre         .
           move      fil-cod-cbp          to   rf-ddp-cod-cbp         .
           move      fil-snx-stp          to   rf-ddp-snx-stp         .
           move      fil-flc-stp          to   rf-ddp-flc-stp         .
           move      fil-snx-ads          to   rf-ddp-snx-ads         .
           move      fil-flc-ads          to   rf-ddp-flc-ads         .
           move      fil-tip-ias          to   rf-ddp-tip-ias         .
           move      fil-flc-ias          to   rf-ddp-flc-ias         .
           move      fil-dtr-act          to   rf-ddp-dtr-act         .
           move      fil-drc-act          to   rf-ddp-drc-act         .
           move      fil-npc-act          to   rf-ddp-npc-act         .
           move      fil-dcb-act          to   rf-ddp-dcb-act         .
           move      fil-ncb-act          to   rf-ddp-ncb-act         .
           move      fil-spe-act          to   rf-ddp-spe-act         .
           move      fil-int-act          to   rf-ddp-int-act         .
           move      fil-dtr-acd          to   rf-ddp-dtr-acd         .
           move      fil-drc-acd          to   rf-ddp-drc-acd         .
           move      fil-npc-acd          to   rf-ddp-npc-acd         .
           move      fil-dcb-acd          to   rf-ddp-dcb-acd         .
           move      fil-ncb-acd          to   rf-ddp-ncb-acd         .
           move      fil-spe-acd          to   rf-ddp-spe-acd         .
           move      fil-int-acd          to   rf-ddp-int-acd         .
           move      fil-flg-ela          to   rf-ddp-flg-ela         .
           move      fil-flg-pul          to   rf-ddp-flg-pul         .
           move      fil-alx-exp          to   rf-ddp-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ddp               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ddp
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

