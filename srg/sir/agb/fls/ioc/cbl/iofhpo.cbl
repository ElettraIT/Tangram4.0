       Identification Division.
       Program-Id.                                 iofhpo             .
      *================================================================*
      *                                                                *
      *                  Input-Output File hpo                         *
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
      *            * Chiave numero 01 : PTHPRG                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-pth-pos        pic  x(40)                  .
                   15  fil-prg-inp        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATPTH                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-dat-min        pic  9(07)       comp-3     .
                   15  fil-pth-pos-2      pic  x(40)                  .
                   15  fil-prg-inp-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-dat-max            pic  9(07)       comp-3     .
               10  fil-flg-inv            pic  x(01)                  .
      *
               10  fil-tip-doc            pic  x(05)                  .
               10  fil-prt-doc            pic  9(11)       comp-3     .
               10  fil-prg-doc            pic  9(05)       comp-3     .
               10  fil-num-doc            pic  9(11)       comp-3     .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-ord            pic  9(11)       comp-3     .
               10  fil-prt-ord            pic  9(11)       comp-3     .
               10  fil-tip-ids            pic  9(02)                  .
               10  fil-cod-cli            pic  9(07)       comp-3     .
               10  fil-dpz-cli            pic  x(04)                  .
               10  fil-cli-mod            pic  9(07)       comp-3     .
               10  fil-dpz-mod            pic  x(04)                  .
               10  fil-cli-rag            pic  x(60)                  .
               10  fil-cli-via            pic  x(40)                  .
               10  fil-cli-loc            pic  x(40)                  .
               10  fil-cli-fat            pic  9(07)       comp-3     .
               10  fil-dpz-fat            pic  x(04)                  .
               10  fil-qta-rig            pic  9(11)       comp-3     .
               10  fil-cos-uni            pic  9(11)       comp-3     .
               10  fil-prz-uni            pic  9(11)       comp-3     .
               10  fil-cdp-pdt            pic  x(20)                  .
               10  fil-alf-pro            pic  x(14)                  .
               10  fil-des-reg            pic  x(20)                  .
               10  fil-flg-blk            pic  x(01)                  .
      *
               10  fil-num-ele            pic  9(07)       comp-3     .
      *
               10  fil-dat-pos.
                   15  filler occurs 1024 pic  x(01)                  .
               10  fil-flg-sgn            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 199  pic  x(01)                  .

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
      *            * Chiave numero 01 : PTHPRG                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-pth-pos        pic  x(40)                  .
                   15  pul-prg-inp        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATPTH                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-dat-min        pic  9(07)       comp-3     .
                   15  pul-pth-pos-2      pic  x(40)                  .
                   15  pul-prg-inp-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-dat-max            pic  9(07)       comp-3     .
               10  pul-flg-inv            pic  x(01)                  .
      *
               10  pul-tip-doc            pic  x(05)                  .
               10  pul-prt-doc            pic  9(11)       comp-3     .
               10  pul-prg-doc            pic  9(05)       comp-3     .
               10  pul-num-doc            pic  9(11)       comp-3     .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-ord            pic  9(11)       comp-3     .
               10  pul-prt-ord            pic  9(11)       comp-3     .
               10  pul-tip-ids            pic  9(02)                  .
               10  pul-cod-cli            pic  9(07)       comp-3     .
               10  pul-dpz-cli            pic  x(04)                  .
               10  pul-cli-mod            pic  9(07)       comp-3     .
               10  pul-dpz-mod            pic  x(04)                  .
               10  pul-cli-rag            pic  x(60)                  .
               10  pul-cli-via            pic  x(40)                  .
               10  pul-cli-loc            pic  x(40)                  .
               10  pul-cli-fat            pic  9(07)       comp-3     .
               10  pul-dpz-fat            pic  x(04)                  .
               10  pul-qta-rig            pic  9(11)       comp-3     .
               10  pul-cos-uni            pic  9(11)       comp-3     .
               10  pul-prz-uni            pic  9(11)       comp-3     .
               10  pul-cdp-pdt            pic  x(20)                  .
               10  pul-alf-pro            pic  x(14)                  .
               10  pul-des-reg            pic  x(20)                  .
               10  pul-flg-blk            pic  x(01)                  .
      *
               10  pul-num-ele            pic  9(07)       comp-3     .
      *
               10  pul-dat-pos.
                   15  filler occurs 1024 pic  x(01)                  .
               10  pul-flg-sgn            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 199  pic  x(01)                  .


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
                     "hpo "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "sir/agb/fls/ioc/obj/iofhpo              "       .

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
                            "PTHPRG"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATPTH"                                  .
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
      *    * Record logico file [hpo]                                  *
      *    *-----------------------------------------------------------*
           copy      "sir/agb/fls/rec/rfhpo"                          .

      ******************************************************************
       Procedure Division                using f rf-hpo               .
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
           move      spaces               to   rf-hpo                 .
           move      spaces               to   rf-hpo-pth-pos         .
           move      zero                 to   rf-hpo-prg-inp         .
      *
           move      zero                 to   rf-hpo-dat-min         .
           move      zero                 to   rf-hpo-dat-max         .
           move      spaces               to   rf-hpo-flg-inv         .
      *
           move      spaces               to   rf-hpo-tip-doc         .
           move      zero                 to   rf-hpo-prt-doc         .
           move      zero                 to   rf-hpo-prg-doc         .
           move      zero                 to   rf-hpo-num-doc         .
           move      zero                 to   rf-hpo-dat-doc         .
           move      zero                 to   rf-hpo-num-ord         .
           move      zero                 to   rf-hpo-prt-ord         .
           move      zero                 to   rf-hpo-tip-ids         .
           move      zero                 to   rf-hpo-cod-cli         .
           move      spaces               to   rf-hpo-dpz-cli         .
           move      zero                 to   rf-hpo-cli-mod         .
           move      spaces               to   rf-hpo-dpz-mod         .
           move      spaces               to   rf-hpo-cli-rag         .
           move      spaces               to   rf-hpo-cli-via         .
           move      spaces               to   rf-hpo-cli-loc         .
           move      zero                 to   rf-hpo-cli-fat         .
           move      spaces               to   rf-hpo-dpz-fat         .
           move      zero                 to   rf-hpo-qta-rig         .
           move      zero                 to   rf-hpo-cos-uni         .
           move      zero                 to   rf-hpo-prz-uni         .
           move      spaces               to   rf-hpo-cdp-pdt         .
           move      spaces               to   rf-hpo-alf-pro         .
           move      spaces               to   rf-hpo-des-reg         .
           move      spaces               to   rf-hpo-flg-blk         .
      *
           move      zero                 to   rf-hpo-num-ele         .
           move      spaces               to   rf-hpo-dat-pos         .
           move      spaces               to   rf-hpo-flg-sgn         .
           move      spaces               to   rf-hpo-alx-exp         .
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
           move      rf-hpo-dat-max       to   fil-dat-max            .
           move      rf-hpo-flg-inv       to   fil-flg-inv            .
      *
           move      rf-hpo-tip-doc       to   fil-tip-doc            .
           move      rf-hpo-prt-doc       to   fil-prt-doc            .
           move      rf-hpo-prg-doc       to   fil-prg-doc            .
           move      rf-hpo-num-doc       to   fil-num-doc            .
           move      rf-hpo-dat-doc       to   fil-dat-doc            .
           move      rf-hpo-num-ord       to   fil-num-ord            .
           move      rf-hpo-prt-ord       to   fil-prt-ord            .
           move      rf-hpo-tip-ids       to   fil-tip-ids            .
           move      rf-hpo-cod-cli       to   fil-cod-cli            .
           move      rf-hpo-dpz-cli       to   fil-dpz-cli            .
           move      rf-hpo-cli-mod       to   fil-cli-mod            .
           move      rf-hpo-dpz-mod       to   fil-dpz-mod            .
           move      rf-hpo-cli-rag       to   fil-cli-rag            .
           move      rf-hpo-cli-via       to   fil-cli-via            .
           move      rf-hpo-cli-loc       to   fil-cli-loc            .
           move      rf-hpo-cli-fat       to   fil-cli-fat            .
           move      rf-hpo-dpz-fat       to   fil-dpz-fat            .
           move      rf-hpo-qta-rig       to   fil-qta-rig            .
           move      rf-hpo-cos-uni       to   fil-cos-uni            .
           move      rf-hpo-prz-uni       to   fil-prz-uni            .
           move      rf-hpo-cdp-pdt       to   fil-cdp-pdt            .
           move      rf-hpo-alf-pro       to   fil-alf-pro            .
           move      rf-hpo-des-reg       to   fil-des-reg            .
           move      rf-hpo-flg-blk       to   fil-flg-blk            .
      *
           move      rf-hpo-num-ele       to   fil-num-ele            .
           move      rf-hpo-dat-pos       to   fil-dat-pos            .
           move      rf-hpo-flg-sgn       to   fil-flg-sgn            .
           move      rf-hpo-alx-exp       to   fil-alx-exp            .
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
           move      rf-hpo-pth-pos       to   fil-pth-pos            .
           move      rf-hpo-prg-inp       to   fil-prg-inp            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-hpo-dat-min       to   fil-dat-min            .
           move      rf-hpo-pth-pos       to   fil-pth-pos-2          .
           move      rf-hpo-prg-inp       to   fil-prg-inp-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-hpo                 .
           move      fil-pth-pos          to   rf-hpo-pth-pos         .
           move      fil-prg-inp          to   rf-hpo-prg-inp         .
      *
           move      fil-dat-min          to   rf-hpo-dat-min         .
           move      fil-dat-max          to   rf-hpo-dat-max         .
           move      fil-flg-inv          to   rf-hpo-flg-inv         .
      *
           move      fil-tip-doc          to   rf-hpo-tip-doc         .
           move      fil-prt-doc          to   rf-hpo-prt-doc         .
           move      fil-prg-doc          to   rf-hpo-prg-doc         .
           move      fil-num-doc          to   rf-hpo-num-doc         .
           move      fil-dat-doc          to   rf-hpo-dat-doc         .
           move      fil-num-ord          to   rf-hpo-num-ord         .
           move      fil-prt-ord          to   rf-hpo-prt-ord         .
           move      fil-tip-ids          to   rf-hpo-tip-ids         .
           move      fil-cod-cli          to   rf-hpo-cod-cli         .
           move      fil-dpz-cli          to   rf-hpo-dpz-cli         .
           move      fil-cli-mod          to   rf-hpo-cli-mod         .
           move      fil-dpz-mod          to   rf-hpo-dpz-mod         .
           move      fil-cli-rag          to   rf-hpo-cli-rag         .
           move      fil-cli-via          to   rf-hpo-cli-via         .
           move      fil-cli-loc          to   rf-hpo-cli-loc         .
           move      fil-cli-fat          to   rf-hpo-cli-fat         .
           move      fil-dpz-fat          to   rf-hpo-dpz-fat         .
           move      fil-qta-rig          to   rf-hpo-qta-rig         .
           move      fil-cos-uni          to   rf-hpo-cos-uni         .
           move      fil-prz-uni          to   rf-hpo-prz-uni         .
           move      fil-cdp-pdt          to   rf-hpo-cdp-pdt         .
           move      fil-alf-pro          to   rf-hpo-alf-pro         .
           move      fil-des-reg          to   rf-hpo-des-reg         .
           move      fil-flg-blk          to   rf-hpo-flg-blk         .
      *
           move      fil-num-ele          to   rf-hpo-num-ele         .
           move      fil-dat-pos          to   rf-hpo-dat-pos         .
           move      fil-flg-sgn          to   rf-hpo-flg-sgn         .
           move      fil-alx-exp          to   rf-hpo-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-hpo               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-hpo
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

