       Identification Division.
       Program-Id.                                 iofyff             .
      *================================================================*
      *                                                                *
      *                  Input-Output File yff                         *
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
      *            * Chiave numero 01 : CODTMF                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-tmf-1      pic  x(05)                  .
                   15  fil-cod-dpz-1      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-des-key-2      pic  x(30)                  .
                   15  fil-cod-tmf-2      pic  x(05)                  .
                   15  fil-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-cod-tmf            pic  x(05)                  .
               10  fil-cod-dpz            pic  9(02)                  .
               10  fil-dti-gen.
                   15  fil-des-key        pic  x(30)                  .
                   15  fil-des-tmf        pic  x(30)                  .
                   15  fil-pwd-tmf        pic  x(08)                  .
                   15  fil-tip-doc        pic  9(02)                  .
                   15  fil-tpv-doc        pic  9(02)                  .
                   15  fil-snx-ird        pic  x(01)                  .
                   15  fil-cau-cge        pic  9(03)                  .
                   15  fil-cau-mag        pic  9(05)                  .
                   15  fil-mov-afd        pic  9(02)                  .
                   15  fil-def-tmf        pic  x(05)                  .
                   15  fil-vld-dpz        pic  x(01)                  .
                   15  fil-def-tpr        pic  x(05)                  .
                   15  fil-num-giv        pic  9(02)                  .
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)       comp-3     .
                   15  fil-sta-tuc        pic  x(05)                  .
                   15  fil-sta-tux        pic  9(02)                  .
                   15  fil-flg-mos        pic  x(01)                  .
                   15  fil-flg-rfp        pic  x(01)                  .
                   15  fil-alx-gen.
                       20  filler  occurs 38
                                          pic  x(01)                  .
               10  fil-dti-dpz.
                   15  fil-cod-dsl        pic  x(07)                  .
                   15  fil-alx-dpz.
                       20  filler occurs 40
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
      *            * Chiave numero 01 : CODTMF                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-tmf-1      pic  x(05)                  .
                   15  pul-cod-dpz-1      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-des-key-2      pic  x(30)                  .
                   15  pul-cod-tmf-2      pic  x(05)                  .
                   15  pul-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-cod-tmf            pic  x(05)                  .
               10  pul-cod-dpz            pic  9(02)                  .
               10  pul-dti-gen.
                   15  pul-des-key        pic  x(30)                  .
                   15  pul-des-tmf        pic  x(30)                  .
                   15  pul-pwd-tmf        pic  x(08)                  .
                   15  pul-tip-doc        pic  9(02)                  .
                   15  pul-tpv-doc        pic  9(02)                  .
                   15  pul-snx-ird        pic  x(01)                  .
                   15  pul-cau-cge        pic  9(03)                  .
                   15  pul-cau-mag        pic  9(05)                  .
                   15  pul-mov-afd        pic  9(02)                  .
                   15  pul-def-tmf        pic  x(05)                  .
                   15  pul-vld-dpz        pic  x(01)                  .
                   15  pul-def-tpr        pic  x(05)                  .
                   15  pul-num-giv        pic  9(02)                  .
                   15  pul-sta-tus        pic  9(02)                  .
                   15  pul-sta-tud        pic  9(07)       comp-3     .
                   15  pul-sta-tuc        pic  x(05)                  .
                   15  pul-sta-tux        pic  9(02)                  .
                   15  pul-flg-mos        pic  x(01)                  .
                   15  pul-flg-rfp        pic  x(01)                  .
                   15  pul-alx-gen.
                       20  filler  occurs 38
                                          pic  x(01)                  .
               10  pul-dti-dpz.
                   15  pul-cod-dsl        pic  x(07)                  .
                   15  pul-alx-dpz.
                       20  filler occurs 40
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
                     "yff "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/ffo/fls/ioc/obj/iofyff              "       .

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
                            "CODTMF    "                              .
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

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  w-c01                      pic  9(02)                  .
           05  w-c02                      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [yff]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfyff"                          .

      ******************************************************************
       Procedure Division                using f rf-yff               .
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
           move      spaces               to   rf-yff                 .
      *              *-------------------------------------------------*
      *              * Uso sperimentale di una istruzione unica per    *
      *              * normalizzare l'intero record file               *
      *              *-------------------------------------------------*
           initialize                          rf-yff                 .
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
           move      rf-yff-cod-tmf       to   fil-cod-tmf            .
           move      rf-yff-cod-dpz       to   fil-cod-dpz            .
           move      rf-yff-des-key       to   fil-des-key            .
           move      rf-yff-des-tmf       to   fil-des-tmf            .
           move      rf-yff-pwd-tmf       to   fil-pwd-tmf            .
           move      rf-yff-tip-doc       to   fil-tip-doc            .
           move      rf-yff-tpv-doc       to   fil-tpv-doc            .
           move      rf-yff-snx-ird       to   fil-snx-ird            .
           move      rf-yff-cau-cge       to   fil-cau-cge            .
           move      rf-yff-cau-mag       to   fil-cau-mag            .
           move      rf-yff-mov-afd       to   fil-mov-afd            .
           move      rf-yff-def-tmf       to   fil-def-tmf            .
           move      rf-yff-vld-dpz       to   fil-vld-dpz            .
           move      rf-yff-def-tpr       to   fil-def-tpr            .
           move      rf-yff-num-giv       to   fil-num-giv            .
           move      rf-yff-sta-tus       to   fil-sta-tus            .
           move      rf-yff-sta-tud       to   fil-sta-tud            .
           move      rf-yff-sta-tuc       to   fil-sta-tuc            .
           move      rf-yff-sta-tux       to   fil-sta-tux            .
           move      rf-yff-flg-mos       to   fil-flg-mos            .
           move      rf-yff-flg-rfp       to   fil-flg-rfp            .
           move      rf-yff-alx-gen       to   fil-alx-gen            .
           move      rf-yff-cod-dsl       to   fil-cod-dsl            .
           move      rf-yff-alx-dpz       to   fil-alx-dpz            .
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
           move      rf-yff-cod-tmf       to   fil-cod-tmf-1          .
           move      rf-yff-cod-dpz       to   fil-cod-dpz-1          .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-yff-des-key       to   fil-des-key-2          .
           move      rf-yff-cod-tmf       to   fil-cod-tmf-2          .
           move      rf-yff-cod-dpz       to   fil-cod-dpz-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-yff                 .
           move      fil-cod-tmf          to   rf-yff-cod-tmf         .
           move      fil-cod-dpz          to   rf-yff-cod-dpz         .
           move      fil-des-key          to   rf-yff-des-key         .
           move      fil-des-tmf          to   rf-yff-des-tmf         .
           move      fil-pwd-tmf          to   rf-yff-pwd-tmf         .
           move      fil-tip-doc          to   rf-yff-tip-doc         .
           move      fil-tpv-doc          to   rf-yff-tpv-doc         .
           move      fil-snx-ird          to   rf-yff-snx-ird         .
           move      fil-cau-cge          to   rf-yff-cau-cge         .
           move      fil-cau-mag          to   rf-yff-cau-mag         .
           move      fil-mov-afd          to   rf-yff-mov-afd         .
           move      fil-def-tmf          to   rf-yff-def-tmf         .
           move      fil-vld-dpz          to   rf-yff-vld-dpz         .
           move      fil-def-tpr          to   rf-yff-def-tpr         .
           move      fil-num-giv          to   rf-yff-num-giv         .
           move      fil-sta-tus          to   rf-yff-sta-tus         .
           move      fil-sta-tud          to   rf-yff-sta-tud         .
           move      fil-sta-tuc          to   rf-yff-sta-tuc         .
           move      fil-sta-tux          to   rf-yff-sta-tux         .
           move      fil-flg-mos          to   rf-yff-flg-mos         .
           move      fil-flg-rfp          to   rf-yff-flg-rfp         .
           move      fil-alx-gen          to   rf-yff-alx-gen         .
           move      fil-cod-dsl          to   rf-yff-cod-dsl         .
           move      fil-alx-dpz          to   rf-yff-alx-dpz         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-yff               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-yff
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

