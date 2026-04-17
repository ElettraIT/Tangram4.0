       Identification Division.
       Program-Id.                                 iofmmv             .
      *================================================================*
      *                                                                *
      *                  Input-Output File mmv                         *
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
                             file status  is                f-fil-sts .

      *    *===========================================================*
      *    * File Control [pul]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pul   assign to disk           f-pul-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pul-k01
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
      *            * Chiave numero 01 : ESECOD                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-ann-ese        pic  9(03)       comp-3     .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ese-pre occurs 10.
                   15  fil-rim-pre        pic s9(08)v9(03) comp-3     .
                   15  fil-ump-pre        pic  9(11)       comp-3     .
                   15  fil-vnb-pre        pic  9(11)       comp-3     .
                   15  fil-fdr-pre        pic  x(01)                  .
               10  fil-dcu-ies            pic  9(07)       comp-3     .
               10  fil-ucu-ies            pic  9(11)       comp-3     .
               10  fil-dst-ies            pic  9(07)       comp-3     .
               10  fil-ust-ies            pic  9(11)       comp-3     .
               10  fil-dst-gea            pic  9(07)       comp-3     .
               10  fil-ust-gea            pic  9(11)       comp-3     .
               10  fil-vnb-gea            pic  9(11)       comp-3     .
               10  fil-mes-mns occurs 12.
                   15  fil-qav-mns        pic s9(08)v9(03) comp-3     .
                   15  fil-qcs-mns        pic s9(08)v9(03) comp-3     .
                   15  fil-qtv-mns        pic s9(08)v9(03) comp-3     .
                   15  fil-vlv-mns        pic s9(11)       comp-3     .
                   15  fil-pca-mns        pic s9(11)       comp-3     .
                   15  fil-prc-mns        pic s9(11)       comp-3     .
                   15  fil-dcu-mns        pic  9(07)       comp-3     .
                   15  fil-ucu-mns        pic  9(11)       comp-3     .
                   15  fil-dst-mns        pic  9(07)       comp-3     .
                   15  fil-ust-mns        pic  9(11)       comp-3     .
               10  fil-pca-che            pic s9(11)       comp-3     .
               10  fil-prc-che            pic s9(11)       comp-3     .
               10  fil-fdr-gea            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 19   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [pul]                                    *
      *    *-----------------------------------------------------------*
       fd  pul       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  pul-rec.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : ESECOD                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-ann-ese        pic  9(03)       comp-3     .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ese-pre occurs 10.
                   15  pul-rim-pre        pic s9(08)v9(03) comp-3     .
                   15  pul-ump-pre        pic  9(11)       comp-3     .
                   15  pul-vnb-pre        pic  9(11)       comp-3     .
                   15  pul-fdr-pre        pic  x(01)                  .
               10  pul-dcu-ies            pic  9(07)       comp-3     .
               10  pul-ucu-ies            pic  9(11)       comp-3     .
               10  pul-dst-ies            pic  9(07)       comp-3     .
               10  pul-ust-ies            pic  9(11)       comp-3     .
               10  pul-dst-gea            pic  9(07)       comp-3     .
               10  pul-ust-gea            pic  9(11)       comp-3     .
               10  pul-vnb-gea            pic  9(11)       comp-3     .
               10  pul-mes-mns occurs 12.
                   15  pul-qav-mns        pic s9(08)v9(03) comp-3     .
                   15  pul-qcs-mns        pic s9(08)v9(03) comp-3     .
                   15  pul-qtv-mns        pic s9(08)v9(03) comp-3     .
                   15  pul-vlv-mns        pic s9(11)       comp-3     .
                   15  pul-pca-mns        pic s9(11)       comp-3     .
                   15  pul-prc-mns        pic s9(11)       comp-3     .
                   15  pul-dcu-mns        pic  9(07)       comp-3     .
                   15  pul-ucu-mns        pic  9(11)       comp-3     .
                   15  pul-dst-mns        pic  9(07)       comp-3     .
                   15  pul-ust-mns        pic  9(11)       comp-3     .
               10  pul-pca-che            pic s9(11)       comp-3     .
               10  pul-prc-che            pic s9(11)       comp-3     .
               10  pul-fdr-gea            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 19   pic  x(01)                  .

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
                     "mmv "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/mag/fls/ioc/obj/iofmmv              "       .

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
           05  k-ctr                      pic  9(02) value 1          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ESECOD    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    1      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Contatore 'I' di comodo                               *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [mmv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmv"                          .

      ******************************************************************
       Procedure Division                using f rf-mmv               .
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
           move      spaces               to   rf-mmv                 .
           move      zero                 to   rf-mmv-ann-ese         .
           move      zero                 to   rf-mmv-tip-mag         .
           move      zero                 to   rf-mmv-num-mag         .
           move      zero                 to   I                      .
       nor-rec-log-100.
           add       1                    to   I                      .
           if        I                    >    10
                     go to nor-rec-log-120.
           move      zero                 to   rf-mmv-rim-pre (I)     .
           move      zero                 to   rf-mmv-ump-pre (I)     .
           move      zero                 to   rf-mmv-vnb-pre (I)     .
           move      spaces               to   rf-mmv-fdr-pre (I)     .
           go to     nor-rec-log-100.
       nor-rec-log-120.
           move      zero                 to   rf-mmv-dcu-ies         .
           move      zero                 to   rf-mmv-ucu-ies         .
           move      zero                 to   rf-mmv-dst-ies         .
           move      zero                 to   rf-mmv-ust-ies         .
           move      zero                 to   rf-mmv-dst-gea         .
           move      zero                 to   rf-mmv-ust-gea         .
           move      zero                 to   rf-mmv-vnb-gea         .
           move      spaces               to   rf-mmv-fdr-gea         .
           move      zero                 to   I                      .
       nor-rec-log-200.
           add       1                    to   I                      .
           if        I                    >    12
                     go to nor-rec-log-220.
           move      zero                 to   rf-mmv-qav-mns (I)     .
           move      zero                 to   rf-mmv-qcs-mns (I)     .
           move      zero                 to   rf-mmv-qtv-mns (I)     .
           move      zero                 to   rf-mmv-vlv-mns (I)     .
           move      zero                 to   rf-mmv-pca-mns (I)     .
           move      zero                 to   rf-mmv-prc-mns (I)     .
           move      zero                 to   rf-mmv-dcu-mns (I)     .
           move      zero                 to   rf-mmv-ucu-mns (I)     .
           move      zero                 to   rf-mmv-dst-mns (I)     .
           move      zero                 to   rf-mmv-ust-mns (I)     .
           go to     nor-rec-log-200.
       nor-rec-log-220.
           move      zero                 to   rf-mmv-pca-che         .
           move      zero                 to   rf-mmv-prc-che         .
           move      spaces               to   rf-mmv-alx-exp         .
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
           move      zero                 to   I                      .
       cmp-log-fis-200.
           add       1                    to   I                      .
           if        I                    >    10
                     go to cmp-log-fis-220.
           move      rf-mmv-rim-pre (I)   to   fil-rim-pre (I)        .
           move      rf-mmv-ump-pre (I)   to   fil-ump-pre (I)        .
           move      rf-mmv-vnb-pre (I)   to   fil-vnb-pre (I)        .
           move      rf-mmv-fdr-pre (I)   to   fil-fdr-pre (I)        .
           go to     cmp-log-fis-200.
       cmp-log-fis-220.
           move      rf-mmv-dcu-ies       to   fil-dcu-ies            .
           move      rf-mmv-ucu-ies       to   fil-ucu-ies            .
           move      rf-mmv-dst-ies       to   fil-dst-ies            .
           move      rf-mmv-ust-ies       to   fil-ust-ies            .
           move      rf-mmv-dst-gea       to   fil-dst-gea         .
           move      rf-mmv-ust-gea       to   fil-ust-gea         .
           move      rf-mmv-vnb-gea       to   fil-vnb-gea         .
           move      rf-mmv-fdr-gea       to   fil-fdr-gea         .
           move      zero                 to   I                      .
       cmp-log-fis-300.
           add       1                    to   I                      .
           if        I                    >    12
                     go to cmp-log-fis-320.
           move      rf-mmv-qav-mns (I)   to   fil-qav-mns (I)        .
           move      rf-mmv-qcs-mns (I)   to   fil-qcs-mns (I)        .
           move      rf-mmv-qtv-mns (I)   to   fil-qtv-mns (I)        .
           move      rf-mmv-vlv-mns (I)   to   fil-vlv-mns (I)        .
           move      rf-mmv-pca-mns (I)   to   fil-pca-mns (I)        .
           move      rf-mmv-prc-mns (I)   to   fil-prc-mns (I)        .
           move      rf-mmv-dcu-mns (I)   to   fil-dcu-mns (I)        .
           move      rf-mmv-ucu-mns (I)   to   fil-ucu-mns (I)        .
           move      rf-mmv-dst-mns (I)   to   fil-dst-mns (I)        .
           move      rf-mmv-ust-mns (I)   to   fil-ust-mns (I)        .
           go to     cmp-log-fis-300.
       cmp-log-fis-320.
           move      rf-mmv-pca-che       to   fil-pca-che            .
           move      rf-mmv-prc-che       to   fil-prc-che            .
           move      rf-mmv-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-mmv-ann-ese       to   fil-ann-ese            .
           move      rf-mmv-tip-mag       to   fil-tip-mag            .
           move      rf-mmv-num-mag       to   fil-num-mag            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-mmv                 .
           move      fil-ann-ese          to   rf-mmv-ann-ese         .
           move      fil-tip-mag          to   rf-mmv-tip-mag         .
           move      fil-num-mag          to   rf-mmv-num-mag         .
           move      zero                 to   I                      .
       dec-fis-log-200.
           add       1                    to   I                      .
           if        I                    >    10
                     go to dec-fis-log-220.
           move      fil-rim-pre (I)      to   rf-mmv-rim-pre (I)     .
           move      fil-ump-pre (I)      to   rf-mmv-ump-pre (I)     .
           move      fil-vnb-pre (I)      to   rf-mmv-vnb-pre (I)     .
           move      fil-fdr-pre (I)      to   rf-mmv-fdr-pre (I)     .
           go to     dec-fis-log-200.
       dec-fis-log-220.
           move      fil-dcu-ies          to   rf-mmv-dcu-ies         .
           move      fil-ucu-ies          to   rf-mmv-ucu-ies         .
           move      fil-dst-ies          to   rf-mmv-dst-ies         .
           move      fil-ust-ies          to   rf-mmv-ust-ies         .
           move      fil-dst-gea          to   rf-mmv-dst-gea         .
           move      fil-ust-gea          to   rf-mmv-ust-gea         .
           move      fil-vnb-gea          to   rf-mmv-vnb-gea         .
           move      fil-fdr-gea          to   rf-mmv-fdr-gea         .
           move      zero                 to   I                      .
       dec-fis-log-300.
           add       1                    to   I                      .
           if        I                    >    12
                     go to dec-fis-log-320.
           move      fil-qav-mns (I)      to   rf-mmv-qav-mns (I)     .
           move      fil-qcs-mns (I)      to   rf-mmv-qcs-mns (I)     .
           move      fil-qtv-mns (I)      to   rf-mmv-qtv-mns (I)     .
           move      fil-vlv-mns (I)      to   rf-mmv-vlv-mns (I)     .
           move      fil-pca-mns (I)      to   rf-mmv-pca-mns (I)     .
           move      fil-prc-mns (I)      to   rf-mmv-prc-mns (I)     .
           move      fil-dcu-mns (I)      to   rf-mmv-dcu-mns (I)     .
           move      fil-ucu-mns (I)      to   rf-mmv-ucu-mns (I)     .
           move      fil-dst-mns (I)      to   rf-mmv-dst-mns (I)     .
           move      fil-ust-mns (I)      to   rf-mmv-ust-mns (I)     .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      fil-pca-che          to   rf-mmv-pca-che         .
           move      fil-prc-che          to   rf-mmv-prc-che         .
           move      fil-alx-exp          to   rf-mmv-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-mmv               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-mmv
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

