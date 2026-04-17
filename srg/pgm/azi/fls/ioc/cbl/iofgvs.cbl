       Identification Division.
       Program-Id.                                 iofgvs             .
      *================================================================*
      *                                                                *
      *                  Input-Output File gvs                         *
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
      *            * Chiave numero 01 : CODVEI                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-vei        pic  9(07)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tip-rig            pic  x(05)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-des-vei            pic  x(40)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-doc            pic  x(10)                  .
               10  fil-imp-doc            pic s9(13)       comp-3     .
               10  fil-dat-cge            pic  9(07)       comp-3     .
               10  fil-num-cge            pic  9(07)       comp-3     .
               10  fil-tip-arc            pic  x(01)                  .
               10  fil-cod-arc            pic  9(07)       comp-3     .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-dat-ope            pic  9(07)       comp-3     .
               10  fil-klm-prc            pic  9(11)       comp-3     .
               10  fil-ore-lav            pic  9(11)       comp-3     .
               10  fil-dat-scd            pic  9(07)       comp-3     .
               10  fil-not-ope.
                   15  fil-rig-not occurs 03
                                          pic  x(40)                  .
               10  fil-snx-pma            pic  x(01)                  .
               10  fil-ute-pma            pic  x(08)                  .
               10  fil-cod-pma            pic  9(05)       comp-3     .
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
      *            * Chiave numero 01 : CODVEI                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-vei        pic  9(07)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tip-rig            pic  x(05)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-des-vei            pic  x(40)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-doc            pic  x(10)                  .
               10  pul-imp-doc            pic s9(13)       comp-3     .
               10  pul-dat-cge            pic  9(07)       comp-3     .
               10  pul-num-cge            pic  9(07)       comp-3     .
               10  pul-tip-arc            pic  x(01)                  .
               10  pul-cod-arc            pic  9(07)       comp-3     .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-dat-ope            pic  9(07)       comp-3     .
               10  pul-klm-prc            pic  9(11)       comp-3     .
               10  pul-ore-lav            pic  9(11)       comp-3     .
               10  pul-dat-scd            pic  9(07)       comp-3     .
               10  pul-not-ope.
                   15  fil-rig-not occurs 03
                                          pic  x(40)                  .
               10  pul-snx-pma            pic  x(01)                  .
               10  pul-ute-pma            pic  x(08)                  .
               10  pul-cod-pma            pic  9(05)       comp-3     .
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
                     "gvs "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/azi/fls/ioc/obj/iofgvs              "       .

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
                            "CODVEI"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    1      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [gvs]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfgvs"                          .

      ******************************************************************
       Procedure Division                using f rf-gvs               .
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
           move      spaces               to   rf-gvs                 .
           move      zero                 to   rf-gvs-cod-vei         .
           move      zero                 to   rf-gvs-num-prg         .
           move      spaces               to   rf-gvs-tip-rig         .
           move      spaces               to   rf-gvs-ide-fas         .
           move      zero                 to   rf-gvs-dat-doc         .
           move      spaces               to   rf-gvs-num-doc         .
           move      zero                 to   rf-gvs-imp-doc         .
           move      zero                 to   rf-gvs-dat-cge         .
           move      zero                 to   rf-gvs-num-cge         .
           move      spaces               to   rf-gvs-tip-arc         .
           move      zero                 to   rf-gvs-cod-arc         .
           move      spaces               to   rf-gvs-dpz-arc         .
           move      zero                 to   rf-gvs-dat-ope         .
           move      zero                 to   rf-gvs-klm-prc         .
           move      zero                 to   rf-gvs-ore-lav         .
           move      zero                 to   rf-gvs-dat-scd         .
           move      spaces               to   rf-gvs-not-ope         .
           move      spaces               to   rf-gvs-snx-pma         .
           move      spaces               to   rf-gvs-ute-pma         .
           move      zero                 to   rf-gvs-cod-pma         .
           move      spaces               to   rf-gvs-alx-exp         .
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
           move      rf-gvs-tip-rig       to   fil-tip-rig            .
           move      rf-gvs-ide-fas       to   fil-ide-fas            .
           move      rf-gvs-dat-doc       to   fil-dat-doc            .
           move      rf-gvs-num-doc       to   fil-num-doc            .
           move      rf-gvs-imp-doc       to   fil-imp-doc            .
           move      rf-gvs-dat-cge       to   fil-dat-cge            .
           move      rf-gvs-num-cge       to   fil-num-cge            .
           move      rf-gvs-tip-arc       to   fil-tip-arc            .
           move      rf-gvs-cod-arc       to   fil-cod-arc            .
           move      rf-gvs-dpz-arc       to   fil-dpz-arc            .
           move      rf-gvs-dat-ope       to   fil-dat-ope            .
           move      rf-gvs-klm-prc       to   fil-klm-prc            .
           move      rf-gvs-ore-lav       to   fil-ore-lav            .
           move      rf-gvs-dat-scd       to   fil-dat-scd            .
           move      rf-gvs-not-ope       to   fil-not-ope            .
           move      rf-gvs-snx-pma       to   fil-snx-pma            .
           move      rf-gvs-ute-pma       to   fil-ute-pma            .
           move      rf-gvs-cod-pma       to   fil-cod-pma            .
           move      rf-gvs-alx-exp       to   fil-alx-exp            .
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
           move      rf-gvs-cod-vei       to   fil-cod-vei            .
           move      rf-gvs-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-gvs                 .
           move      fil-cod-vei          to   rf-gvs-cod-vei         .
           move      fil-num-prg          to   rf-gvs-num-prg         .
           move      fil-tip-rig          to   rf-gvs-tip-rig         .
           move      fil-ide-fas          to   rf-gvs-ide-fas         .
           move      fil-dat-doc          to   rf-gvs-dat-doc         .
           move      fil-num-doc          to   rf-gvs-num-doc         .
           move      fil-imp-doc          to   rf-gvs-imp-doc         .
           move      fil-dat-cge          to   rf-gvs-dat-cge         .
           move      fil-num-cge          to   rf-gvs-num-cge         .
           move      fil-tip-arc          to   rf-gvs-tip-arc         .
           move      fil-cod-arc          to   rf-gvs-cod-arc         .
           move      fil-dpz-arc          to   rf-gvs-dpz-arc         .
           move      fil-dat-ope          to   rf-gvs-dat-ope         .
           move      fil-klm-prc          to   rf-gvs-klm-prc         .
           move      fil-ore-lav          to   rf-gvs-ore-lav         .
           move      fil-dat-scd          to   rf-gvs-dat-scd         .
           move      fil-not-ope          to   rf-gvs-not-ope         .
           move      fil-snx-pma          to   rf-gvs-snx-pma         .
           move      fil-ute-pma          to   rf-gvs-ute-pma         .
           move      fil-cod-pma          to   rf-gvs-cod-pma         .
           move      fil-alx-exp          to   rf-gvs-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-gvs               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-gvs
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

