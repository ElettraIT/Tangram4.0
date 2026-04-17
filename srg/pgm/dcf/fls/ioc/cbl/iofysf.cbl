       Identification Division.
       Program-Id.                                 iofysf             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ysf                         *
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
      *            * Chiave numero 01 : CODSPF                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-spf        pic  9(03)       comp-3     .
                   15  fil-cod-lng        pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-ves            pic  x(25)                  .
               10  fil-des-vri            pic  x(15)                  .
               10  fil-des-stp            pic  x(40)                  .
               10  fil-tfu-spe            pic  9(02)                  .
               10  fil-per-spe            pic  9(02)v9(01)            .
               10  fil-ibl-spe            pic  9(02)                  .
               10  fil-ibt-spe.
                   15  fil-ibx-spe occurs 09
                                          pic  x(01)                  .
               10  fil-imp-spe            pic  9(09)       comp-3     .
               10  fil-civ-spe            pic  9(05)       comp-3     .
               10  fil-ccp-spe            pic  9(07)       comp-3     .
               10  fil-alx-exp            pic  x(40)                  .

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
      *            * Chiave numero 01 : CODSPF                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-spf        pic  9(03)       comp-3     .
                   15  pul-cod-lng        pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-ves            pic  x(25)                  .
               10  pul-des-vri            pic  x(15)                  .
               10  pul-des-stp            pic  x(40)                  .
               10  pul-tfu-spe            pic  9(02)                  .
               10  pul-per-spe            pic  9(02)v9(01)            .
               10  pul-ibl-spe            pic  9(02)                  .
               10  pul-ibt-spe.
                   15  pul-ibx-spe occurs 09
                                          pic  x(01)                  .
               10  pul-imp-spe            pic  9(09)       comp-3     .
               10  pul-civ-spe            pic  9(05)       comp-3     .
               10  pul-ccp-spe            pic  9(07)       comp-3     .
               10  pul-alx-exp            pic  x(40)                  .

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
                     "ysf "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcf/fls/ioc/obj/iofysf              "       .

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
                            "CODSPF"                                  .
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
      *    * Record logico file [ysf]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfysf"                          .

      ******************************************************************
       Procedure Division                using f rf-ysf               .
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
           move      spaces               to   rf-ysf                 .
           move      zero                 to   rf-ysf-num-spf         .
           move      spaces               to   rf-ysf-cod-lng         .
           move      spaces               to   rf-ysf-des-ves         .
           move      spaces               to   rf-ysf-des-vri         .
           move      spaces               to   rf-ysf-des-stp         .
           move      zero                 to   rf-ysf-tfu-spe         .
           move      zero                 to   rf-ysf-per-spe         .
           move      zero                 to   rf-ysf-ibl-spe         .
           move      spaces               to   rf-ysf-ibx-spe (1)     .
           move      spaces               to   rf-ysf-ibx-spe (2)     .
           move      spaces               to   rf-ysf-ibx-spe (3)     .
           move      spaces               to   rf-ysf-ibx-spe (4)     .
           move      spaces               to   rf-ysf-ibx-spe (5)     .
           move      spaces               to   rf-ysf-ibx-spe (6)     .
           move      spaces               to   rf-ysf-ibx-spe (7)     .
           move      spaces               to   rf-ysf-ibx-spe (8)     .
           move      spaces               to   rf-ysf-ibx-spe (9)     .
           move      zero                 to   rf-ysf-imp-spe         .
           move      zero                 to   rf-ysf-civ-spe         .
           move      zero                 to   rf-ysf-ccp-spe         .
           move      spaces               to   rf-ysf-alx-exp         .
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
           move      rf-ysf-des-ves       to   fil-des-ves            .
           move      rf-ysf-des-vri       to   fil-des-vri            .
           move      rf-ysf-des-stp       to   fil-des-stp            .
           move      rf-ysf-tfu-spe       to   fil-tfu-spe            .
           move      rf-ysf-per-spe       to   fil-per-spe            .
           move      rf-ysf-ibl-spe       to   fil-ibl-spe            .
           move      rf-ysf-ibx-spe (1)   to   fil-ibx-spe (1)        .
           move      rf-ysf-ibx-spe (2)   to   fil-ibx-spe (2)        .
           move      rf-ysf-ibx-spe (3)   to   fil-ibx-spe (3)        .
           move      rf-ysf-ibx-spe (4)   to   fil-ibx-spe (4)        .
           move      rf-ysf-ibx-spe (5)   to   fil-ibx-spe (5)        .
           move      rf-ysf-ibx-spe (6)   to   fil-ibx-spe (6)        .
           move      rf-ysf-ibx-spe (7)   to   fil-ibx-spe (7)        .
           move      rf-ysf-ibx-spe (8)   to   fil-ibx-spe (8)        .
           move      rf-ysf-ibx-spe (9)   to   fil-ibx-spe (9)        .
           move      rf-ysf-imp-spe       to   fil-imp-spe            .
           move      rf-ysf-civ-spe       to   fil-civ-spe            .
           move      rf-ysf-ccp-spe       to   fil-ccp-spe            .
           move      rf-ysf-alx-exp       to   fil-alx-exp            .
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
           move      rf-ysf-num-spf       to   fil-num-spf            .
           move      rf-ysf-cod-lng       to   fil-cod-lng            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ysf                 .
           move      fil-num-spf          to   rf-ysf-num-spf         .
           move      fil-cod-lng          to   rf-ysf-cod-lng         .
           move      fil-des-ves          to   rf-ysf-des-ves         .
           move      fil-des-vri          to   rf-ysf-des-vri         .
           move      fil-des-stp          to   rf-ysf-des-stp         .
           move      fil-tfu-spe          to   rf-ysf-tfu-spe         .
           move      fil-per-spe          to   rf-ysf-per-spe         .
           move      fil-ibl-spe          to   rf-ysf-ibl-spe         .
           move      fil-ibx-spe (1)      to   rf-ysf-ibx-spe (1)     .
           move      fil-ibx-spe (2)      to   rf-ysf-ibx-spe (2)     .
           move      fil-ibx-spe (3)      to   rf-ysf-ibx-spe (3)     .
           move      fil-ibx-spe (4)      to   rf-ysf-ibx-spe (4)     .
           move      fil-ibx-spe (5)      to   rf-ysf-ibx-spe (5)     .
           move      fil-ibx-spe (6)      to   rf-ysf-ibx-spe (6)     .
           move      fil-ibx-spe (7)      to   rf-ysf-ibx-spe (7)     .
           move      fil-ibx-spe (8)      to   rf-ysf-ibx-spe (8)     .
           move      fil-ibx-spe (9)      to   rf-ysf-ibx-spe (9)     .
           move      fil-imp-spe          to   rf-ysf-imp-spe         .
           move      fil-civ-spe          to   rf-ysf-civ-spe         .
           move      fil-ccp-spe          to   rf-ysf-ccp-spe         .
           move      fil-alx-exp          to   rf-ysf-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ysf               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ysf
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

