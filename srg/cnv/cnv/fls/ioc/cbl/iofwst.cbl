       Identification Division.
       Program-Id.                                 iofwst             .
      *================================================================*
      *                                                                *
      *                  Input-Output File wst                         *
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
      *            * Chiave numero 01 : LSTPRO                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-rec        pic  9(02)                  .
                   15  fil-cod-wst        pic  x(03)                  .
                   15  fil-cod-cli        pic  9(07)       comp-3     .
                   15  fil-sgl-vlt        pic  x(03)                  .
                   15  fil-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PROLST                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-tip-rec-2      pic  9(02)                  .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
                   15  fil-sgl-vlt-2      pic  x(03)                  .
                   15  fil-cod-wst-2      pic  x(03)                  .
                   15  fil-cod-cli-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-prz-wst            pic  9(09)       comp-3     .
               10  fil-cat-pvg            pic  9(05)       comp-3     .
               10  fil-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-cat-sco            pic  9(05)       comp-3     .
               10  fil-per-sco occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-snx-prz            pic  x(01)                  .
               10  fil-snx-sco            pic  x(01)                  .
               10  fil-snx-pvg            pic  x(01)                  .
               10  fil-dva-ini            pic  9(07)                  .
               10  fil-dva-fin            pic  9(07)                  .
               10  fil-qta-rif            pic  9(08)v9(03) comp-3     .
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
      *            * Chiave numero 01 : LSTPRO                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-rec        pic  9(02)                  .
                   15  pul-cod-wst        pic  x(03)                  .
                   15  pul-cod-cli        pic  9(07)       comp-3     .
                   15  pul-sgl-vlt        pic  x(03)                  .
                   15  pul-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PROLST                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-tip-rec-2      pic  9(02)                  .
                   15  pul-num-pro-2      pic  9(07)       comp-3     .
                   15  pul-sgl-vlt-2      pic  x(03)                  .
                   15  pul-cod-wst-2      pic  x(03)                  .
                   15  pul-cod-cli-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-prz-wst            pic  9(09)       comp-3     .
               10  pul-cat-pvg            pic  9(05)       comp-3     .
               10  pul-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-cat-sco            pic  9(05)       comp-3     .
               10  pul-per-sco occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-snx-prz            pic  x(01)                  .
               10  pul-snx-sco            pic  x(01)                  .
               10  pul-snx-pvg            pic  x(01)                  .
               10  pul-dva-ini            pic  9(07)                  .
               10  pul-dva-fin            pic  9(07)                  .
               10  pul-qta-rif            pic  9(08)v9(03) comp-3     .
               10  pul-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

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
                     "wst "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "cnv/cnv/fls/ioc/obj/iofwst              "       .

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
                            "LSTPRO"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PROLST"                                  .
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
      *    * Record logico file [wst]                                  *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/fls/rec/rfwst"                          .

      ******************************************************************
       Procedure Division                using f rf-wst               .
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
           move      spaces               to   rf-wst                 .
           move      zero                 to   rf-wst-tip-rec         .
           move      spaces               to   rf-wst-cod-wst         .
           move      zero                 to   rf-wst-cod-cli         .
           move      spaces               to   rf-wst-sgl-vlt         .
           move      zero                 to   rf-wst-dec-vlt         .
           move      zero                 to   rf-wst-num-pro         .
           move      zero                 to   rf-wst-prz-wst         .
           move      zero                 to   rf-wst-cat-pvg         .
           move      zero                 to   rf-wst-per-pvg (1)     .
           move      zero                 to   rf-wst-per-pvg (2)     .
           move      zero                 to   rf-wst-per-pvg (3)     .
           move      zero                 to   rf-wst-cat-sco         .
           move      zero                 to   rf-wst-per-sco (1)     .
           move      zero                 to   rf-wst-per-sco (2)     .
           move      zero                 to   rf-wst-per-sco (3)     .
           move      zero                 to   rf-wst-per-sco (4)     .
           move      zero                 to   rf-wst-per-sco (5)     .
           move      spaces               to   rf-wst-snx-prz         .
           move      spaces               to   rf-wst-snx-pvg         .
           move      spaces               to   rf-wst-snx-sco         .
           move      zero                 to   rf-wst-dva-ini         .
           move      zero                 to   rf-wst-dva-fin         .
           move      zero                 to   rf-wst-qta-rif         .
           move      spaces               to   rf-wst-alx-exp         .
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
           move      rf-wst-dec-vlt       to   fil-dec-vlt            .
           move      rf-wst-prz-wst       to   fil-prz-wst            .
           move      rf-wst-cat-pvg       to   fil-cat-pvg            .
           move      rf-wst-per-pvg (1)   to   fil-per-pvg (1)        .
           move      rf-wst-per-pvg (2)   to   fil-per-pvg (2)        .
           move      rf-wst-per-pvg (3)   to   fil-per-pvg (3)        .
           move      rf-wst-cat-sco       to   fil-cat-sco            .
           move      rf-wst-per-sco (1)   to   fil-per-sco (1)        .
           move      rf-wst-per-sco (2)   to   fil-per-sco (2)        .
           move      rf-wst-per-sco (3)   to   fil-per-sco (3)        .
           move      rf-wst-per-sco (4)   to   fil-per-sco (4)        .
           move      rf-wst-per-sco (5)   to   fil-per-sco (5)        .
           move      rf-wst-snx-prz       to   fil-snx-prz            .
           move      rf-wst-snx-pvg       to   fil-snx-pvg            .
           move      rf-wst-snx-sco       to   fil-snx-sco            .
           move      rf-wst-dva-ini       to   fil-dva-ini            .
           move      rf-wst-dva-fin       to   fil-dva-fin            .
           move      rf-wst-qta-rif       to   fil-qta-rif            .
           move      rf-wst-alx-exp       to   fil-alx-exp            .
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
           move      rf-wst-tip-rec       to   fil-tip-rec            .
           move      rf-wst-cod-wst       to   fil-cod-wst            .
           move      rf-wst-cod-cli       to   fil-cod-cli            .
           move      rf-wst-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-wst-num-pro       to   fil-num-pro            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-wst-tip-rec       to   fil-tip-rec-2          .
           move      rf-wst-num-pro       to   fil-num-pro-2          .
           move      rf-wst-sgl-vlt       to   fil-sgl-vlt-2          .
           move      rf-wst-cod-wst       to   fil-cod-wst-2          .
           move      rf-wst-cod-cli       to   fil-cod-cli-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-wst                 .
           move      fil-tip-rec          to   rf-wst-tip-rec         .
           move      fil-cod-wst          to   rf-wst-cod-wst         .
           move      fil-cod-cli          to   rf-wst-cod-cli         .
           move      fil-sgl-vlt          to   rf-wst-sgl-vlt         .
           move      fil-dec-vlt          to   rf-wst-dec-vlt         .
           move      fil-num-pro          to   rf-wst-num-pro         .
           move      fil-prz-wst          to   rf-wst-prz-wst         .
           move      fil-cat-pvg          to   rf-wst-cat-pvg         .
           move      fil-per-pvg (1)      to   rf-wst-per-pvg (1)     .
           move      fil-per-pvg (2)      to   rf-wst-per-pvg (2)     .
           move      fil-per-pvg (3)      to   rf-wst-per-pvg (3)     .
           move      fil-cat-sco          to   rf-wst-cat-sco         .
           move      fil-per-sco (1)      to   rf-wst-per-sco (1)     .
           move      fil-per-sco (2)      to   rf-wst-per-sco (2)     .
           move      fil-per-sco (3)      to   rf-wst-per-sco (3)     .
           move      fil-per-sco (4)      to   rf-wst-per-sco (4)     .
           move      fil-per-sco (5)      to   rf-wst-per-sco (5)     .
           move      fil-snx-prz          to   rf-wst-snx-prz         .
           move      fil-snx-pvg          to   rf-wst-snx-pvg         .
           move      fil-snx-sco          to   rf-wst-snx-sco         .
           move      fil-dva-ini          to   rf-wst-dva-ini         .
           move      fil-dva-fin          to   rf-wst-dva-fin         .
           move      fil-qta-rif          to   rf-wst-qta-rif         .
           move      fil-alx-exp          to   rf-wst-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-wst               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-wst
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

