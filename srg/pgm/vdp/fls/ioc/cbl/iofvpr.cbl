       Identification Division.
       Program-Id.                                 iofvpr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File vdp                         *
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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-prt        pic  9(11)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tip-mag            pic  9(02)                  .
               10  fil-num-mag            pic  9(07)       comp-3     .
               10  fil-alf-mag            pic  x(14)                  .
               10  fil-sgl-vrn            pic  x(14)                  .
               10  fil-umi-prd            pic  x(03)                  .
               10  fil-dec-qta            pic  9(01)                  .
               10  fil-qta-cns            pic s9(06)v9(03) comp-3     .
               10  fil-cod-dsl            pic  x(07)                  .
               10  fil-snx-2qt            pic  9(01)                  .
               10  fil-dec-2qt            pic  9(01)                  .
               10  fil-qta-a02            pic s9(06)v9(03) comp-3     .
               10  fil-snx-3qt            pic  9(01)                  .
               10  fil-dec-3qt            pic  9(01)                  .
               10  fil-qta-a03            pic s9(06)v9(03) comp-3     .
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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-prt        pic  9(11)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tip-mag            pic  9(02)                  .
               10  pul-num-mag            pic  9(07)       comp-3     .
               10  pul-alf-mag            pic  x(14)                  .
               10  pul-sgl-vrn            pic  x(14)                  .
               10  pul-umi-prd            pic  x(03)                  .
               10  pul-dec-qta            pic  9(01)                  .
               10  pul-qta-cns            pic s9(06)v9(03) comp-3     .
               10  pul-cod-dsl            pic  x(07)                  .
               10  pul-snx-2qt            pic  9(01)                  .
               10  pul-dec-2qt            pic  9(01)                  .
               10  pul-qta-a02            pic s9(06)v9(03) comp-3     .
               10  pul-snx-3qt            pic  9(01)                  .
               10  pul-dec-3qt            pic  9(01)                  .
               10  pul-qta-a03            pic s9(06)v9(03) comp-3     .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
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
                     "vpr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/vdp/fls/ioc/obj/iofvpr              "       .

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
                            "NUMPRT    "                              .
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
      *    * Record logico file [vpr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfvpr"                          .

      ******************************************************************
       Procedure Division                using f rf-vpr               .
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
           move      spaces               to   rf-vpr                 .
           move      zero                 to   rf-vpr-num-prt         .
           move      zero                 to   rf-vpr-num-prg         .
           move      zero                 to   rf-vpr-tip-mag         .
           move      zero                 to   rf-vpr-num-mag         .
           move      spaces               to   rf-vpr-alf-mag         .
           move      spaces               to   rf-vpr-sgl-vrn         .
           move      spaces               to   rf-vpr-umi-prd         .
           move      zero                 to   rf-vpr-dec-qta         .
           move      zero                 to   rf-vpr-qta-cns         .
           move      spaces               to   rf-vpr-cod-dsl         .
           move      zero                 to   rf-vpr-snx-2qt         .
           move      zero                 to   rf-vpr-dec-2qt         .
           move      zero                 to   rf-vpr-qta-a02         .
           move      zero                 to   rf-vpr-snx-3qt         .
           move      zero                 to   rf-vpr-dec-3qt         .
           move      zero                 to   rf-vpr-qta-a03         .
           move      spaces               to   rf-vpr-flg-ela         .
           move      spaces               to   rf-vpr-flg-pul         .
           move      spaces               to   rf-vpr-alx-exp         .
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
           move      rf-vpr-tip-mag       to   fil-tip-mag            .
           move      rf-vpr-num-mag       to   fil-num-mag            .
           move      rf-vpr-alf-mag       to   fil-alf-mag            .
           move      rf-vpr-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-vpr-umi-prd       to   fil-umi-prd            .
           move      rf-vpr-dec-qta       to   fil-dec-qta            .
           move      rf-vpr-qta-cns       to   fil-qta-cns            .
           move      rf-vpr-cod-dsl       to   fil-cod-dsl            .
           move      rf-vpr-snx-2qt       to   fil-snx-2qt            .
           move      rf-vpr-dec-2qt       to   fil-dec-2qt            .
           move      rf-vpr-qta-a02       to   fil-qta-a02            .
           move      rf-vpr-snx-3qt       to   fil-snx-3qt            .
           move      rf-vpr-dec-3qt       to   fil-dec-3qt            .
           move      rf-vpr-qta-a03       to   fil-qta-a03            .
           move      rf-vpr-flg-ela       to   fil-flg-ela            .
           move      rf-vpr-flg-pul       to   fil-flg-pul            .
           move      rf-vpr-alx-exp       to   fil-alx-exp            .
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
           move      rf-vpr-num-prt       to   fil-num-prt            .
           move      rf-vpr-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-vpr                 .
           move      fil-num-prt          to   rf-vpr-num-prt         .
           move      fil-num-prg          to   rf-vpr-num-prg         .
           move      fil-tip-mag          to   rf-vpr-tip-mag         .
           move      fil-num-mag          to   rf-vpr-num-mag         .
           move      fil-alf-mag          to   rf-vpr-alf-mag         .
           move      fil-sgl-vrn          to   rf-vpr-sgl-vrn         .
           move      fil-umi-prd          to   rf-vpr-umi-prd         .
           move      fil-dec-qta          to   rf-vpr-dec-qta         .
           move      fil-qta-cns          to   rf-vpr-qta-cns         .
           move      fil-cod-dsl          to   rf-vpr-cod-dsl         .
           move      fil-snx-2qt          to   rf-vpr-snx-2qt         .
           move      fil-dec-2qt          to   rf-vpr-dec-2qt         .
           move      fil-qta-a02          to   rf-vpr-qta-a02         .
           move      fil-snx-3qt          to   rf-vpr-snx-3qt         .
           move      fil-dec-3qt          to   rf-vpr-dec-3qt         .
           move      fil-qta-a03          to   rf-vpr-qta-a03         .
           move      fil-flg-ela          to   rf-vpr-flg-ela         .
           move      fil-flg-pul          to   rf-vpr-flg-pul         .
           move      fil-alx-exp          to   rf-vpr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-vpr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-vpr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
