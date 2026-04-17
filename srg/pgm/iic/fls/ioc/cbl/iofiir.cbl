       Identification Division.
       Program-Id.                                 iofiir             .
      *================================================================*
      *                                                                *
      *                  Input-Output File iir                         *
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
      *            * Chiave numero 01 : DATREG                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-dat-reg        pic  9(07)       comp-3     .
                   15  fil-num-prt        pic  9(09)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-top-iic            pic  9(04)                  .
               10  fil-mes-rif            pic  9(02)                  .
               10  fil-tri-rif            pic  9(02)                  .
               10  fil-ann-rif            pic  9(03)       comp-3     .
               10  fil-aml-ope            pic  9(13)       comp-3     .
               10  fil-aml-pre            pic  9(13)       comp-3     .
               10  fil-avl-ope            pic  9(13)       comp-3     .
               10  fil-avl-pre            pic  9(13)       comp-3     .
               10  fil-cdn-cdm            pic  9(08)                  .
               10  fil-cnc-pre            pic  9(08)                  .
               10  fil-mas-net            pic  9(10)                  .
               10  fil-qen-ums            pic  9(10)                  .
               10  fil-vls-iml            pic  9(13)       comp-3     .
               10  fil-vls-pre            pic  9(13)       comp-3     .
               10  fil-iso-org            pic  x(03)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-tip-mag            pic  9(02)                  .
               10  fil-num-mag            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler  occurs 21  pic  x(01)                  .

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
                   15  pul-num-prt        pic  9(09)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-top-iic            pic  9(04)                  .
               10  pul-mes-rif            pic  9(02)                  .
               10  pul-tri-rif            pic  9(02)                  .
               10  pul-ann-rif            pic  9(03)       comp-3     .
               10  pul-aml-ope            pic  9(13)       comp-3     .
               10  pul-aml-pre            pic  9(13)       comp-3     .
               10  pul-avl-ope            pic  9(13)       comp-3     .
               10  pul-avl-pre            pic  9(13)       comp-3     .
               10  pul-cdn-cdm            pic  9(08)                  .
               10  pul-cnc-pre            pic  9(08)                  .
               10  pul-mas-net            pic  9(10)                  .
               10  pul-qen-ums            pic  9(10)                  .
               10  pul-vls-iml            pic  9(13)       comp-3     .
               10  pul-vls-pre            pic  9(13)       comp-3     .
               10  pul-iso-org            pic  x(03)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-tip-mag            pic  9(02)                  .
               10  pul-num-mag            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler  occurs 21  pic  x(01)                  .

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
                     "iir "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/iic/fls/ioc/obj/iofiir              "       .

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
                            "DATREG    "                              .
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
      *    * Record logico file [iir]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiir"                          .

      ******************************************************************
       Procedure Division                using f rf-iir               .
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
           move      spaces               to   rf-iir                 .
           move      zero                 to   rf-iir-dat-reg         .
           move      zero                 to   rf-iir-num-prt         .
           move      zero                 to   rf-iir-num-prg         .
           move      zero                 to   rf-iir-top-iic         .
           move      zero                 to   rf-iir-mes-rif         .
           move      zero                 to   rf-iir-tri-rif         .
           move      zero                 to   rf-iir-ann-rif         .
           move      zero                 to   rf-iir-aml-ope         .
           move      zero                 to   rf-iir-aml-pre         .
           move      zero                 to   rf-iir-avl-ope         .
           move      zero                 to   rf-iir-avl-pre         .
           move      zero                 to   rf-iir-cdn-cdm         .
           move      zero                 to   rf-iir-cnc-pre         .
           move      zero                 to   rf-iir-mas-net         .
           move      zero                 to   rf-iir-qen-ums         .
           move      zero                 to   rf-iir-vls-iml         .
           move      zero                 to   rf-iir-vls-pre         .
           move      spaces               to   rf-iir-iso-org         .
           move      spaces               to   rf-iir-flg-ela         .
           move      spaces               to   rf-iir-flg-pul         .
           move      zero                 to   rf-iir-tip-mag         .
           move      zero                 to   rf-iir-num-mag         .
           move      spaces               to   rf-iir-alx-exp         .
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
           move      rf-iir-top-iic       to   fil-top-iic            .
           move      rf-iir-mes-rif       to   fil-mes-rif            .
           move      rf-iir-tri-rif       to   fil-tri-rif            .
           move      rf-iir-ann-rif       to   fil-ann-rif            .
           move      rf-iir-aml-ope       to   fil-aml-ope            .
           move      rf-iir-aml-pre       to   fil-aml-pre            .
           move      rf-iir-avl-ope       to   fil-avl-ope            .
           move      rf-iir-avl-pre       to   fil-avl-pre            .
           move      rf-iir-cdn-cdm       to   fil-cdn-cdm            .
           move      rf-iir-cnc-pre       to   fil-cnc-pre            .
           move      rf-iir-mas-net       to   fil-mas-net            .
           move      rf-iir-qen-ums       to   fil-qen-ums            .
           move      rf-iir-vls-iml       to   fil-vls-iml            .
           move      rf-iir-vls-pre       to   fil-vls-pre            .
           move      rf-iir-iso-org       to   fil-iso-org            .
           move      rf-iir-flg-ela       to   fil-flg-ela            .
           move      rf-iir-flg-pul       to   fil-flg-pul            .
           move      rf-iir-tip-mag       to   fil-tip-mag            .
           move      rf-iir-num-mag       to   fil-num-mag            .
           move      rf-iir-alx-exp       to   fil-alx-exp            .
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
           move      rf-iir-dat-reg       to   fil-dat-reg            .
           move      rf-iir-num-prt       to   fil-num-prt            .
           move      rf-iir-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-iir                 .
           move      fil-dat-reg          to   rf-iir-dat-reg         .
           move      fil-num-prt          to   rf-iir-num-prt         .
           move      fil-num-prg          to   rf-iir-num-prg         .
           move      fil-top-iic          to   rf-iir-top-iic         .
           move      fil-mes-rif          to   rf-iir-mes-rif         .
           move      fil-tri-rif          to   rf-iir-tri-rif         .
           move      fil-ann-rif          to   rf-iir-ann-rif         .
           move      fil-aml-ope          to   rf-iir-aml-ope         .
           move      fil-aml-pre          to   rf-iir-aml-pre         .
           move      fil-avl-ope          to   rf-iir-avl-ope         .
           move      fil-avl-pre          to   rf-iir-avl-pre         .
           move      fil-cdn-cdm          to   rf-iir-cdn-cdm         .
           move      fil-cnc-pre          to   rf-iir-cnc-pre         .
           move      fil-mas-net          to   rf-iir-mas-net         .
           move      fil-qen-ums          to   rf-iir-qen-ums         .
           move      fil-vls-iml          to   rf-iir-vls-iml         .
           move      fil-vls-pre          to   rf-iir-vls-pre         .
           move      fil-iso-org          to   rf-iir-iso-org         .
           move      fil-flg-ela          to   rf-iir-flg-ela         .
           move      fil-flg-pul          to   rf-iir-flg-pul         .
           move      fil-tip-mag          to   rf-iir-tip-mag         .
           move      fil-num-mag          to   rf-iir-num-mag         .
           move      fil-alx-exp          to   rf-iir-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-iir               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-iir
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
