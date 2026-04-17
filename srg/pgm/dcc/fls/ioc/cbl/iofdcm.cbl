       Identification Division.
       Program-Id.                                 iofdcm             .
      *================================================================*
      *                                                                *
      *                  Input-Output File dcm                         *
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
                   alternate record key   is fil-k04
                   alternate record key   is fil-k05
                   alternate record key   is fil-k06
                   alternate record key   is fil-k07
                   alternate record key   is fil-k08
                   alternate record key   is fil-k09
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
                   alternate record key   is pul-k04
                   alternate record key   is pul-k05
                   alternate record key   is pul-k06
                   alternate record key   is pul-k07
                   alternate record key   is pul-k08
                   alternate record key   is pul-k09
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
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-cli        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-cod-cli-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-rag-key        pic  x(40)                  .
                   15  fil-cod-cli-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-mne        pic  x(10)                  .
                   15  fil-cod-cli-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PRTIVA                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-prt-iva        pic  9(11)       comp-3     .
                   15  fil-cod-cli-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CODFIS                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-fis        pic  x(16)                  .
                   15  fil-cod-cli-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CODSTT                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cod-stt        pic  9(07)       comp-3     .
                   15  fil-cod-cli-7      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : CODAFS                         *
      *            *---------------------------------------------------*
               10  fil-k08.
                   15  fil-cod-afs        pic  9(09)       comp-3     .
                   15  fil-cod-cli-8      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : CODCGE                         *
      *            *---------------------------------------------------*
               10  fil-k09.
                   15  fil-cod-cge        pic  9(07)       comp-3     .
                   15  fil-cod-cli-9      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-rs1-cli            pic  x(40)                  .
               10  fil-rs2-cli            pic  x(40)                  .
               10  fil-via-cli            pic  x(40)                  .
               10  fil-loc-cli            pic  x(40)                  .
               10  fil-cod-naz            pic  x(03)                  .
               10  fil-cod-cmn            pic  9(05)       comp-3     .
               10  fil-cod-fzn            pic  9(03)       comp-3     .
               10  fil-cod-lct            pic  9(03)       comp-3     .
               10  fil-num-tel            pic  x(20)                  .
               10  fil-num-fax            pic  x(20)                  .
               10  fil-iem-ail            pic  x(40)                  .
               10  fil-nom-int            pic  x(30)                  .
               10  fil-snx-att            pic  x(01)                  .
               10  fil-cat-sea            pic  x(06)                  .
               10  fil-alx-exp.
                   15  filler  occurs 194 pic  x(01)                  .

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
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-cli        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-cod-cli-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-rag-key        pic  x(40)                  .
                   15  pul-cod-cli-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-mne        pic  x(10)                  .
                   15  pul-cod-cli-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PRTIVA                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-prt-iva        pic  9(11)       comp-3     .
                   15  pul-cod-cli-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CODFIS                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-fis        pic  x(16)                  .
                   15  pul-cod-cli-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CODSTT                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-cod-stt        pic  9(07)       comp-3     .
                   15  pul-cod-cli-7      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : CODAFS                         *
      *            *---------------------------------------------------*
               10  pul-k08.
                   15  pul-cod-afs        pic  9(09)       comp-3     .
                   15  pul-cod-cli-8      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : CODCGE                         *
      *            *---------------------------------------------------*
               10  pul-k09.
                   15  pul-cod-cge        pic  9(07)       comp-3     .
                   15  pul-cod-cli-9      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-rs1-cli            pic  x(40)                  .
               10  pul-rs2-cli            pic  x(40)                  .
               10  pul-via-cli            pic  x(40)                  .
               10  pul-loc-cli            pic  x(40)                  .
               10  pul-cod-naz            pic  x(03)                  .
               10  pul-cod-cmn            pic  9(05)       comp-3     .
               10  pul-cod-fzn            pic  9(03)       comp-3     .
               10  pul-cod-lct            pic  9(03)       comp-3     .
               10  pul-num-tel            pic  x(20)                  .
               10  pul-num-fax            pic  x(20)                  .
               10  pul-iem-ail            pic  x(40)                  .
               10  pul-nom-int            pic  x(30)                  .
               10  pul-snx-att            pic  x(01)                  .
               10  pul-cat-sea            pic  x(06)                  .
               10  pul-alx-exp.
                   15  filler  occurs 194 pic  x(01)                  .

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
                     "dcm "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcc/fls/ioc/obj/iofdcm              "       .

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
           05  k-ctr                      pic  9(02) value 9          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODCLI"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RAGKEY"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODMNE"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PRTIVA"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODFIS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODSTT"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 8                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODAFS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 9                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODCGE"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    9      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [dcm]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcm"                          .

      ******************************************************************
       Procedure Division                using f rf-dcm               .
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
                     str-400
                     str-500
                     str-600
                     str-700
                     str-800
                     str-900
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
       str-400.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 4                       *
      *              *-------------------------------------------------*
           go to     str-410
                     str-420
                     str-430
                     depending            on   z-tco                  .
       str-410.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-420.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-430.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-500.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 5                       *
      *              *-------------------------------------------------*
           go to     str-510
                     str-520
                     str-530
                     depending            on   z-tco                  .
       str-510.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-520.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-530.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-600.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 6                       *
      *              *-------------------------------------------------*
           go to     str-610
                     str-620
                     str-630
                     depending            on   z-tco                  .
       str-610.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-620.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-630.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-700.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 7                       *
      *              *-------------------------------------------------*
           go to     str-710
                     str-720
                     str-730
                     depending            on   z-tco                  .
       str-710.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 7           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k07
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-720.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 7           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k07
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-730.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 7           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k07
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-800.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 8                       *
      *              *-------------------------------------------------*
           go to     str-810
                     str-820
                     str-830
                     depending            on   z-tco                  .
       str-810.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-820.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-830.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-900.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 9                       *
      *              *-------------------------------------------------*
           go to     str-910
                     str-920
                     str-930
                     depending            on   z-tco                  .
       str-910.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 9           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k09
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-920.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 9           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k09
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-930.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 9           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k09
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
                     rea-400
                     rea-500
                     rea-600
                     rea-700
                     rea-800
                     rea-900
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
       rea-400.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 4                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-410.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k04
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-410.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k04
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-500.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 5                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-510.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k05
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-510.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k05
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-600.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 6                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-610.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k06
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-610.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k06
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-700.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 7                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-710.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k07
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-710.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k07
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-800.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 8                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-810.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k08
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-810.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k08
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-900.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 9                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-910.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k09
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-910.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k09
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
           move      spaces               to   rf-dcm                 .
           move      zero                 to   rf-dcm-ide-dat         .
           move      spaces               to   rf-dcm-ide-ute         .
           move      spaces               to   rf-dcm-ide-fas         .
           move      zero                 to   rf-dcm-cod-cli         .
           move      spaces               to   rf-dcm-cod-mne         .
           move      spaces               to   rf-dcm-rag-key         .
           move      spaces               to   rf-dcm-rs1-cli         .
           move      spaces               to   rf-dcm-rs2-cli         .
           move      spaces               to   rf-dcm-via-cli         .
           move      spaces               to   rf-dcm-loc-cli         .
           move      spaces               to   rf-dcm-cod-naz         .
           move      zero                 to   rf-dcm-cod-cmn         .
           move      zero                 to   rf-dcm-cod-fzn         .
           move      zero                 to   rf-dcm-cod-lct         .
           move      spaces               to   rf-dcm-num-tel         .
           move      spaces               to   rf-dcm-num-fax         .
           move      spaces               to   rf-dcm-iem-ail         .
           move      spaces               to   rf-dcm-nom-int         .
           move      zero                 to   rf-dcm-cod-stt         .
           move      zero                 to   rf-dcm-prt-iva         .
           move      spaces               to   rf-dcm-cod-fis         .
           move      spaces               to   rf-dcm-snx-att         .
           move      zero                 to   rf-dcm-cod-cge         .
           move      zero                 to   rf-dcm-cod-afs         .
           move      spaces               to   rf-dcm-cat-sea         .
           move      spaces               to   rf-dcm-alx-exp         .
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
           move      rf-dcm-ide-ute       to   fil-ide-ute            .
           move      rf-dcm-ide-fas       to   fil-ide-fas            .
           move      rf-dcm-rs1-cli       to   fil-rs1-cli            .
           move      rf-dcm-rs2-cli       to   fil-rs2-cli            .
           move      rf-dcm-via-cli       to   fil-via-cli            .
           move      rf-dcm-loc-cli       to   fil-loc-cli            .
           move      rf-dcm-cod-naz       to   fil-cod-naz            .
           move      rf-dcm-cod-cmn       to   fil-cod-cmn            .
           move      rf-dcm-cod-fzn       to   fil-cod-fzn            .
           move      rf-dcm-cod-lct       to   fil-cod-lct            .
           move      rf-dcm-num-tel       to   fil-num-tel            .
           move      rf-dcm-num-fax       to   fil-num-fax            .
           move      rf-dcm-iem-ail       to   fil-iem-ail            .
           move      rf-dcm-nom-int       to   fil-nom-int            .
           move      rf-dcm-snx-att       to   fil-snx-att            .
           move      rf-dcm-cat-sea       to   fil-cat-sea            .
           move      rf-dcm-alx-exp       to   fil-alx-exp            .
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
                     cmp-key-fis-400
                     cmp-key-fis-500
                     cmp-key-fis-600
                     cmp-key-fis-700
                     cmp-key-fis-800
                     cmp-key-fis-900
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-dcm-cod-cli       to   fil-cod-cli            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-dcm-ide-dat       to   fil-ide-dat            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-dcm-rag-key       to   fil-rag-key            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-dcm-cod-mne       to   fil-cod-mne            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-dcm-prt-iva       to   fil-prt-iva            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-dcm-cod-fis       to   fil-cod-fis            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-dcm-cod-stt       to   fil-cod-stt            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-800.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 8                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k08                .
           move      rf-dcm-cod-afs       to   fil-cod-afs            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-8          .
           go to     cmp-key-fis-999.
       cmp-key-fis-900.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 9                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k09                .
           move      rf-dcm-cod-cge       to   fil-cod-cge            .
           move      rf-dcm-cod-cli       to   fil-cod-cli-9          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-dcm                 .
           move      fil-ide-dat          to   rf-dcm-ide-dat         .
           move      fil-ide-ute          to   rf-dcm-ide-ute         .
           move      fil-ide-fas          to   rf-dcm-ide-fas         .
           move      fil-cod-cli          to   rf-dcm-cod-cli         .
           move      fil-cod-mne          to   rf-dcm-cod-mne         .
           move      fil-rag-key          to   rf-dcm-rag-key         .
           move      fil-rs1-cli          to   rf-dcm-rs1-cli         .
           move      fil-rs2-cli          to   rf-dcm-rs2-cli         .
           move      fil-via-cli          to   rf-dcm-via-cli         .
           move      fil-loc-cli          to   rf-dcm-loc-cli         .
           move      fil-cod-naz          to   rf-dcm-cod-naz         .
           move      fil-cod-cmn          to   rf-dcm-cod-cmn         .
           move      fil-cod-fzn          to   rf-dcm-cod-fzn         .
           move      fil-cod-lct          to   rf-dcm-cod-lct         .
           move      fil-num-tel          to   rf-dcm-num-tel         .
           move      fil-num-fax          to   rf-dcm-num-fax         .
           move      fil-iem-ail          to   rf-dcm-iem-ail         .
           move      fil-nom-int          to   rf-dcm-nom-int         .
           move      fil-cod-stt          to   rf-dcm-cod-stt         .
           move      fil-prt-iva          to   rf-dcm-prt-iva         .
           move      fil-cod-fis          to   rf-dcm-cod-fis         .
           move      fil-snx-att          to   rf-dcm-snx-att         .
           move      fil-cod-cge          to   rf-dcm-cod-cge         .
           move      fil-cod-afs          to   rf-dcm-cod-afs         .
           move      fil-cat-sea          to   rf-dcm-cat-sea         .
           move      fil-alx-exp          to   rf-dcm-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-dcm               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-dcm
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

