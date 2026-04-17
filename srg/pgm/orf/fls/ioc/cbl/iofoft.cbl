       Identification Division.
       Program-Id.                                 iofoft             .
      *================================================================*
      *                                                                *
      *                  Input-Output File oft                         *
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
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-doc-3      pic  9(07)       comp-3     .
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-num-doc        pic  9(11)       comp-3     .
                   15  fil-tmo-orf        pic  x(05)                  .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-scl-ann        pic  9(03)       comp-3     .
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-sgl-num        pic  x(03)                  .
                   15  fil-num-doc-4      pic  9(11)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-dat-doc-5      pic  9(07)       comp-3     .
                   15  fil-num-doc-5      pic  9(11)       comp-3     .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMODAT                      *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-dpz-6      pic  9(02)                  .
                   15  fil-tmo-orf-6      pic  x(05)                  .
                   15  fil-dat-doc-6      pic  9(07)       comp-3     .
                   15  fil-num-doc-6      pic  9(11)       comp-3     .
                   15  fil-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZOCH                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cod-dpz-7      pic  9(02)                  .
                   15  fil-flg-och        pic  x(01)                  .
                   15  fil-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-arc-plf            pic  9(07)       comp-3     .
               10  fil-dpz-plf            pic  x(04)                  .
               10  fil-tip-ids            pic  9(02)                  .
               10  fil-cof-dat            pic  9(07)       comp-3     .
               10  fil-cof-num            pic  x(10)                  .
               10  fil-cof-rif            pic  x(40)                  .
               10  fil-cod-rsp            pic  9(05)       comp-3     .
               10  fil-dat-cns            pic  9(07)       comp-3     .
               10  fil-fds-dtc            pic  9(02)                  .
               10  fil-tip-eva            pic  9(02)                  .
               10  fil-pri-eva            pic  x(02)                  .
               10  fil-cod-cda            pic  9(03)       comp-3     .
               10  fil-com-int.
                   15  fil-com-rig occurs 03
                                          pic  x(40)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-ass-iva            pic  9(05)       comp-3     .
               10  fil-ctp-acq            pic  9(07)       comp-3     .
               10  fil-inl-dcm            pic  9(02)                  .
               10  fil-inl-pgt            pic  9(02)                  .
               10  fil-cod-lst            pic  x(03)                  .
               10  fil-csr-aaf            pic  9(05)       comp-3     .
               10  fil-psr-aaf occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-csc-aaf            pic  9(05)       comp-3     .
               10  fil-psc-aaf            pic  9(02)v9(01) comp-3     .
               10  fil-voc-des occurs 06  pic  x(03)                  .
               10  fil-cod-fop            pic  9(07)       comp-3     .
               10  fil-scp-aap            pic  9(02)v9(01) comp-3     .
               10  fil-nos-ban            pic  x(10)                  .
               10  fil-cod-abi            pic  9(05)       comp-3     .
               10  fil-cod-cab            pic  9(05)       comp-3     .
               10  fil-ccc-app            pic  x(12)                  .
               10  fil-ccp-app            pic  x(12)                  .
               10  fil-add-spi            pic  x(03)                  .
               10  fil-add-spb            pic  x(03)                  .
               10  fil-ipr-iel            pic  9(02)                  .
               10  fil-pag-dsm            pic  9(07)       comp-3     .
               10  fil-pag-qaf            pic  9(09)       comp-3     .
               10  fil-pag-act            pic  9(09)       comp-3     .
               10  fil-cod-aqt            pic  9(07)       comp-3     .
               10  fil-pvf-aqt            pic  9(11)       comp-3     .
               10  fil-cod-ime            pic  9(07)       comp-3     .
               10  fil-pvf-ime            pic  9(11)       comp-3     .
               10  fil-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  fil-tot-scc            pic s9(11)       comp-3     .
               10  fil-per-scc            pic  9(02)v9(01) comp-3     .
               10  fil-tot-scp            pic s9(11)       comp-3     .
               10  fil-per-scp            pic  9(02)v9(01) comp-3     .
               10  fil-spe-add occurs 06.
                   15  fil-spe-snx        pic  9(01)                  .
                   15  fil-spe-mad        pic  9(01)                  .
                   15  fil-spe-per        pic  9(02)v9(01) comp-3     .
                   15  fil-spe-ibl        pic  9(02)                  .
                   15  fil-ibt-spe.
                       20  fil-ibx-spe  occurs 09
                                          pic  x(01)                  .
                   15  fil-spe-imp        pic s9(09)       comp-3     .
               10  fil-tot-doc            pic s9(11)       comp-3     .
               10  fil-ctr-stp            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-dpz-ids            pic  9(02)                  .
               10  fil-tip-ord            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 17  pic  x(01)                  .

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
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-doc-3      pic  9(07)       comp-3     .
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-num-doc        pic  9(11)       comp-3     .
                   15  pul-tmo-orf        pic  x(05)                  .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-scl-ann        pic  9(03)       comp-3     .
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-sgl-num        pic  x(03)                  .
                   15  pul-num-doc-4      pic  9(11)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-dat-doc-5      pic  9(07)       comp-3     .
                   15  pul-num-doc-5      pic  9(11)       comp-3     .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMODAT                      *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-dpz-6      pic  9(02)                  .
                   15  pul-tmo-orf-6      pic  x(05)                  .
                   15  pul-dat-doc-6      pic  9(07)       comp-3     .
                   15  pul-num-doc-6      pic  9(11)       comp-3     .
                   15  pul-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZOCH                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-cod-dpz-7      pic  9(02)                  .
                   15  pul-flg-och        pic  x(01)                  .
                   15  pul-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-arc-plf            pic  9(07)       comp-3     .
               10  pul-dpz-plf            pic  x(04)                  .
               10  pul-tip-ids            pic  9(02)                  .
               10  pul-cof-dat            pic  9(07)       comp-3     .
               10  pul-cof-num            pic  x(10)                  .
               10  pul-cof-rif            pic  x(40)                  .
               10  pul-cod-rsp            pic  9(05)       comp-3     .
               10  pul-dat-cns            pic  9(07)       comp-3     .
               10  pul-fds-dtc            pic  9(02)                  .
               10  pul-tip-eva            pic  9(02)                  .
               10  pul-pri-eva            pic  x(02)                  .
               10  pul-cod-cda            pic  9(03)       comp-3     .
               10  pul-com-int.
                   15  pul-com-rig occurs 03
                                          pic  x(40)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-ass-iva            pic  9(05)       comp-3     .
               10  pul-ctp-acq            pic  9(07)       comp-3     .
               10  pul-inl-dcm            pic  9(02)                  .
               10  pul-inl-pgt            pic  9(02)                  .
               10  pul-cod-lst            pic  x(03)                  .
               10  pul-csr-aaf            pic  9(05)       comp-3     .
               10  pul-psr-aaf occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-csc-aaf            pic  9(05)       comp-3     .
               10  pul-psc-aaf            pic  9(02)v9(01) comp-3     .
               10  pul-voc-des occurs 06  pic  x(03)                  .
               10  pul-cod-fop            pic  9(07)       comp-3     .
               10  pul-scp-aap            pic  9(02)v9(01) comp-3     .
               10  pul-nos-ban            pic  x(10)                  .
               10  pul-cod-abi            pic  9(05)       comp-3     .
               10  pul-cod-cab            pic  9(05)       comp-3     .
               10  pul-ccc-app            pic  x(12)                  .
               10  pul-ccp-app            pic  x(12)                  .
               10  pul-add-spi            pic  x(03)                  .
               10  pul-add-spb            pic  x(03)                  .
               10  pul-ipr-iel            pic  9(02)                  .
               10  pul-pag-dsm            pic  9(07)       comp-3     .
               10  pul-pag-qaf            pic  9(09)       comp-3     .
               10  pul-pag-act            pic  9(09)       comp-3     .
               10  pul-cod-aqt            pic  9(07)       comp-3     .
               10  pul-pvf-aqt            pic  9(11)       comp-3     .
               10  pul-cod-ime            pic  9(07)       comp-3     .
               10  pul-pvf-ime            pic  9(11)       comp-3     .
               10  pul-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  pul-tot-scc            pic s9(11)       comp-3     .
               10  pul-per-scc            pic  9(02)v9(01) comp-3     .
               10  pul-tot-scp            pic s9(11)       comp-3     .
               10  pul-per-scp            pic  9(02)v9(01) comp-3     .
               10  pul-spe-add occurs 06.
                   15  pul-spe-snx        pic  9(01)                  .
                   15  pul-spe-mad        pic  9(01)                  .
                   15  pul-spe-per        pic  9(02)v9(01) comp-3     .
                   15  pul-spe-ibl        pic  9(02)                  .
                   15  pul-ibt-spe.
                       20  pul-ibx-spe  occurs 09
                                          pic  x(01)                  .
                   15  pul-spe-imp        pic s9(09)       comp-3     .
               10  pul-tot-doc            pic s9(11)       comp-3     .
               10  pul-ctr-stp            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-dpz-ids            pic  9(02)                  .
               10  pul-tip-ord            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 17  pic  x(01)                  .

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
                     "oft "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/orf/fls/ioc/obj/iofoft              "       .

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
           05  k-ctr                      pic  9(02) value 7          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMPRT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "IDEDOC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CNTDEN    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZARCDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZTMODAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZOCH    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    7      pic  x(10)                  .

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
      *    * Record logico file [oft]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .

      ******************************************************************
       Procedure Division                using f rf-oft               .
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
           move      spaces               to   rf-oft                 .
           move      zero                 to   rf-oft-ide-dat         .
           move      spaces               to   rf-oft-ide-ute         .
           move      spaces               to   rf-oft-ide-fas         .
           move      zero                 to   rf-oft-num-prt         .
           move      spaces               to   rf-oft-tmo-orf         .
           move      zero                 to   rf-oft-cod-dpz         .
           move      zero                 to   rf-oft-dat-doc         .
           move      zero                 to   rf-oft-num-doc         .
           move      zero                 to   rf-oft-scl-ann         .
           move      spaces               to   rf-oft-sgl-num         .
           move      spaces               to   rf-oft-tip-arc         .
           move      zero                 to   rf-oft-cod-arc         .
           move      spaces               to   rf-oft-dpz-arc         .
           move      zero                 to   rf-oft-arc-plf         .
           move      spaces               to   rf-oft-dpz-plf         .
           move      zero                 to   rf-oft-tip-ids         .
           move      zero                 to   rf-oft-cof-dat         .
           move      spaces               to   rf-oft-cof-num         .
           move      spaces               to   rf-oft-cof-rif         .
           move      zero                 to   rf-oft-cod-rsp         .
           move      zero                 to   rf-oft-dat-cns         .
           move      zero                 to   rf-oft-fds-dtc         .
           move      zero                 to   rf-oft-tip-eva         .
           move      zero                 to   rf-oft-pri-eva         .
           move      zero                 to   rf-oft-cod-cda         .
           move      spaces               to   rf-oft-com-int         .
           move      spaces               to   rf-oft-cod-lng         .
           move      spaces               to   rf-oft-sgl-vpf         .
           move      zero                 to   rf-oft-dec-vpf         .
           move      spaces               to   rf-oft-tdc-vpf         .
           move      zero                 to   rf-oft-cdc-vpf         .
           move      zero                 to   rf-oft-ass-iva         .
           move      zero                 to   rf-oft-ctp-acq         .
           move      zero                 to   rf-oft-inl-dcm         .
           move      zero                 to   rf-oft-inl-pgt         .
           move      spaces               to   rf-oft-cod-lst         .
           move      zero                 to   rf-oft-csr-aaf         .
           move      zero                 to   rf-oft-psr-aaf (1)     .
           move      zero                 to   rf-oft-psr-aaf (2)     .
           move      zero                 to   rf-oft-psr-aaf (3)     .
           move      zero                 to   rf-oft-psr-aaf (4)     .
           move      zero                 to   rf-oft-psr-aaf (5)     .
           move      zero                 to   rf-oft-csc-aaf         .
           move      zero                 to   rf-oft-psc-aaf         .
           move      spaces               to   rf-oft-voc-des (1)     .
           move      spaces               to   rf-oft-voc-des (2)     .
           move      spaces               to   rf-oft-voc-des (3)     .
           move      spaces               to   rf-oft-voc-des (4)     .
           move      spaces               to   rf-oft-voc-des (5)     .
           move      spaces               to   rf-oft-voc-des (6)     .
           move      zero                 to   rf-oft-cod-fop         .
           move      zero                 to   rf-oft-scp-aap         .
           move      spaces               to   rf-oft-nos-ban         .
           move      zero                 to   rf-oft-cod-abi         .
           move      zero                 to   rf-oft-cod-cab         .
           move      spaces               to   rf-oft-ccc-app         .
           move      spaces               to   rf-oft-ccp-app         .
           move      spaces               to   rf-oft-add-spi         .
           move      spaces               to   rf-oft-add-spb         .
           move      zero                 to   rf-oft-ipr-iel         .
           move      zero                 to   rf-oft-pag-dsm         .
           move      zero                 to   rf-oft-pag-qaf         .
           move      zero                 to   rf-oft-pag-act         .
           move      zero                 to   rf-oft-cod-aqt         .
           move      zero                 to   rf-oft-pvf-aqt         .
           move      zero                 to   rf-oft-cod-ime         .
           move      zero                 to   rf-oft-pvf-ime         .
           move      zero                 to   rf-oft-tot-rig (1)     .
           move      zero                 to   rf-oft-tot-rig (2)     .
           move      zero                 to   rf-oft-tot-rig (3)     .
           move      zero                 to   rf-oft-tot-rig (4)     .
           move      zero                 to   rf-oft-tot-rig (5)     .
           move      zero                 to   rf-oft-tot-rig (6)     .
           move      zero                 to   rf-oft-tot-rig (7)     .
           move      zero                 to   rf-oft-tot-rig (8)     .
           move      zero                 to   rf-oft-tot-rig (9)     .
           move      zero                 to   rf-oft-tot-scc         .
           move      zero                 to   rf-oft-per-scc         .
           move      zero                 to   rf-oft-tot-scp         .
           move      zero                 to   rf-oft-per-scp         .
           move      zero                 to   w-c01                  .
       nor-rec-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-240.
           move      zero                 to   rf-oft-spe-snx (w-c01) .
           move      zero                 to   rf-oft-spe-mad (w-c01) .
           move      zero                 to   rf-oft-spe-per (w-c01) .
           move      zero                 to   rf-oft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       nor-rec-log-200.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to nor-rec-log-220.
           move      spaces               to   rf-oft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     nor-rec-log-200.
       nor-rec-log-220.
           move      zero                 to   rf-oft-spe-imp (w-c01) .
           go to     nor-rec-log-100.
       nor-rec-log-240.
           move      zero                 to   rf-oft-tot-doc         .
           move      zero                 to   rf-oft-ctr-stp         .
           move      spaces               to   rf-oft-flg-och         .
           move      spaces               to   rf-oft-flg-blo         .
           move      spaces               to   rf-oft-flg-nbl         .
           move      spaces               to   rf-oft-flg-pul         .
           move      zero                 to   rf-oft-dpz-ids         .
           move      spaces               to   rf-oft-tip-ord         .
           move      spaces               to   rf-oft-alx-exp         .
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
           move      rf-oft-ide-ute       to   fil-ide-ute            .
           move      rf-oft-ide-fas       to   fil-ide-fas            .
           move      rf-oft-dpz-arc       to   fil-dpz-arc            .
           move      rf-oft-arc-plf       to   fil-arc-plf            .
           move      rf-oft-dpz-plf       to   fil-dpz-plf            .
           move      rf-oft-tip-ids       to   fil-tip-ids            .
           move      rf-oft-cof-dat       to   fil-cof-dat            .
           move      rf-oft-cof-num       to   fil-cof-num            .
           move      rf-oft-cof-rif       to   fil-cof-rif            .
           move      rf-oft-cod-rsp       to   fil-cod-rsp            .
           move      rf-oft-dat-cns       to   fil-dat-cns            .
           move      rf-oft-fds-dtc       to   fil-fds-dtc            .
           move      rf-oft-tip-eva       to   fil-tip-eva            .
           move      rf-oft-pri-eva       to   fil-pri-eva            .
           move      rf-oft-cod-cda       to   fil-cod-cda            .
           move      rf-oft-com-int       to   fil-com-int            .
           move      rf-oft-cod-lng       to   fil-cod-lng            .
           move      rf-oft-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-oft-dec-vpf       to   fil-dec-vpf            .
           move      rf-oft-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-oft-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-oft-ass-iva       to   fil-ass-iva            .
           move      rf-oft-ctp-acq       to   fil-ctp-acq            .
           move      rf-oft-inl-dcm       to   fil-inl-dcm            .
           move      rf-oft-inl-pgt       to   fil-inl-pgt            .
           move      rf-oft-cod-lst       to   fil-cod-lst            .
           move      rf-oft-csr-aaf       to   fil-csr-aaf            .
           move      rf-oft-psr-aaf (1)   to   fil-psr-aaf (1)        .
           move      rf-oft-psr-aaf (2)   to   fil-psr-aaf (2)        .
           move      rf-oft-psr-aaf (3)   to   fil-psr-aaf (3)        .
           move      rf-oft-psr-aaf (4)   to   fil-psr-aaf (4)        .
           move      rf-oft-psr-aaf (5)   to   fil-psr-aaf (5)        .
           move      rf-oft-csc-aaf       to   fil-csc-aaf            .
           move      rf-oft-psc-aaf       to   fil-psc-aaf            .
           move      rf-oft-voc-des (1)   to   fil-voc-des (1)        .
           move      rf-oft-voc-des (2)   to   fil-voc-des (2)        .
           move      rf-oft-voc-des (3)   to   fil-voc-des (3)        .
           move      rf-oft-voc-des (4)   to   fil-voc-des (4)        .
           move      rf-oft-voc-des (5)   to   fil-voc-des (5)        .
           move      rf-oft-voc-des (6)   to   fil-voc-des (6)        .
           move      rf-oft-cod-fop       to   fil-cod-fop            .
           move      rf-oft-scp-aap       to   fil-scp-aap            .
           move      rf-oft-nos-ban       to   fil-nos-ban            .
           move      rf-oft-cod-abi       to   fil-cod-abi            .
           move      rf-oft-cod-cab       to   fil-cod-cab            .
           move      rf-oft-ccc-app       to   fil-ccc-app            .
           move      rf-oft-ccp-app       to   fil-ccp-app            .
           move      rf-oft-add-spi       to   fil-add-spi            .
           move      rf-oft-add-spb       to   fil-add-spb            .
           move      rf-oft-ipr-iel       to   fil-ipr-iel            .
           move      rf-oft-pag-dsm       to   fil-pag-dsm            .
           move      rf-oft-pag-qaf       to   fil-pag-qaf            .
           move      rf-oft-pag-act       to   fil-pag-act            .
           move      rf-oft-cod-aqt       to   fil-cod-aqt            .
           move      rf-oft-pvf-aqt       to   fil-pvf-aqt            .
           move      rf-oft-cod-ime       to   fil-cod-ime            .
           move      rf-oft-pvf-ime       to   fil-pvf-ime            .
           move      rf-oft-tot-rig (1)   to   fil-tot-rig (1)        .
           move      rf-oft-tot-rig (2)   to   fil-tot-rig (2)        .
           move      rf-oft-tot-rig (3)   to   fil-tot-rig (3)        .
           move      rf-oft-tot-rig (4)   to   fil-tot-rig (4)        .
           move      rf-oft-tot-rig (5)   to   fil-tot-rig (5)        .
           move      rf-oft-tot-rig (6)   to   fil-tot-rig (6)        .
           move      rf-oft-tot-rig (7)   to   fil-tot-rig (7)        .
           move      rf-oft-tot-rig (8)   to   fil-tot-rig (8)        .
           move      rf-oft-tot-rig (9)   to   fil-tot-rig (9)        .
           move      rf-oft-tot-scc       to   fil-tot-scc            .
           move      rf-oft-per-scc       to   fil-per-scc            .
           move      rf-oft-tot-scp       to   fil-tot-scp            .
           move      rf-oft-per-scp       to   fil-per-scp            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-340.
           move      rf-oft-spe-snx (w-c01)
                                          to   fil-spe-snx (w-c01)    .
           move      rf-oft-spe-mad (w-c01)
                                          to   fil-spe-mad (w-c01)    .
           move      rf-oft-spe-per (w-c01)
                                          to   fil-spe-per (w-c01)    .
           move      rf-oft-spe-ibl (w-c01)
                                          to   fil-spe-ibl (w-c01)    .
           move      zero                 to   w-c02                  .
       cmp-log-fis-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to cmp-log-fis-320.
           move      rf-oft-ibx-spe
                     (w-c01, w-c02)       to   fil-ibx-spe
                                              (w-c01, w-c02)          .
           go to     cmp-log-fis-300.
       cmp-log-fis-320.
           move      rf-oft-spe-imp (w-c01)
                                          to   fil-spe-imp (w-c01)    .
           go to     cmp-log-fis-200.
       cmp-log-fis-340.
           move      rf-oft-tot-doc       to   fil-tot-doc            .
           move      rf-oft-ctr-stp       to   fil-ctr-stp            .
           move      rf-oft-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-oft-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-oft-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-oft-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-oft-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-oft-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-oft-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-oft-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-oft-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-oft-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-oft-flg-pul       to   fil-flg-pul            .
           move      rf-oft-dpz-ids       to   fil-dpz-ids            .
           move      rf-oft-tip-ord       to   fil-tip-ord            .
           move      rf-oft-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-oft-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-oft-ide-dat       to   fil-ide-dat            .
           move      rf-oft-dat-doc       to   fil-dat-doc            .
           move      rf-oft-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-oft-cod-dpz       to   fil-cod-dpz            .
           move      rf-oft-tmo-orf       to   fil-tmo-orf            .
           move      rf-oft-dat-doc       to   fil-dat-doc-3          .
           move      rf-oft-num-doc       to   fil-num-doc            .
           move      rf-oft-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-oft-scl-ann       to   fil-scl-ann            .
           move      rf-oft-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-oft-sgl-num       to   fil-sgl-num            .
           move      rf-oft-num-doc       to   fil-num-doc-4          .
           move      rf-oft-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-oft-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-oft-tip-arc       to   fil-tip-arc            .
           move      rf-oft-cod-arc       to   fil-cod-arc            .
           move      rf-oft-dat-doc       to   fil-dat-doc-5          .
           move      rf-oft-num-doc       to   fil-num-doc-5          .
           move      rf-oft-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-oft-cod-dpz       to   fil-cod-dpz-6          .
           move      rf-oft-tmo-orf       to   fil-tmo-orf-6          .
           move      rf-oft-dat-doc       to   fil-dat-doc-6          .
           move      rf-oft-num-doc       to   fil-num-doc-6          .
           move      rf-oft-num-prt       to   fil-num-prt-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-oft-cod-dpz       to   fil-cod-dpz-7          .
           move      rf-oft-flg-och       to   fil-flg-och            .
           move      rf-oft-num-prt       to   fil-num-prt-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-oft                 .
           move      fil-ide-dat          to   rf-oft-ide-dat         .
           move      fil-ide-ute          to   rf-oft-ide-ute         .
           move      fil-ide-fas          to   rf-oft-ide-fas         .
           move      fil-num-prt          to   rf-oft-num-prt         .
           move      fil-tmo-orf          to   rf-oft-tmo-orf         .
           move      fil-cod-dpz          to   rf-oft-cod-dpz         .
           move      fil-dat-doc          to   rf-oft-dat-doc         .
           move      fil-num-doc          to   rf-oft-num-doc         .
           move      fil-scl-ann          to   rf-oft-scl-ann         .
           move      fil-sgl-num          to   rf-oft-sgl-num         .
           move      fil-tip-arc          to   rf-oft-tip-arc         .
           move      fil-cod-arc          to   rf-oft-cod-arc         .
           move      fil-dpz-arc          to   rf-oft-dpz-arc         .
           move      fil-arc-plf          to   rf-oft-arc-plf         .
           move      fil-dpz-plf          to   rf-oft-dpz-plf         .
           move      fil-tip-ids          to   rf-oft-tip-ids         .
           move      fil-cof-dat          to   rf-oft-cof-dat         .
           move      fil-cof-num          to   rf-oft-cof-num         .
           move      fil-cof-rif          to   rf-oft-cof-rif         .
           move      fil-cod-rsp          to   rf-oft-cod-rsp         .
           move      fil-dat-cns          to   rf-oft-dat-cns         .
           move      fil-fds-dtc          to   rf-oft-fds-dtc         .
           move      fil-tip-eva          to   rf-oft-tip-eva         .
           move      fil-pri-eva          to   rf-oft-pri-eva         .
           move      fil-cod-cda          to   rf-oft-cod-cda         .
           move      fil-com-int          to   rf-oft-com-int         .
           move      fil-cod-lng          to   rf-oft-cod-lng         .
           move      fil-sgl-vpf          to   rf-oft-sgl-vpf         .
           move      fil-dec-vpf          to   rf-oft-dec-vpf         .
           move      fil-tdc-vpf          to   rf-oft-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-oft-cdc-vpf         .
           move      fil-ass-iva          to   rf-oft-ass-iva         .
           move      fil-ctp-acq          to   rf-oft-ctp-acq         .
           move      fil-inl-dcm          to   rf-oft-inl-dcm         .
           move      fil-inl-pgt          to   rf-oft-inl-pgt         .
           move      fil-cod-lst          to   rf-oft-cod-lst         .
           move      fil-csr-aaf          to   rf-oft-csr-aaf         .
           move      fil-psr-aaf (1)      to   rf-oft-psr-aaf (1)     .
           move      fil-psr-aaf (2)      to   rf-oft-psr-aaf (2)     .
           move      fil-psr-aaf (3)      to   rf-oft-psr-aaf (3)     .
           move      fil-psr-aaf (4)      to   rf-oft-psr-aaf (4)     .
           move      fil-psr-aaf (5)      to   rf-oft-psr-aaf (5)     .
           move      fil-csc-aaf          to   rf-oft-csc-aaf         .
           move      fil-psc-aaf          to   rf-oft-psc-aaf         .
           move      fil-voc-des (1)      to   rf-oft-voc-des (1)     .
           move      fil-voc-des (2)      to   rf-oft-voc-des (2)     .
           move      fil-voc-des (3)      to   rf-oft-voc-des (3)     .
           move      fil-voc-des (4)      to   rf-oft-voc-des (4)     .
           move      fil-voc-des (5)      to   rf-oft-voc-des (5)     .
           move      fil-voc-des (6)      to   rf-oft-voc-des (6)     .
           move      fil-cod-fop          to   rf-oft-cod-fop         .
           move      fil-scp-aap          to   rf-oft-scp-aap         .
           move      fil-nos-ban          to   rf-oft-nos-ban         .
           move      fil-cod-abi          to   rf-oft-cod-abi         .
           move      fil-cod-cab          to   rf-oft-cod-cab         .
           move      fil-ccc-app          to   rf-oft-ccc-app         .
           move      fil-ccp-app          to   rf-oft-ccp-app         .
           move      fil-add-spi          to   rf-oft-add-spi         .
           move      fil-add-spb          to   rf-oft-add-spb         .
           move      fil-ipr-iel          to   rf-oft-ipr-iel         .
           move      fil-pag-dsm          to   rf-oft-pag-dsm         .
           move      fil-pag-qaf          to   rf-oft-pag-qaf         .
           move      fil-pag-act          to   rf-oft-pag-act         .
           move      fil-cod-aqt          to   rf-oft-cod-aqt         .
           move      fil-pvf-aqt          to   rf-oft-pvf-aqt         .
           move      fil-cod-ime          to   rf-oft-cod-ime         .
           move      fil-pvf-ime          to   rf-oft-pvf-ime         .
           move      fil-tot-rig (1)      to   rf-oft-tot-rig (1)     .
           move      fil-tot-rig (2)      to   rf-oft-tot-rig (2)     .
           move      fil-tot-rig (3)      to   rf-oft-tot-rig (3)     .
           move      fil-tot-rig (4)      to   rf-oft-tot-rig (4)     .
           move      fil-tot-rig (5)      to   rf-oft-tot-rig (5)     .
           move      fil-tot-rig (6)      to   rf-oft-tot-rig (6)     .
           move      fil-tot-rig (7)      to   rf-oft-tot-rig (7)     .
           move      fil-tot-rig (8)      to   rf-oft-tot-rig (8)     .
           move      fil-tot-rig (9)      to   rf-oft-tot-rig (9)     .
           move      fil-tot-scc          to   rf-oft-tot-scc         .
           move      fil-per-scc          to   rf-oft-per-scc         .
           move      fil-tot-scp          to   rf-oft-tot-scp         .
           move      fil-per-scp          to   rf-oft-per-scp         .
           move      zero                 to   w-c01                  .
       dec-fis-log-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-340.
           move      fil-spe-snx (w-c01)  to   rf-oft-spe-snx (w-c01) .
           move      fil-spe-mad (w-c01)  to   rf-oft-spe-mad (w-c01) .
           move      fil-spe-per (w-c01)  to   rf-oft-spe-per (w-c01) .
           move      fil-spe-ibl (w-c01)  to   rf-oft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       dec-fis-log-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to dec-fis-log-320.
           move      fil-ibx-spe
                    (w-c01, w-c02)        to   rf-oft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      fil-spe-imp (w-c01)  to   rf-oft-spe-imp (w-c01) .
           go to     dec-fis-log-200.
       dec-fis-log-340.
           move      fil-tot-doc          to   rf-oft-tot-doc         .
           move      fil-ctr-stp          to   rf-oft-ctr-stp         .
           move      fil-flg-och          to   rf-oft-flg-och         .
           move      fil-flg-blx (1)      to   rf-oft-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-oft-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-oft-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-oft-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-oft-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-oft-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-oft-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-oft-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-oft-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-oft-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-oft-flg-pul         .
           move      fil-dpz-ids          to   rf-oft-dpz-ids         .
           move      fil-tip-ord          to   rf-oft-tip-ord         .
           move      fil-alx-exp          to   rf-oft-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-oft               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-oft
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

