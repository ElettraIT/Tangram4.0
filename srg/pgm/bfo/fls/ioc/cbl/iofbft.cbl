       Identification Division.
       Program-Id.                                 iofbft             .
      *================================================================*
      *                                                                *
      *                  Input-Output File bft                         *
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
                   alternate record key   is fil-k03
                   alternate record key   is fil-k04
                   alternate record key   is fil-k05
                   alternate record key   is fil-k06
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
                   15  fil-dat-reg        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-reg-3      pic  9(07)       comp-3     .
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-cod-tmb        pic  x(05)                  .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-dat-reg-4      pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-cod-tmb-5      pic  x(05)                  .
                   15  fil-dat-reg-5      pic  9(07)       comp-3     .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZBCH                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-dpz-6      pic  9(02)                  .
                   15  fil-flg-bch        pic  x(01)                  .
                   15  fil-num-prt-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-prt-ftf            pic  9(11)       comp-3     .
               10  fil-prt-mag            pic  9(11)       comp-3     .
               10  fil-pr2-mag            pic  9(11)       comp-3     .
               10  fil-int-ftr            pic  9(02)                  .
               10  fil-tmo-ftr            pic  x(05)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-arc-plf            pic  9(07)       comp-3     .
               10  fil-dpz-plf            pic  x(04)                  .
               10  fil-tip-ids            pic  9(02)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-doc            pic  x(10)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-ass-iva            pic  9(05)       comp-3     .
               10  fil-ctp-acq            pic  9(07)       comp-3     .
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
               10  fil-civ-scc            pic  9(05)       comp-3     .
               10  fil-ccp-scc            pic  9(07)       comp-3     .
               10  fil-tot-scp            pic s9(11)       comp-3     .
               10  fil-per-scp            pic  9(02)v9(01) comp-3     .
               10  fil-civ-scp            pic  9(05)       comp-3     .
               10  fil-ccp-scp            pic  9(07)       comp-3     .
               10  fil-spe-add occurs 06.
                   15  fil-spe-snx        pic  9(01)                  .
                   15  fil-spe-mad        pic  9(01)                  .
                   15  fil-spe-per        pic  9(02)v9(01) comp-3     .
                   15  fil-spe-ibl        pic  9(02)                  .
                   15  fil-ibt-spe.
                       20  fil-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  fil-spe-imp        pic s9(09)       comp-3     .
                   15  fil-spe-civ        pic  9(05)       comp-3     .
                   15  fil-spe-ccp        pic  9(07)       comp-3     .
               10  fil-civ-spi            pic  9(05)       comp-3     .
               10  fil-ccp-spi            pic  9(07)       comp-3     .
               10  fil-tot-sic            pic s9(09)       comp-3     .
               10  fil-tot-sia            pic s9(09)       comp-3     .
               10  fil-tot-spb            pic s9(09)       comp-3     .
               10  fil-civ-spb            pic  9(05)       comp-3     .
               10  fil-ccp-spb            pic  9(07)       comp-3     .
               10  fil-prt-mgd            pic  9(07)       comp-3     .
               10  fil-nrg-mgd            pic  9(02)                  .
               10  fil-dri-mgd            pic  9(07)       comp-3     .
               10  fil-nri-mgd            pic  x(10)                  .
               10  fil-nps-sdb            pic  9(11)                  .
               10  fil-ctr-sdb            pic  9(02)                  .
               10  fil-iva-cst.
                   15  fil-iva-rig occurs 06.
                       20  fil-iva-cod    pic  9(05)       comp-3     .
                       20  fil-iva-ibl    pic s9(11)       comp-3     .
               10  fil-ctp-cst.
                   15  fil-ctp-rig occurs 10.
                       20  fil-ctp-cod    pic  9(07)       comp-3     .
                       20  fil-ctp-imp    pic s9(11)       comp-3     .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
                   15  pul-dat-reg        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-reg-3      pic  9(07)       comp-3     .
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-cod-tmb        pic  x(05)                  .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-dat-reg-4      pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-cod-tmb-5      pic  x(05)                  .
                   15  pul-dat-reg-5      pic  9(07)       comp-3     .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZBCH                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-dpz-6      pic  9(02)                  .
                   15  pul-flg-bch        pic  x(01)                  .
                   15  pul-num-prt-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-prt-ftf            pic  9(11)       comp-3     .
               10  pul-prt-mag            pic  9(11)       comp-3     .
               10  pul-pr2-mag            pic  9(11)       comp-3     .
               10  pul-int-ftr            pic  9(02)                  .
               10  pul-tmo-ftr            pic  x(05)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-arc-plf            pic  9(07)       comp-3     .
               10  pul-dpz-plf            pic  x(04)                  .
               10  pul-tip-ids            pic  9(02)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-doc            pic  x(10)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-ass-iva            pic  9(05)       comp-3     .
               10  pul-ctp-acq            pic  9(07)       comp-3     .
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
               10  pul-civ-scc            pic  9(05)       comp-3     .
               10  pul-ccp-scc            pic  9(07)       comp-3     .
               10  pul-tot-scp            pic s9(11)       comp-3     .
               10  pul-per-scp            pic  9(02)v9(01) comp-3     .
               10  pul-civ-scp            pic  9(05)       comp-3     .
               10  pul-ccp-scp            pic  9(07)       comp-3     .
               10  pul-spe-add occurs 06.
                   15  pul-spe-snx        pic  9(01)                  .
                   15  pul-spe-mad        pic  9(01)                  .
                   15  pul-spe-per        pic  9(02)v9(01) comp-3     .
                   15  pul-spe-ibl        pic  9(02)                  .
                   15  pul-ibt-spe.
                       20  pul-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  pul-spe-imp        pic s9(09)       comp-3     .
                   15  pul-spe-civ        pic  9(05)       comp-3     .
                   15  pul-spe-ccp        pic  9(07)       comp-3     .
               10  pul-civ-spi            pic  9(05)       comp-3     .
               10  pul-ccp-spi            pic  9(07)       comp-3     .
               10  pul-tot-sic            pic s9(09)       comp-3     .
               10  pul-tot-sia            pic s9(09)       comp-3     .
               10  pul-tot-spb            pic s9(09)       comp-3     .
               10  pul-civ-spb            pic  9(05)       comp-3     .
               10  pul-ccp-spb            pic  9(07)       comp-3     .
               10  pul-prt-mgd            pic  9(07)       comp-3     .
               10  pul-nrg-mgd            pic  9(02)                  .
               10  pul-dri-mgd            pic  9(07)       comp-3     .
               10  pul-nri-mgd            pic  x(10)                  .
               10  pul-nps-sdb            pic  9(11)                  .
               10  pul-ctr-sdb            pic  9(02)                  .
               10  pul-iva-cst.
                   15  pul-iva-rig occurs 06.
                       20  pul-iva-cod    pic  9(05)       comp-3     .
                       20  pul-iva-ibl    pic s9(11)       comp-3     .
               10  pul-ctp-cst.
                   15  pul-ctp-rig occurs 10.
                       20  pul-ctp-cod    pic  9(07)       comp-3     .
                       20  pul-ctp-imp    pic s9(11)       comp-3     .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
                     "bft "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bfo/fls/ioc/obj/iofbft              "       .

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
           05  k-ctr                      pic  9(02) value 6          .
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
                            "DPZARCDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZTMBDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZBCH    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    6      pic  x(10)                  .

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
      *    * Record logico file [bft]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .

      ******************************************************************
       Procedure Division                using f rf-bft               .
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
           move      spaces               to   rf-bft                 .
           move      zero                 to   rf-bft-ide-dat         .
           move      spaces               to   rf-bft-ide-ute         .
           move      spaces               to   rf-bft-ide-fas         .
           move      zero                 to   rf-bft-num-prt         .
           move      spaces               to   rf-bft-cod-tmb         .
           move      zero                 to   rf-bft-prt-ftf         .
           move      zero                 to   rf-bft-prt-mag         .
           move      zero                 to   rf-bft-pr2-mag         .
           move      zero                 to   rf-bft-int-ftr         .
           move      spaces               to   rf-bft-tmo-ftr         .
           move      zero                 to   rf-bft-cod-dpz         .
           move      zero                 to   rf-bft-dat-reg         .
           move      spaces               to   rf-bft-tip-arc         .
           move      zero                 to   rf-bft-cod-arc         .
           move      spaces               to   rf-bft-dpz-arc         .
           move      zero                 to   rf-bft-arc-plf         .
           move      spaces               to   rf-bft-dpz-plf         .
           move      zero                 to   rf-bft-tip-ids         .
           move      zero                 to   rf-bft-dat-doc         .
           move      spaces               to   rf-bft-num-doc         .
           move      spaces               to   rf-bft-cod-lng         .
           move      spaces               to   rf-bft-sgl-vpf         .
           move      zero                 to   rf-bft-dec-vpf         .
           move      spaces               to   rf-bft-tdc-vpf         .
           move      zero                 to   rf-bft-cdc-vpf         .
           move      zero                 to   rf-bft-ass-iva         .
           move      zero                 to   rf-bft-ctp-acq         .
           move      zero                 to   rf-bft-inl-pgt         .
           move      spaces               to   rf-bft-cod-lst         .
           move      zero                 to   rf-bft-csr-aaf         .
           move      zero                 to   rf-bft-psr-aaf (1)     .
           move      zero                 to   rf-bft-psr-aaf (2)     .
           move      zero                 to   rf-bft-psr-aaf (3)     .
           move      zero                 to   rf-bft-psr-aaf (4)     .
           move      zero                 to   rf-bft-psr-aaf (5)     .
           move      zero                 to   rf-bft-csc-aaf         .
           move      zero                 to   rf-bft-psc-aaf         .
           move      spaces               to   rf-bft-voc-des (1)     .
           move      spaces               to   rf-bft-voc-des (2)     .
           move      spaces               to   rf-bft-voc-des (3)     .
           move      spaces               to   rf-bft-voc-des (4)     .
           move      spaces               to   rf-bft-voc-des (5)     .
           move      spaces               to   rf-bft-voc-des (6)     .
           move      zero                 to   rf-bft-cod-fop         .
           move      zero                 to   rf-bft-scp-aap         .
           move      spaces               to   rf-bft-nos-ban         .
           move      zero                 to   rf-bft-cod-abi         .
           move      zero                 to   rf-bft-cod-cab         .
           move      spaces               to   rf-bft-ccc-app         .
           move      spaces               to   rf-bft-ccp-app         .
           move      spaces               to   rf-bft-add-spi         .
           move      spaces               to   rf-bft-add-spb         .
           move      zero                 to   rf-bft-ipr-iel         .
           move      zero                 to   rf-bft-pag-dsm         .
           move      zero                 to   rf-bft-pag-qaf         .
           move      zero                 to   rf-bft-pag-act         .
           move      zero                 to   rf-bft-cod-aqt         .
           move      zero                 to   rf-bft-pvf-aqt         .
           move      zero                 to   rf-bft-cod-ime         .
           move      zero                 to   rf-bft-pvf-ime         .
           move      zero                 to   rf-bft-tot-rig (1)     .
           move      zero                 to   rf-bft-tot-rig (2)     .
           move      zero                 to   rf-bft-tot-rig (3)     .
           move      zero                 to   rf-bft-tot-rig (4)     .
           move      zero                 to   rf-bft-tot-rig (5)     .
           move      zero                 to   rf-bft-tot-rig (6)     .
           move      zero                 to   rf-bft-tot-rig (7)     .
           move      zero                 to   rf-bft-tot-rig (8)     .
           move      zero                 to   rf-bft-tot-rig (9)     .
           move      zero                 to   rf-bft-tot-scc         .
           move      zero                 to   rf-bft-per-scc         .
           move      zero                 to   rf-bft-civ-scc         .
           move      zero                 to   rf-bft-ccp-scc         .
           move      zero                 to   rf-bft-tot-scp         .
           move      zero                 to   rf-bft-per-scp         .
           move      zero                 to   rf-bft-civ-scp         .
           move      zero                 to   rf-bft-ccp-scp         .
           move      zero                 to   w-c01                  .
       nor-rec-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-240.
           move      zero                 to   rf-bft-spe-snx (w-c01) .
           move      zero                 to   rf-bft-spe-mad (w-c01) .
           move      zero                 to   rf-bft-spe-per (w-c01) .
           move      zero                 to   rf-bft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       nor-rec-log-200.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to nor-rec-log-220.
           move      spaces               to   rf-bft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     nor-rec-log-200.
       nor-rec-log-220.
           move      zero                 to   rf-bft-spe-imp (w-c01) .
           move      zero                 to   rf-bft-spe-civ (w-c01) .
           move      zero                 to   rf-bft-spe-ccp (w-c01) .
           go to     nor-rec-log-100.
       nor-rec-log-240.
           move      zero                 to   rf-bft-civ-spi         .
           move      zero                 to   rf-bft-ccp-spi         .
           move      zero                 to   rf-bft-tot-sic         .
           move      zero                 to   rf-bft-tot-sia         .
           move      zero                 to   rf-bft-tot-spb         .
           move      zero                 to   rf-bft-civ-spb         .
           move      zero                 to   rf-bft-ccp-spb         .
           move      zero                 to   rf-bft-prt-mgd         .
           move      zero                 to   rf-bft-nrg-mgd         .
           move      zero                 to   rf-bft-dri-mgd         .
           move      spaces               to   rf-bft-nri-mgd         .
           move      zero                 to   rf-bft-nps-sdb         .
           move      zero                 to   rf-bft-ctr-sdb         .
           move      zero                 to   w-c01                  .
       nor-rec-log-300.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-320.
           move      zero                 to   rf-bft-iva-cod (w-c01) .
           move      zero                 to   rf-bft-iva-ibl (w-c01) .
           go to     nor-rec-log-300.
       nor-rec-log-320.
           move      zero                 to   w-c01                  .
       nor-rec-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to nor-rec-log-420.
           move      zero                 to   rf-bft-ctp-cod (w-c01) .
           move      zero                 to   rf-bft-ctp-imp (w-c01) .
           go to     nor-rec-log-400.
       nor-rec-log-420.
           move      spaces               to   rf-bft-flg-bch         .
           move      spaces               to   rf-bft-flg-blx (1)     .
           move      spaces               to   rf-bft-flg-blx (2)     .
           move      spaces               to   rf-bft-flg-blx (3)     .
           move      spaces               to   rf-bft-flg-blx (4)     .
           move      spaces               to   rf-bft-flg-blx (5)     .
           move      spaces               to   rf-bft-flg-blx (6)     .
           move      spaces               to   rf-bft-flg-blx (7)     .
           move      spaces               to   rf-bft-flg-nbx (1)     .
           move      spaces               to   rf-bft-flg-nbx (2)     .
           move      spaces               to   rf-bft-flg-nbx (3)     .
           move      spaces               to   rf-bft-flg-pul         .
           move      spaces               to   rf-bft-alx-exp         .
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
           move      rf-bft-ide-ute       to   fil-ide-ute            .
           move      rf-bft-ide-fas       to   fil-ide-fas            .
           move      rf-bft-prt-ftf       to   fil-prt-ftf            .
           move      rf-bft-prt-mag       to   fil-prt-mag            .
           move      rf-bft-pr2-mag       to   fil-pr2-mag            .
           move      rf-bft-int-ftr       to   fil-int-ftr            .
           move      rf-bft-tmo-ftr       to   fil-tmo-ftr            .
           move      rf-bft-dpz-arc       to   fil-dpz-arc            .
           move      rf-bft-arc-plf       to   fil-arc-plf            .
           move      rf-bft-dpz-plf       to   fil-dpz-plf            .
           move      rf-bft-tip-ids       to   fil-tip-ids            .
           move      rf-bft-dat-doc       to   fil-dat-doc            .
           move      rf-bft-num-doc       to   fil-num-doc            .
           move      rf-bft-cod-lng       to   fil-cod-lng            .
           move      rf-bft-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-bft-dec-vpf       to   fil-dec-vpf            .
           move      rf-bft-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-bft-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-bft-ass-iva       to   fil-ass-iva            .
           move      rf-bft-ctp-acq       to   fil-ctp-acq            .
           move      rf-bft-inl-pgt       to   fil-inl-pgt            .
           move      rf-bft-cod-lst       to   fil-cod-lst            .
           move      rf-bft-csr-aaf       to   fil-csr-aaf            .
           move      rf-bft-psr-aaf (1)   to   fil-psr-aaf (1)        .
           move      rf-bft-psr-aaf (2)   to   fil-psr-aaf (2)        .
           move      rf-bft-psr-aaf (3)   to   fil-psr-aaf (3)        .
           move      rf-bft-psr-aaf (4)   to   fil-psr-aaf (4)        .
           move      rf-bft-psr-aaf (5)   to   fil-psr-aaf (5)        .
           move      rf-bft-csc-aaf       to   fil-csc-aaf            .
           move      rf-bft-psc-aaf       to   fil-psc-aaf            .
           move      rf-bft-voc-des (1)   to   fil-voc-des (1)        .
           move      rf-bft-voc-des (2)   to   fil-voc-des (2)        .
           move      rf-bft-voc-des (3)   to   fil-voc-des (3)        .
           move      rf-bft-voc-des (4)   to   fil-voc-des (4)        .
           move      rf-bft-voc-des (5)   to   fil-voc-des (5)        .
           move      rf-bft-voc-des (6)   to   fil-voc-des (6)        .
           move      rf-bft-cod-fop       to   fil-cod-fop            .
           move      rf-bft-scp-aap       to   fil-scp-aap            .
           move      rf-bft-nos-ban       to   fil-nos-ban            .
           move      rf-bft-cod-abi       to   fil-cod-abi            .
           move      rf-bft-cod-cab       to   fil-cod-cab            .
           move      rf-bft-ccc-app       to   fil-ccc-app            .
           move      rf-bft-ccp-app       to   fil-ccp-app            .
           move      rf-bft-add-spi       to   fil-add-spi            .
           move      rf-bft-add-spb       to   fil-add-spb            .
           move      rf-bft-ipr-iel       to   fil-ipr-iel            .
           move      rf-bft-pag-dsm       to   fil-pag-dsm            .
           move      rf-bft-pag-qaf       to   fil-pag-qaf            .
           move      rf-bft-pag-act       to   fil-pag-act            .
           move      rf-bft-cod-aqt       to   fil-cod-aqt            .
           move      rf-bft-pvf-aqt       to   fil-pvf-aqt            .
           move      rf-bft-cod-ime       to   fil-cod-ime            .
           move      rf-bft-pvf-ime       to   fil-pvf-ime            .
           move      rf-bft-tot-rig (1)   to   fil-tot-rig (1)        .
           move      rf-bft-tot-rig (2)   to   fil-tot-rig (2)        .
           move      rf-bft-tot-rig (3)   to   fil-tot-rig (3)        .
           move      rf-bft-tot-rig (4)   to   fil-tot-rig (4)        .
           move      rf-bft-tot-rig (5)   to   fil-tot-rig (5)        .
           move      rf-bft-tot-rig (6)   to   fil-tot-rig (6)        .
           move      rf-bft-tot-rig (7)   to   fil-tot-rig (7)        .
           move      rf-bft-tot-rig (8)   to   fil-tot-rig (8)        .
           move      rf-bft-tot-rig (9)   to   fil-tot-rig (9)        .
           move      rf-bft-tot-scc       to   fil-tot-scc            .
           move      rf-bft-per-scc       to   fil-per-scc            .
           move      rf-bft-civ-scc       to   fil-civ-scc            .
           move      rf-bft-ccp-scc       to   fil-ccp-scc            .
           move      rf-bft-tot-scp       to   fil-tot-scp            .
           move      rf-bft-per-scp       to   fil-per-scp            .
           move      rf-bft-civ-scp       to   fil-civ-scp            .
           move      rf-bft-ccp-scp       to   fil-ccp-scp            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-340.
           move      rf-bft-spe-snx (w-c01)
                                          to   fil-spe-snx (w-c01)    .
           move      rf-bft-spe-mad (w-c01)
                                          to   fil-spe-mad (w-c01)    .
           move      rf-bft-spe-per (w-c01)
                                          to   fil-spe-per (w-c01)    .
           move      rf-bft-spe-ibl (w-c01)
                                          to   fil-spe-ibl (w-c01)    .
           move      zero                 to   w-c02                  .
       cmp-log-fis-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to cmp-log-fis-320.
           move      rf-bft-ibx-spe
                     (w-c01, w-c02)       to   fil-ibx-spe
                                              (w-c01, w-c02)          .
           go to     cmp-log-fis-300.
       cmp-log-fis-320.
           move      rf-bft-spe-imp (w-c01)
                                          to   fil-spe-imp (w-c01)    .
           move      rf-bft-spe-civ (w-c01)
                                          to   fil-spe-civ (w-c01)    .
           move      rf-bft-spe-ccp (w-c01)
                                          to   fil-spe-ccp (w-c01)    .
           go to     cmp-log-fis-200.
       cmp-log-fis-340.
           move      rf-bft-civ-spi       to   fil-civ-spi            .
           move      rf-bft-ccp-spi       to   fil-ccp-spi            .
           move      rf-bft-tot-sic       to   fil-tot-sic            .
           move      rf-bft-tot-sia       to   fil-tot-sia            .
           move      rf-bft-tot-spb       to   fil-tot-spb            .
           move      rf-bft-civ-spb       to   fil-civ-spb            .
           move      rf-bft-ccp-spb       to   fil-ccp-spb            .
           move      rf-bft-prt-mgd       to   fil-prt-mgd            .
           move      rf-bft-nrg-mgd       to   fil-nrg-mgd            .
           move      rf-bft-dri-mgd       to   fil-dri-mgd            .
           move      rf-bft-nri-mgd       to   fil-nri-mgd            .
           move      rf-bft-nps-sdb       to   fil-nps-sdb            .
           move      rf-bft-ctr-sdb       to   fil-ctr-sdb            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-420.
           move      rf-bft-iva-cod (w-c01)
                                          to   fil-iva-cod (w-c01)    .
           move      rf-bft-iva-ibl (w-c01)
                                          to   fil-iva-ibl (w-c01)    .
           go to     cmp-log-fis-400.
       cmp-log-fis-420.
           move      zero                 to   w-c01                  .
       cmp-log-fis-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to cmp-log-fis-520.
           move      rf-bft-ctp-cod (w-c01)
                                          to   fil-ctp-cod (w-c01)    .
           move      rf-bft-ctp-imp (w-c01)
                                          to   fil-ctp-imp (w-c01)    .
           go to     cmp-log-fis-500.
       cmp-log-fis-520.
           move      rf-bft-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-bft-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-bft-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-bft-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-bft-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-bft-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-bft-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-bft-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-bft-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-bft-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-bft-flg-pul       to   fil-flg-pul            .
           move      rf-bft-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-bft-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-bft-ide-dat       to   fil-ide-dat            .
           move      rf-bft-dat-reg       to   fil-dat-reg            .
           move      rf-bft-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-bft-dat-reg       to   fil-dat-reg-3          .
           move      rf-bft-cod-dpz       to   fil-cod-dpz            .
           move      rf-bft-cod-tmb       to   fil-cod-tmb            .
           move      rf-bft-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-bft-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-bft-tip-arc       to   fil-tip-arc            .
           move      rf-bft-cod-arc       to   fil-cod-arc            .
           move      rf-bft-dat-reg       to   fil-dat-reg-4          .
           move      rf-bft-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-bft-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-bft-cod-tmb       to   fil-cod-tmb-5          .
           move      rf-bft-dat-reg       to   fil-dat-reg-5          .
           move      rf-bft-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-bft-cod-dpz       to   fil-cod-dpz-6          .
           move      rf-bft-flg-bch       to   fil-flg-bch            .
           move      rf-bft-num-prt       to   fil-num-prt-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-bft                 .
           move      fil-ide-dat          to   rf-bft-ide-dat         .
           move      fil-ide-ute          to   rf-bft-ide-ute         .
           move      fil-ide-fas          to   rf-bft-ide-fas         .
           move      fil-num-prt          to   rf-bft-num-prt         .
           move      fil-cod-tmb          to   rf-bft-cod-tmb         .
           move      fil-prt-ftf          to   rf-bft-prt-ftf         .
           move      fil-prt-mag          to   rf-bft-prt-mag         .
           move      fil-pr2-mag          to   rf-bft-pr2-mag         .
           move      fil-int-ftr          to   rf-bft-int-ftr         .
           move      fil-tmo-ftr          to   rf-bft-tmo-ftr         .
           move      fil-cod-dpz          to   rf-bft-cod-dpz         .
           move      fil-dat-reg          to   rf-bft-dat-reg         .
           move      fil-tip-arc          to   rf-bft-tip-arc         .
           move      fil-cod-arc          to   rf-bft-cod-arc         .
           move      fil-dpz-arc          to   rf-bft-dpz-arc         .
           move      fil-arc-plf          to   rf-bft-arc-plf         .
           move      fil-dpz-plf          to   rf-bft-dpz-plf         .
           move      fil-tip-ids          to   rf-bft-tip-ids         .
           move      fil-dat-doc          to   rf-bft-dat-doc         .
           move      fil-num-doc          to   rf-bft-num-doc         .
           move      fil-cod-lng          to   rf-bft-cod-lng         .
           move      fil-sgl-vpf          to   rf-bft-sgl-vpf         .
           move      fil-dec-vpf          to   rf-bft-dec-vpf         .
           move      fil-tdc-vpf          to   rf-bft-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-bft-cdc-vpf         .
           move      fil-ass-iva          to   rf-bft-ass-iva         .
           move      fil-ctp-acq          to   rf-bft-ctp-acq         .
           move      fil-inl-pgt          to   rf-bft-inl-pgt         .
           move      fil-cod-lst          to   rf-bft-cod-lst         .
           move      fil-csr-aaf          to   rf-bft-csr-aaf         .
           move      fil-psr-aaf (1)      to   rf-bft-psr-aaf (1)     .
           move      fil-psr-aaf (2)      to   rf-bft-psr-aaf (2)     .
           move      fil-psr-aaf (3)      to   rf-bft-psr-aaf (3)     .
           move      fil-psr-aaf (4)      to   rf-bft-psr-aaf (4)     .
           move      fil-psr-aaf (5)      to   rf-bft-psr-aaf (5)     .
           move      fil-csc-aaf          to   rf-bft-csc-aaf         .
           move      fil-psc-aaf          to   rf-bft-psc-aaf         .
           move      fil-voc-des (1)      to   rf-bft-voc-des (1)     .
           move      fil-voc-des (2)      to   rf-bft-voc-des (2)     .
           move      fil-voc-des (3)      to   rf-bft-voc-des (3)     .
           move      fil-voc-des (4)      to   rf-bft-voc-des (4)     .
           move      fil-voc-des (5)      to   rf-bft-voc-des (5)     .
           move      fil-voc-des (6)      to   rf-bft-voc-des (6)     .
           move      fil-cod-fop          to   rf-bft-cod-fop         .
           move      fil-scp-aap          to   rf-bft-scp-aap         .
           move      fil-nos-ban          to   rf-bft-nos-ban         .
           move      fil-cod-abi          to   rf-bft-cod-abi         .
           move      fil-cod-cab          to   rf-bft-cod-cab         .
           move      fil-ccc-app          to   rf-bft-ccc-app         .
           move      fil-ccp-app          to   rf-bft-ccp-app         .
           move      fil-add-spi          to   rf-bft-add-spi         .
           move      fil-add-spb          to   rf-bft-add-spb         .
           move      fil-ipr-iel          to   rf-bft-ipr-iel         .
           move      fil-pag-dsm          to   rf-bft-pag-dsm         .
           move      fil-pag-qaf          to   rf-bft-pag-qaf         .
           move      fil-pag-act          to   rf-bft-pag-act         .
           move      fil-cod-aqt          to   rf-bft-cod-aqt         .
           move      fil-pvf-aqt          to   rf-bft-pvf-aqt         .
           move      fil-cod-ime          to   rf-bft-cod-ime         .
           move      fil-pvf-ime          to   rf-bft-pvf-ime         .
           move      fil-tot-rig (1)      to   rf-bft-tot-rig (1)     .
           move      fil-tot-rig (2)      to   rf-bft-tot-rig (2)     .
           move      fil-tot-rig (3)      to   rf-bft-tot-rig (3)     .
           move      fil-tot-rig (4)      to   rf-bft-tot-rig (4)     .
           move      fil-tot-rig (5)      to   rf-bft-tot-rig (5)     .
           move      fil-tot-rig (6)      to   rf-bft-tot-rig (6)     .
           move      fil-tot-rig (7)      to   rf-bft-tot-rig (7)     .
           move      fil-tot-rig (8)      to   rf-bft-tot-rig (8)     .
           move      fil-tot-rig (9)      to   rf-bft-tot-rig (9)     .
           move      fil-tot-scc          to   rf-bft-tot-scc         .
           move      fil-per-scc          to   rf-bft-per-scc         .
           move      fil-civ-scc          to   rf-bft-civ-scc         .
           move      fil-ccp-scc          to   rf-bft-ccp-scc         .
           move      fil-tot-scp          to   rf-bft-tot-scp         .
           move      fil-per-scp          to   rf-bft-per-scp         .
           move      fil-civ-scp          to   rf-bft-civ-scp         .
           move      fil-ccp-scp          to   rf-bft-ccp-scp         .
           move      zero                 to   w-c01                  .
       dec-fis-log-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-340.
           move      fil-spe-snx (w-c01)  to   rf-bft-spe-snx (w-c01) .
           move      fil-spe-mad (w-c01)  to   rf-bft-spe-mad (w-c01) .
           move      fil-spe-per (w-c01)  to   rf-bft-spe-per (w-c01) .
           move      fil-spe-ibl (w-c01)  to   rf-bft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       dec-fis-log-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to dec-fis-log-320.
           move      fil-ibx-spe
                    (w-c01, w-c02)        to   rf-bft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      fil-spe-imp (w-c01)  to   rf-bft-spe-imp (w-c01) .
           move      fil-spe-civ (w-c01)  to   rf-bft-spe-civ (w-c01) .
           move      fil-spe-ccp (w-c01)  to   rf-bft-spe-ccp (w-c01) .
           go to     dec-fis-log-200.
       dec-fis-log-340.
           move      fil-civ-spi          to   rf-bft-civ-spi         .
           move      fil-ccp-spi          to   rf-bft-ccp-spi         .
           move      fil-tot-sic          to   rf-bft-tot-sic         .
           move      fil-tot-sia          to   rf-bft-tot-sia         .
           move      fil-tot-spb          to   rf-bft-tot-spb         .
           move      fil-civ-spb          to   rf-bft-civ-spb         .
           move      fil-ccp-spb          to   rf-bft-ccp-spb         .
           move      fil-prt-mgd          to   rf-bft-prt-mgd         .
           move      fil-nrg-mgd          to   rf-bft-nrg-mgd         .
           move      fil-dri-mgd          to   rf-bft-dri-mgd         .
           move      fil-nri-mgd          to   rf-bft-nri-mgd         .
           move      fil-nps-sdb          to   rf-bft-nps-sdb         .
           move      fil-ctr-sdb          to   rf-bft-ctr-sdb         .
           move      zero                 to   w-c01                  .
       dec-fis-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-420.
           move      fil-iva-cod (w-c01)  to   rf-bft-iva-cod (w-c01) .
           move      fil-iva-ibl (w-c01)  to   rf-bft-iva-ibl (w-c01) .
           go to     dec-fis-log-400.
       dec-fis-log-420.
           move      zero                 to   w-c01                  .
       dec-fis-log-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to dec-fis-log-520.
           move      fil-ctp-cod (w-c01)  to   rf-bft-ctp-cod (w-c01) .
           move      fil-ctp-imp (w-c01)  to   rf-bft-ctp-imp (w-c01) .
           go to     dec-fis-log-500.
       dec-fis-log-520.
           move      fil-flg-bch          to   rf-bft-flg-bch         .
           move      fil-flg-blx (1)      to   rf-bft-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-bft-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-bft-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-bft-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-bft-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-bft-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-bft-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-bft-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-bft-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-bft-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-bft-flg-pul         .
           move      fil-alx-exp          to   rf-bft-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-bft               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-bft
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

