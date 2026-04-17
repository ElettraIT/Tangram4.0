       Identification Division.
       Program-Id.                                 ioffit             .
      *================================================================*
      *                                                                *
      *                  Input-Output File fit                         *
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
                   15  fil-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-doc-3      pic  9(07)       comp-3     .
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-num-doc        pic  9(11)       comp-3     .
                   15  fil-cod-tmo        pic  x(05)                  .
                   15  fil-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-scl-ann        pic  9(03)       comp-3     .
                   15  fil-num-giv        pic  9(02)                  .
                   15  fil-sgl-num        pic  x(03)                  .
                   15  fil-num-doc-4      pic  9(11)       comp-3     .
                   15  fil-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CLIDAT                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-cli        pic  9(07)       comp-3     .
                   15  fil-dat-doc-5      pic  9(07)       comp-3     .
                   15  fil-num-doc-5      pic  9(11)       comp-3     .
                   15  fil-cod-tmo-5      pic  x(05)                  .
                   15  fil-num-prt-5      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-tip-doc            pic  9(02)                  .
               10  fil-org-doc            pic  9(02)                  .
               10  fil-dpz-cli            pic  x(04)                  .
               10  fil-tip-frn            pic  9(02)                  .
               10  fil-cli-plf            pic  9(07)       comp-3     .
               10  fil-dpc-plf            pic  x(04)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-ass-iva            pic  9(05)       comp-3     .
               10  fil-ctp-ven            pic  9(07)       comp-3     .
               10  fil-inl-dcm            pic  9(02)                  .
               10  fil-inl-pgt            pic  9(02)                  .
               10  fil-cod-lst            pic  x(03)                  .
               10  fil-csr-aac            pic  9(05)       comp-3     .
               10  fil-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-csc-aac            pic  9(05)       comp-3     .
               10  fil-psc-aac            pic  9(02)v9(01) comp-3     .
               10  fil-cpv-aac            pic  9(05)       comp-3     .
               10  fil-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-voc-des occurs 06  pic  x(03)                  .
               10  fil-cod-fop            pic  9(07)       comp-3     .
               10  fil-scp-aap            pic  9(02)v9(01) comp-3     .
               10  fil-cod-abi            pic  9(05)       comp-3     .
               10  fil-cod-cab            pic  9(05)       comp-3     .
               10  fil-ccc-app            pic  x(12)                  .
               10  fil-nos-ban            pic  x(10)                  .
               10  fil-nos-ccp            pic  x(10)                  .
               10  fil-add-spb            pic  x(03)                  .
               10  fil-ipr-iel            pic  9(02)                  .
               10  fil-pag-dsm            pic  9(07)       comp-3     .
               10  fil-pag-qaf            pic  9(09)       comp-3     .
               10  fil-pag-act            pic  9(09)       comp-3     .
               10  fil-cod-age            pic  9(07)       comp-3     .
               10  fil-fsp-doc            pic  9(02)                  .
               10  fil-pvf-age            pic  9(11)       comp-3     .
               10  fil-tip-vpa            pic  9(02)                  .
               10  fil-cpv-aaa            pic  9(05)       comp-3     .
               10  fil-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
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
               10  fil-add-spi            pic  x(03)                  .
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
                       20  fil-iva-imp    pic s9(11)       comp-3     .
                   15  fil-iva-tdo        pic s9(11)       comp-3     .
               10  fil-ctp-cst.
                   15  fil-ctp-rig occurs 10.
                       20  fil-ctp-cod    pic  9(07)       comp-3     .
                       20  fil-ctp-imp    pic s9(11)       comp-3     .
               10  fil-ctr-stp            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-flg-rfp            pic  x(01)                  .
               10  fil-flg-rda            pic  x(01)                  .
               10  fil-per-rda            pic  9(02)v9(01)            .
               10  fil-pib-rda            pic  9(02)v9(01)            .
               10  fil-tdo-fel            pic  x(05)                  .
               10  fil-alx-exp.
                   15  filler occurs 27   pic  x(01)                  .

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
                   15  pul-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-doc-3      pic  9(07)       comp-3     .
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-num-doc        pic  9(11)       comp-3     .
                   15  pul-cod-tmo        pic  x(05)                  .
                   15  pul-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-scl-ann        pic  9(03)       comp-3     .
                   15  pul-num-giv        pic  9(02)                  .
                   15  pul-sgl-num        pic  x(03)                  .
                   15  pul-num-doc-4      pic  9(11)       comp-3     .
                   15  pul-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CLIDAT                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-cli        pic  9(07)       comp-3     .
                   15  pul-dat-doc-5      pic  9(07)       comp-3     .
                   15  pul-num-doc-5      pic  9(11)       comp-3     .
                   15  pul-cod-tmo-5      pic  x(05)                  .
                   15  pul-num-prt-5      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-tip-doc            pic  9(02)                  .
               10  pul-org-doc            pic  9(02)                  .
               10  pul-dpz-cli            pic  x(04)                  .
               10  pul-tip-frn            pic  9(02)                  .
               10  pul-cli-plf            pic  9(07)       comp-3     .
               10  pul-dpc-plf            pic  x(04)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-ass-iva            pic  9(05)       comp-3     .
               10  pul-ctp-ven            pic  9(07)       comp-3     .
               10  pul-inl-dcm            pic  9(02)                  .
               10  pul-inl-pgt            pic  9(02)                  .
               10  pul-cod-lst            pic  x(03)                  .
               10  pul-csr-aac            pic  9(05)       comp-3     .
               10  pul-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-csc-aac            pic  9(05)       comp-3     .
               10  pul-psc-aac            pic  9(02)v9(01) comp-3     .
               10  pul-cpv-aac            pic  9(05)       comp-3     .
               10  pul-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-voc-des occurs 06  pic  x(03)                  .
               10  pul-cod-fop            pic  9(07)       comp-3     .
               10  pul-scp-aap            pic  9(02)v9(01) comp-3     .
               10  pul-cod-abi            pic  9(05)       comp-3     .
               10  pul-cod-cab            pic  9(05)       comp-3     .
               10  pul-ccc-app            pic  x(12)                  .
               10  pul-nos-ban            pic  x(10)                  .
               10  pul-nos-ccp            pic  x(10)                  .
               10  pul-add-spb            pic  x(03)                  .
               10  pul-ipr-iel            pic  9(02)                  .
               10  pul-pag-dsm            pic  9(07)       comp-3     .
               10  pul-pag-qaf            pic  9(09)       comp-3     .
               10  pul-pag-act            pic  9(09)       comp-3     .
               10  pul-cod-age            pic  9(07)       comp-3     .
               10  pul-fsp-doc            pic  9(02)                  .
               10  pul-pvf-age            pic  9(11)       comp-3     .
               10  pul-tip-vpa            pic  9(02)                  .
               10  pul-cpv-aaa            pic  9(05)       comp-3     .
               10  pul-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
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
               10  pul-add-spi            pic  x(03)                  .
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
                       20  pul-iva-imp    pic s9(11)       comp-3     .
                   15  pul-iva-tdo        pic s9(11)       comp-3     .
               10  pul-ctp-cst.
                   15  pul-ctp-rig occurs 10.
                       20  pul-ctp-cod    pic  9(07)       comp-3     .
                       20  pul-ctp-imp    pic s9(11)       comp-3     .
               10  pul-ctr-stp            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-flg-rfp            pic  x(01)                  .
               10  pul-flg-rda            pic  x(01)                  .
               10  pul-per-rda            pic  9(02)v9(01)            .
               10  pul-pib-rda            pic  9(02)v9(01)            .
               10  pul-tdo-fel            pic  x(05)                  .
               10  pul-alx-exp.
                   15  filler occurs 27   pic  x(01)                  .

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
                     "fit "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/fat/fls/ioc/obj/ioffit              "       .

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
           05  k-ctr                      pic  9(02) value 5          .
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
                            "CLIDAT    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    5      pic  x(10)                  .

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
      *    * Record logico file [fit]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .

      ******************************************************************
       Procedure Division                using f rf-fit               .
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
           move      spaces               to   rf-fit                 .
           move      zero                 to   rf-fit-ide-dat         .
           move      spaces               to   rf-fit-ide-ute         .
           move      spaces               to   rf-fit-ide-fas         .
           move      zero                 to   rf-fit-num-prt         .
           move      spaces               to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-tip-doc         .
           move      zero                 to   rf-fit-org-doc         .
           move      zero                 to   rf-fit-num-giv         .
           move      zero                 to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-num-doc         .
           move      zero                 to   rf-fit-scl-ann         .
           move      spaces               to   rf-fit-sgl-num         .
           move      zero                 to   rf-fit-cod-cli         .
           move      spaces               to   rf-fit-dpz-cli         .
           move      zero                 to   rf-fit-tip-frn         .
           move      zero                 to   rf-fit-cli-plf         .
           move      spaces               to   rf-fit-dpc-plf         .
           move      spaces               to   rf-fit-cod-lng         .
           move      spaces               to   rf-fit-sgl-vpf         .
           move      zero                 to   rf-fit-dec-vpf         .
           move      spaces               to   rf-fit-tdc-vpf         .
           move      zero                 to   rf-fit-cdc-vpf         .
           move      zero                 to   rf-fit-ass-iva         .
           move      zero                 to   rf-fit-ctp-ven         .
           move      zero                 to   rf-fit-inl-dcm         .
           move      zero                 to   rf-fit-inl-pgt         .
           move      spaces               to   rf-fit-cod-lst         .
           move      zero                 to   rf-fit-csr-aac         .
           move      zero                 to   rf-fit-psr-aac (1)     .
           move      zero                 to   rf-fit-psr-aac (2)     .
           move      zero                 to   rf-fit-psr-aac (3)     .
           move      zero                 to   rf-fit-psr-aac (4)     .
           move      zero                 to   rf-fit-psr-aac (5)     .
           move      zero                 to   rf-fit-csc-aac         .
           move      zero                 to   rf-fit-psc-aac         .
           move      zero                 to   rf-fit-cpv-aac         .
           move      zero                 to   rf-fit-ppv-aac (1)     .
           move      zero                 to   rf-fit-ppv-aac (2)     .
           move      zero                 to   rf-fit-ppv-aac (3)     .
           move      spaces               to   rf-fit-voc-des (1)     .
           move      spaces               to   rf-fit-voc-des (2)     .
           move      spaces               to   rf-fit-voc-des (3)     .
           move      spaces               to   rf-fit-voc-des (4)     .
           move      spaces               to   rf-fit-voc-des (5)     .
           move      spaces               to   rf-fit-voc-des (6)     .
           move      zero                 to   rf-fit-cod-fop         .
           move      zero                 to   rf-fit-scp-aap         .
           move      zero                 to   rf-fit-cod-abi         .
           move      zero                 to   rf-fit-cod-cab         .
           move      spaces               to   rf-fit-ccc-app         .
           move      spaces               to   rf-fit-nos-ban         .
           move      spaces               to   rf-fit-nos-ccp         .
           move      spaces               to   rf-fit-add-spb         .
           move      zero                 to   rf-fit-ipr-iel         .
           move      zero                 to   rf-fit-pag-dsm         .
           move      zero                 to   rf-fit-pag-qaf         .
           move      zero                 to   rf-fit-pag-act         .
           move      zero                 to   rf-fit-cod-age         .
           move      zero                 to   rf-fit-fsp-doc         .
           move      zero                 to   rf-fit-pvf-age         .
           move      zero                 to   rf-fit-tip-vpa         .
           move      zero                 to   rf-fit-cpv-aaa         .
           move      zero                 to   rf-fit-ppv-aaa (1)     .
           move      zero                 to   rf-fit-ppv-aaa (2)     .
           move      zero                 to   rf-fit-ppv-aaa (3)     .
           move      zero                 to   rf-fit-cod-ime         .
           move      zero                 to   rf-fit-pvf-ime         .
           move      zero                 to   rf-fit-tot-rig (1)     .
           move      zero                 to   rf-fit-tot-rig (2)     .
           move      zero                 to   rf-fit-tot-rig (3)     .
           move      zero                 to   rf-fit-tot-rig (4)     .
           move      zero                 to   rf-fit-tot-rig (5)     .
           move      zero                 to   rf-fit-tot-rig (6)     .
           move      zero                 to   rf-fit-tot-rig (7)     .
           move      zero                 to   rf-fit-tot-rig (8)     .
           move      zero                 to   rf-fit-tot-rig (9)     .
           move      zero                 to   rf-fit-tot-scc         .
           move      zero                 to   rf-fit-per-scc         .
           move      zero                 to   rf-fit-civ-scc         .
           move      zero                 to   rf-fit-ccp-scc         .
           move      zero                 to   rf-fit-tot-scp         .
           move      zero                 to   rf-fit-per-scp         .
           move      zero                 to   rf-fit-civ-scp         .
           move      zero                 to   rf-fit-ccp-scp         .
           move      zero                 to   w-c01                  .
       nor-rec-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-240.
           move      zero                 to   rf-fit-spe-snx (w-c01) .
           move      zero                 to   rf-fit-spe-mad (w-c01) .
           move      zero                 to   rf-fit-spe-per (w-c01) .
           move      zero                 to   rf-fit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       nor-rec-log-200.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to nor-rec-log-220.
           move      spaces               to   rf-fit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     nor-rec-log-200.
       nor-rec-log-220.
           move      zero                 to   rf-fit-spe-imp (w-c01) .
           move      zero                 to   rf-fit-spe-civ (w-c01) .
           move      zero                 to   rf-fit-spe-ccp (w-c01) .
           go to     nor-rec-log-100.
       nor-rec-log-240.
           move      spaces               to   rf-fit-add-spi         .
           move      zero                 to   rf-fit-civ-spi         .
           move      zero                 to   rf-fit-ccp-spi         .
           move      zero                 to   rf-fit-tot-sic         .
           move      zero                 to   rf-fit-tot-sia         .
           move      zero                 to   rf-fit-tot-spb         .
           move      zero                 to   rf-fit-civ-spb         .
           move      zero                 to   rf-fit-ccp-spb         .
           move      zero                 to   rf-fit-prt-mgd         .
           move      zero                 to   rf-fit-nrg-mgd         .
           move      zero                 to   rf-fit-dri-mgd         .
           move      spaces               to   rf-fit-nri-mgd         .
           move      zero                 to   rf-fit-nps-sdb         .
           move      zero                 to   rf-fit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       nor-rec-log-300.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-320.
           move      zero                 to   rf-fit-iva-cod (w-c01) .
           move      zero                 to   rf-fit-iva-ibl (w-c01) .
           move      zero                 to   rf-fit-iva-imp (w-c01) .
           go to     nor-rec-log-300.
       nor-rec-log-320.
           move      zero                 to   rf-fit-iva-tdo         .
           move      zero                 to   w-c01                  .
       nor-rec-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to nor-rec-log-420.
           move      zero                 to   rf-fit-ctp-cod (w-c01) .
           move      zero                 to   rf-fit-ctp-imp (w-c01) .
           go to     nor-rec-log-400.
       nor-rec-log-420.
           move      zero                 to   rf-fit-ctr-stp         .
           move      spaces               to   rf-fit-flg-blx (1)     .
           move      spaces               to   rf-fit-flg-blx (2)     .
           move      spaces               to   rf-fit-flg-blx (3)     .
           move      spaces               to   rf-fit-flg-blx (4)     .
           move      spaces               to   rf-fit-flg-blx (5)     .
           move      spaces               to   rf-fit-flg-blx (6)     .
           move      spaces               to   rf-fit-flg-blx (7)     .
           move      spaces               to   rf-fit-flg-nbx (1)     .
           move      spaces               to   rf-fit-flg-nbx (2)     .
           move      spaces               to   rf-fit-flg-nbx (3)     .
           move      spaces               to   rf-fit-flg-pul         .
           move      spaces               to   rf-fit-flg-rfp         .
           move      spaces               to   rf-fit-flg-rda         .
           move      zero                 to   rf-fit-per-rda         .
           move      zero                 to   rf-fit-pib-rda         .
           move      spaces               to   rf-fit-tdo-fel         .
           move      spaces               to   rf-fit-alx-exp         .
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
           move      rf-fit-ide-ute       to   fil-ide-ute            .
           move      rf-fit-ide-fas       to   fil-ide-fas            .
           move      rf-fit-tip-doc       to   fil-tip-doc            .
           move      rf-fit-org-doc       to   fil-org-doc            .
           move      rf-fit-dpz-cli       to   fil-dpz-cli            .
           move      rf-fit-tip-frn       to   fil-tip-frn            .
           move      rf-fit-cli-plf       to   fil-cli-plf            .
           move      rf-fit-dpc-plf       to   fil-dpc-plf            .
           move      rf-fit-cod-lng       to   fil-cod-lng            .
           move      rf-fit-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-fit-dec-vpf       to   fil-dec-vpf            .
           move      rf-fit-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-fit-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-fit-ass-iva       to   fil-ass-iva            .
           move      rf-fit-ctp-ven       to   fil-ctp-ven            .
           move      rf-fit-inl-dcm       to   fil-inl-dcm            .
           move      rf-fit-inl-pgt       to   fil-inl-pgt            .
           move      rf-fit-cod-lst       to   fil-cod-lst            .
           move      rf-fit-csr-aac       to   fil-csr-aac            .
           move      rf-fit-psr-aac (1)   to   fil-psr-aac (1)        .
           move      rf-fit-psr-aac (2)   to   fil-psr-aac (2)        .
           move      rf-fit-psr-aac (3)   to   fil-psr-aac (3)        .
           move      rf-fit-psr-aac (4)   to   fil-psr-aac (4)        .
           move      rf-fit-psr-aac (5)   to   fil-psr-aac (5)        .
           move      rf-fit-csc-aac       to   fil-csc-aac            .
           move      rf-fit-psc-aac       to   fil-psc-aac            .
           move      rf-fit-cpv-aac       to   fil-cpv-aac            .
           move      rf-fit-ppv-aac (1)   to   fil-ppv-aac (1)        .
           move      rf-fit-ppv-aac (2)   to   fil-ppv-aac (2)        .
           move      rf-fit-ppv-aac (3)   to   fil-ppv-aac (3)        .
           move      rf-fit-voc-des (1)   to   fil-voc-des (1)        .
           move      rf-fit-voc-des (2)   to   fil-voc-des (2)        .
           move      rf-fit-voc-des (3)   to   fil-voc-des (3)        .
           move      rf-fit-voc-des (4)   to   fil-voc-des (4)        .
           move      rf-fit-voc-des (5)   to   fil-voc-des (5)        .
           move      rf-fit-voc-des (6)   to   fil-voc-des (6)        .
           move      rf-fit-cod-fop       to   fil-cod-fop            .
           move      rf-fit-scp-aap       to   fil-scp-aap            .
           move      rf-fit-cod-abi       to   fil-cod-abi            .
           move      rf-fit-cod-cab       to   fil-cod-cab            .
           move      rf-fit-ccc-app       to   fil-ccc-app            .
           move      rf-fit-nos-ban       to   fil-nos-ban            .
           move      rf-fit-nos-ccp       to   fil-nos-ccp            .
           move      rf-fit-add-spb       to   fil-add-spb            .
           move      rf-fit-ipr-iel       to   fil-ipr-iel            .
           move      rf-fit-pag-dsm       to   fil-pag-dsm            .
           move      rf-fit-pag-qaf       to   fil-pag-qaf            .
           move      rf-fit-pag-act       to   fil-pag-act            .
           move      rf-fit-cod-age       to   fil-cod-age            .
           move      rf-fit-fsp-doc       to   fil-fsp-doc            .
           move      rf-fit-pvf-age       to   fil-pvf-age            .
           move      rf-fit-tip-vpa       to   fil-tip-vpa            .
           move      rf-fit-cpv-aaa       to   fil-cpv-aaa            .
           move      rf-fit-ppv-aaa (1)   to   fil-ppv-aaa (1)        .
           move      rf-fit-ppv-aaa (2)   to   fil-ppv-aaa (2)        .
           move      rf-fit-ppv-aaa (3)   to   fil-ppv-aaa (3)        .
           move      rf-fit-cod-ime       to   fil-cod-ime            .
           move      rf-fit-pvf-ime       to   fil-pvf-ime            .
           move      rf-fit-tot-rig (1)   to   fil-tot-rig (1)        .
           move      rf-fit-tot-rig (2)   to   fil-tot-rig (2)        .
           move      rf-fit-tot-rig (3)   to   fil-tot-rig (3)        .
           move      rf-fit-tot-rig (4)   to   fil-tot-rig (4)        .
           move      rf-fit-tot-rig (5)   to   fil-tot-rig (5)        .
           move      rf-fit-tot-rig (6)   to   fil-tot-rig (6)        .
           move      rf-fit-tot-rig (7)   to   fil-tot-rig (7)        .
           move      rf-fit-tot-rig (8)   to   fil-tot-rig (8)        .
           move      rf-fit-tot-rig (9)   to   fil-tot-rig (9)        .
           move      rf-fit-tot-scc       to   fil-tot-scc            .
           move      rf-fit-per-scc       to   fil-per-scc            .
           move      rf-fit-civ-scc       to   fil-civ-scc            .
           move      rf-fit-ccp-scc       to   fil-ccp-scc            .
           move      rf-fit-tot-scp       to   fil-tot-scp            .
           move      rf-fit-per-scp       to   fil-per-scp            .
           move      rf-fit-civ-scp       to   fil-civ-scp            .
           move      rf-fit-ccp-scp       to   fil-ccp-scp            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-340.
           move      rf-fit-spe-snx (w-c01)
                                          to   fil-spe-snx (w-c01)    .
           move      rf-fit-spe-mad (w-c01)
                                          to   fil-spe-mad (w-c01)    .
           move      rf-fit-spe-per (w-c01)
                                          to   fil-spe-per (w-c01)    .
           move      rf-fit-spe-ibl (w-c01)
                                          to   fil-spe-ibl (w-c01)    .
           move      zero                 to   w-c02                  .
       cmp-log-fis-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to cmp-log-fis-320.
           move      rf-fit-ibx-spe
                     (w-c01, w-c02)       to   fil-ibx-spe
                                              (w-c01, w-c02)          .
           go to     cmp-log-fis-300.
       cmp-log-fis-320.
           move      rf-fit-spe-imp (w-c01)
                                          to   fil-spe-imp (w-c01)    .
           move      rf-fit-spe-civ (w-c01)
                                          to   fil-spe-civ (w-c01)    .
           move      rf-fit-spe-ccp (w-c01)
                                          to   fil-spe-ccp (w-c01)    .
           go to     cmp-log-fis-200.
       cmp-log-fis-340.
           move      rf-fit-add-spi       to   fil-add-spi            .
           move      rf-fit-civ-spi       to   fil-civ-spi            .
           move      rf-fit-ccp-spi       to   fil-ccp-spi            .
           move      rf-fit-tot-sic       to   fil-tot-sic            .
           move      rf-fit-tot-sia       to   fil-tot-sia            .
           move      rf-fit-tot-spb       to   fil-tot-spb            .
           move      rf-fit-civ-spb       to   fil-civ-spb            .
           move      rf-fit-ccp-spb       to   fil-ccp-spb            .
           move      rf-fit-prt-mgd       to   fil-prt-mgd            .
           move      rf-fit-nrg-mgd       to   fil-nrg-mgd            .
           move      rf-fit-dri-mgd       to   fil-dri-mgd            .
           move      rf-fit-nri-mgd       to   fil-nri-mgd            .
           move      rf-fit-nps-sdb       to   fil-nps-sdb            .
           move      rf-fit-ctr-sdb       to   fil-ctr-sdb            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-420.
           move      rf-fit-iva-cod (w-c01)
                                          to   fil-iva-cod (w-c01)    .
           move      rf-fit-iva-ibl (w-c01)
                                          to   fil-iva-ibl (w-c01)    .
           move      rf-fit-iva-imp (w-c01)
                                          to   fil-iva-imp (w-c01)    .
           go to     cmp-log-fis-400.
       cmp-log-fis-420.
           move      rf-fit-iva-tdo       to   fil-iva-tdo            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to cmp-log-fis-520.
           move      rf-fit-ctp-cod (w-c01)
                                          to   fil-ctp-cod (w-c01)    .
           move      rf-fit-ctp-imp (w-c01)
                                          to   fil-ctp-imp (w-c01)    .
           go to     cmp-log-fis-500.
       cmp-log-fis-520.
           move      rf-fit-ctr-stp       to   fil-ctr-stp            .
           move      rf-fit-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-fit-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-fit-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-fit-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-fit-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-fit-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-fit-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-fit-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-fit-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-fit-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-fit-flg-pul       to   fil-flg-pul            .
           move      rf-fit-flg-rfp       to   fil-flg-rfp            .
           move      rf-fit-flg-rda       to   fil-flg-rda            .
           move      rf-fit-per-rda       to   fil-per-rda            .
           move      rf-fit-pib-rda       to   fil-pib-rda            .
           move      rf-fit-tdo-fel       to   fil-tdo-fel            .
           move      rf-fit-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-fit-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-fit-ide-dat       to   fil-ide-dat            .
           move      rf-fit-dat-doc       to   fil-dat-doc            .
           move      rf-fit-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-fit-dat-doc       to   fil-dat-doc-3          .
           move      rf-fit-cod-dpz       to   fil-cod-dpz            .
           move      rf-fit-num-doc       to   fil-num-doc            .
           move      rf-fit-cod-tmo       to   fil-cod-tmo            .
           move      rf-fit-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-fit-scl-ann       to   fil-scl-ann            .
           move      rf-fit-num-giv       to   fil-num-giv            .
           move      rf-fit-sgl-num       to   fil-sgl-num            .
           move      rf-fit-num-doc       to   fil-num-doc-4          .
           move      rf-fit-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-fit-cod-cli       to   fil-cod-cli            .
           move      rf-fit-dat-doc       to   fil-dat-doc-5          .
           move      rf-fit-num-doc       to   fil-num-doc-5          .
           move      rf-fit-cod-tmo       to   fil-cod-tmo-5          .
           move      rf-fit-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-fit                 .
           move      fil-ide-dat          to   rf-fit-ide-dat         .
           move      fil-ide-ute          to   rf-fit-ide-ute         .
           move      fil-ide-fas          to   rf-fit-ide-fas         .
           move      fil-num-prt          to   rf-fit-num-prt         .
           move      fil-cod-tmo          to   rf-fit-cod-tmo         .
           move      fil-tip-doc          to   rf-fit-tip-doc         .
           move      fil-org-doc          to   rf-fit-org-doc         .
           move      fil-num-giv          to   rf-fit-num-giv         .
           move      fil-cod-dpz          to   rf-fit-cod-dpz         .
           move      fil-dat-doc          to   rf-fit-dat-doc         .
           move      fil-num-doc          to   rf-fit-num-doc         .
           move      fil-scl-ann          to   rf-fit-scl-ann         .
           move      fil-sgl-num          to   rf-fit-sgl-num         .
           move      fil-cod-cli          to   rf-fit-cod-cli         .
           move      fil-dpz-cli          to   rf-fit-dpz-cli         .
           move      fil-tip-frn          to   rf-fit-tip-frn         .
           move      fil-cli-plf          to   rf-fit-cli-plf         .
           move      fil-dpc-plf          to   rf-fit-dpc-plf         .
           move      fil-cod-lng          to   rf-fit-cod-lng         .
           move      fil-sgl-vpf          to   rf-fit-sgl-vpf         .
           move      fil-dec-vpf          to   rf-fit-dec-vpf         .
           move      fil-tdc-vpf          to   rf-fit-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-fit-cdc-vpf         .
           move      fil-ass-iva          to   rf-fit-ass-iva         .
           move      fil-ctp-ven          to   rf-fit-ctp-ven         .
           move      fil-inl-dcm          to   rf-fit-inl-dcm         .
           move      fil-inl-pgt          to   rf-fit-inl-pgt         .
           move      fil-cod-lst          to   rf-fit-cod-lst         .
           move      fil-csr-aac          to   rf-fit-csr-aac         .
           move      fil-psr-aac (1)      to   rf-fit-psr-aac (1)     .
           move      fil-psr-aac (2)      to   rf-fit-psr-aac (2)     .
           move      fil-psr-aac (3)      to   rf-fit-psr-aac (3)     .
           move      fil-psr-aac (4)      to   rf-fit-psr-aac (4)     .
           move      fil-psr-aac (5)      to   rf-fit-psr-aac (5)     .
           move      fil-csc-aac          to   rf-fit-csc-aac         .
           move      fil-psc-aac          to   rf-fit-psc-aac         .
           move      fil-cpv-aac          to   rf-fit-cpv-aac         .
           move      fil-ppv-aac (1)      to   rf-fit-ppv-aac (1)     .
           move      fil-ppv-aac (2)      to   rf-fit-ppv-aac (2)     .
           move      fil-ppv-aac (3)      to   rf-fit-ppv-aac (3)     .
           move      fil-voc-des (1)      to   rf-fit-voc-des (1)     .
           move      fil-voc-des (2)      to   rf-fit-voc-des (2)     .
           move      fil-voc-des (3)      to   rf-fit-voc-des (3)     .
           move      fil-voc-des (4)      to   rf-fit-voc-des (4)     .
           move      fil-voc-des (5)      to   rf-fit-voc-des (5)     .
           move      fil-voc-des (6)      to   rf-fit-voc-des (6)     .
           move      fil-cod-fop          to   rf-fit-cod-fop         .
           move      fil-scp-aap          to   rf-fit-scp-aap         .
           move      fil-cod-abi          to   rf-fit-cod-abi         .
           move      fil-cod-cab          to   rf-fit-cod-cab         .
           move      fil-ccc-app          to   rf-fit-ccc-app         .
           move      fil-nos-ban          to   rf-fit-nos-ban         .
           move      fil-nos-ccp          to   rf-fit-nos-ccp         .
           move      fil-add-spb          to   rf-fit-add-spb         .
           move      fil-ipr-iel          to   rf-fit-ipr-iel         .
           move      fil-pag-dsm          to   rf-fit-pag-dsm         .
           move      fil-pag-qaf          to   rf-fit-pag-qaf         .
           move      fil-pag-act          to   rf-fit-pag-act         .
           move      fil-cod-age          to   rf-fit-cod-age         .
           move      fil-fsp-doc          to   rf-fit-fsp-doc         .
           move      fil-pvf-age          to   rf-fit-pvf-age         .
           move      fil-tip-vpa          to   rf-fit-tip-vpa         .
           move      fil-cpv-aaa          to   rf-fit-cpv-aaa         .
           move      fil-ppv-aaa (1)      to   rf-fit-ppv-aaa (1)     .
           move      fil-ppv-aaa (2)      to   rf-fit-ppv-aaa (2)     .
           move      fil-ppv-aaa (3)      to   rf-fit-ppv-aaa (3)     .
           move      fil-cod-ime          to   rf-fit-cod-ime         .
           move      fil-pvf-ime          to   rf-fit-pvf-ime         .
           move      fil-tot-rig (1)      to   rf-fit-tot-rig (1)     .
           move      fil-tot-rig (2)      to   rf-fit-tot-rig (2)     .
           move      fil-tot-rig (3)      to   rf-fit-tot-rig (3)     .
           move      fil-tot-rig (4)      to   rf-fit-tot-rig (4)     .
           move      fil-tot-rig (5)      to   rf-fit-tot-rig (5)     .
           move      fil-tot-rig (6)      to   rf-fit-tot-rig (6)     .
           move      fil-tot-rig (7)      to   rf-fit-tot-rig (7)     .
           move      fil-tot-rig (8)      to   rf-fit-tot-rig (8)     .
           move      fil-tot-rig (9)      to   rf-fit-tot-rig (9)     .
           move      fil-tot-scc          to   rf-fit-tot-scc         .
           move      fil-per-scc          to   rf-fit-per-scc         .
           move      fil-civ-scc          to   rf-fit-civ-scc         .
           move      fil-ccp-scc          to   rf-fit-ccp-scc         .
           move      fil-tot-scp          to   rf-fit-tot-scp         .
           move      fil-per-scp          to   rf-fit-per-scp         .
           move      fil-civ-scp          to   rf-fit-civ-scp         .
           move      fil-ccp-scp          to   rf-fit-ccp-scp         .
           move      zero                 to   w-c01                  .
       dec-fis-log-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-340.
           move      fil-spe-snx (w-c01)  to   rf-fit-spe-snx (w-c01) .
           move      fil-spe-mad (w-c01)  to   rf-fit-spe-mad (w-c01) .
           move      fil-spe-per (w-c01)  to   rf-fit-spe-per (w-c01) .
           move      fil-spe-ibl (w-c01)  to   rf-fit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       dec-fis-log-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to dec-fis-log-320.
           move      fil-ibx-spe
                    (w-c01, w-c02)        to   rf-fit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      fil-spe-imp (w-c01)  to   rf-fit-spe-imp (w-c01) .
           move      fil-spe-civ (w-c01)  to   rf-fit-spe-civ (w-c01) .
           move      fil-spe-ccp (w-c01)  to   rf-fit-spe-ccp (w-c01) .
           go to     dec-fis-log-200.
       dec-fis-log-340.
           move      fil-add-spi          to   rf-fit-add-spi         .
           move      fil-civ-spi          to   rf-fit-civ-spi         .
           move      fil-ccp-spi          to   rf-fit-ccp-spi         .
           move      fil-tot-sic          to   rf-fit-tot-sic         .
           move      fil-tot-sia          to   rf-fit-tot-sia         .
           move      fil-tot-spb          to   rf-fit-tot-spb         .
           move      fil-civ-spb          to   rf-fit-civ-spb         .
           move      fil-ccp-spb          to   rf-fit-ccp-spb         .
           move      fil-prt-mgd          to   rf-fit-prt-mgd         .
           move      fil-nrg-mgd          to   rf-fit-nrg-mgd         .
           move      fil-dri-mgd          to   rf-fit-dri-mgd         .
           move      fil-nri-mgd          to   rf-fit-nri-mgd         .
           move      fil-nps-sdb          to   rf-fit-nps-sdb         .
           move      fil-ctr-sdb          to   rf-fit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       dec-fis-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-420.
           move      fil-iva-cod (w-c01)  to   rf-fit-iva-cod (w-c01) .
           move      fil-iva-ibl (w-c01)  to   rf-fit-iva-ibl (w-c01) .
           move      fil-iva-imp (w-c01)  to   rf-fit-iva-imp (w-c01) .
           go to     dec-fis-log-400.
       dec-fis-log-420.
           move      fil-iva-tdo          to   rf-fit-iva-tdo         .
           move      zero                 to   w-c01                  .
       dec-fis-log-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to dec-fis-log-520.
           move      fil-ctp-cod (w-c01)  to   rf-fit-ctp-cod (w-c01) .
           move      fil-ctp-imp (w-c01)  to   rf-fit-ctp-imp (w-c01) .
           go to     dec-fis-log-500.
       dec-fis-log-520.
           move      fil-ctr-stp          to   rf-fit-ctr-stp         .
           move      fil-flg-blx (1)      to   rf-fit-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-fit-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-fit-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-fit-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-fit-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-fit-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-fit-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-fit-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-fit-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-fit-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-fit-flg-pul         .
           move      fil-flg-rfp          to   rf-fit-flg-rfp         .
           move      fil-flg-rda          to   rf-fit-flg-rda         .
           move      fil-per-rda          to   rf-fit-per-rda         .
           move      fil-pib-rda          to   rf-fit-pib-rda         .
           move      fil-tdo-fel          to   rf-fit-tdo-fel         .
           move      fil-alx-exp          to   rf-fit-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-fit               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-fit
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

