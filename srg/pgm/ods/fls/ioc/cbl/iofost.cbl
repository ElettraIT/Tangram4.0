       Identification Division.
       Program-Id.                                 iofost             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ost                         *
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
                   15  fil-num-prt-1      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat-2      pic  9(07)       comp-3     .
                   15  fil-dat-doc-2      pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dpz-3      pic  9(02)                  .
                   15  fil-tip-arc-3      pic  x(01)                  .
                   15  fil-cod-arc-3      pic  9(07)       comp-3     .
                   15  fil-dat-doc-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZTMSDAT                      *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-cod-tms-4      pic  x(05)                  .
                   15  fil-dat-doc-4      pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZSCH                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-flg-sch-5      pic  x(01)                  .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DATDOC                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-dpz-6      pic  9(02)                  .
                   15  fil-dat-doc-6      pic  9(07)       comp-3     .
                   15  fil-num-prt-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-dat            pic  9(07)       comp-3     .
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-num-prt            pic  9(11)       comp-3     .
               10  fil-cod-tms            pic  x(05)                  .
               10  fil-cod-dpz            pic  9(02)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-tip-arc            pic  x(01)                  .
               10  fil-cod-arc            pic  9(07)       comp-3     .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-tip-frn            pic  9(02)                  .
               10  fil-arc-plf            pic  9(07)       comp-3     .
               10  fil-dpz-plf            pic  x(04)                  .
               10  fil-tip-ftz            pic  9(02)                  .
               10  fil-tip-ids            pic  9(02)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-ass-iva            pic  9(05)       comp-3     .
               10  fil-ctp-ven            pic  9(07)       comp-3     .
               10  fil-fat-sep            pic  x(01)                  .
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
               10  fil-add-spi            pic  x(03)                  .
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
               10  fil-dtr-tra.
                   15  fil-tra-cur        pic  9(02)                  .
                   15  fil-cau-tra        pic  x(03)                  .
                   15  fil-asp-ben        pic  x(03)                  .
                   15  fil-num-col        pic  9(05)       comp-3     .
                   15  fil-pes-tot        pic  9(06)v9(03) comp-3     .
                   15  fil-dat-itr        pic  9(07)       comp-3     .
                   15  fil-ora-itr        pic  9(04)                  .
                   15  fil-cod-vet        pic  9(07)       comp-3     .
                   15  fil-cod-vt2        pic  9(07)       comp-3     .
                   15  fil-cod-vt3        pic  9(07)       comp-3     .
                   15  fil-cod-aps        pic  9(03)       comp-3     .
               10  fil-ctr-stp            pic  9(02)                  .
               10  fil-flg-sch            pic  x(01)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-flg-rfp            pic  x(01)                  .
               10  fil-prt-mag            pic  9(11)                  .
               10  fil-cod-rsm            pic  9(05)                  .
               10  fil-bcd-tip            pic  x(05)                  .
               10  fil-alx-exp.
                   15  filler  occurs 18  pic  x(01)                  .

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
                   15  pul-num-prt-1      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat-2      pic  9(07)       comp-3     .
                   15  pul-dat-doc-2      pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dpz-3      pic  9(02)                  .
                   15  pul-tip-arc-3      pic  x(01)                  .
                   15  pul-cod-arc-3      pic  9(07)       comp-3     .
                   15  pul-dat-doc-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZTMSDAT                      *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-cod-tms-4      pic  x(05)                  .
                   15  pul-dat-doc-4      pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZSCH                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-flg-sch-5      pic  x(01)                  .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DATDOC                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-dpz-6      pic  9(02)                  .
                   15  pul-dat-doc-6      pic  9(07)       comp-3     .
                   15  pul-num-prt-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-dat            pic  9(07)       comp-3     .
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-num-prt            pic  9(11)       comp-3     .
               10  pul-cod-tms            pic  x(05)                  .
               10  pul-cod-dpz            pic  9(02)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-tip-arc            pic  x(01)                  .
               10  pul-cod-arc            pic  9(07)       comp-3     .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-tip-frn            pic  9(02)                  .
               10  pul-arc-plf            pic  9(07)       comp-3     .
               10  pul-dpz-plf            pic  x(04)                  .
               10  pul-tip-ftz            pic  9(02)                  .
               10  pul-tip-ids            pic  9(02)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-ass-iva            pic  9(05)       comp-3     .
               10  pul-ctp-ven            pic  9(07)       comp-3     .
               10  pul-fat-sep            pic  x(01)                  .
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
               10  pul-add-spi            pic  x(03)                  .
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
               10  pul-dtr-tra.
                   15  pul-tra-cur        pic  9(02)                  .
                   15  pul-cau-tra        pic  x(03)                  .
                   15  pul-asp-ben        pic  x(03)                  .
                   15  pul-num-col        pic  9(05)       comp-3     .
                   15  pul-pes-tot        pic  9(06)v9(03) comp-3     .
                   15  pul-dat-itr        pic  9(07)       comp-3     .
                   15  pul-ora-itr        pic  9(04)                  .
                   15  pul-cod-vet        pic  9(07)       comp-3     .
                   15  pul-cod-vt2        pic  9(07)       comp-3     .
                   15  pul-cod-vt3        pic  9(07)       comp-3     .
                   15  pul-cod-aps        pic  9(03)       comp-3     .
               10  pul-ctr-stp            pic  9(02)                  .
               10  pul-flg-sch            pic  x(01)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-flg-rfp            pic  x(01)                  .
               10  pul-prt-mag            pic  9(11)                  .
               10  pul-cod-rsm            pic  9(05)                  .
               10  pul-bcd-tip            pic  x(05)                  .
               10  pul-alx-exp.
                   15  filler  occurs 18  pic  x(01)                  .

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
                     "ost "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/ods/fls/ioc/obj/iofost              "       .

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
                            "DPZARCDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZTMSDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZSCH    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATDOC    "                              .
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
      *    * Record logico file [ost]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .

      ******************************************************************
       Procedure Division                using f rf-ost               .
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
           move      spaces               to   rf-ost                 .
           move      zero                 to   rf-ost-ide-dat         .
           move      spaces               to   rf-ost-ide-ute         .
           move      spaces               to   rf-ost-ide-fas         .
           move      zero                 to   rf-ost-num-prt         .
           move      spaces               to   rf-ost-cod-tms         .
           move      zero                 to   rf-ost-cod-dpz         .
           move      zero                 to   rf-ost-dat-doc         .
           move      spaces               to   rf-ost-tip-arc         .
           move      zero                 to   rf-ost-cod-arc         .
           move      spaces               to   rf-ost-dpz-arc         .
           move      zero                 to   rf-ost-tip-frn         .
           move      zero                 to   rf-ost-arc-plf         .
           move      spaces               to   rf-ost-dpz-plf         .
           move      zero                 to   rf-ost-tip-ftz         .
           move      zero                 to   rf-ost-tip-ids         .
           move      spaces               to   rf-ost-cod-lng         .
           move      spaces               to   rf-ost-sgl-vpf         .
           move      zero                 to   rf-ost-dec-vpf         .
           move      spaces               to   rf-ost-tdc-vpf         .
           move      zero                 to   rf-ost-cdc-vpf         .
           move      zero                 to   rf-ost-ass-iva         .
           move      zero                 to   rf-ost-ctp-ven         .
           move      spaces               to   rf-ost-fat-sep         .
           move      zero                 to   rf-ost-inl-dcm         .
           move      zero                 to   rf-ost-inl-pgt         .
           move      spaces               to   rf-ost-cod-lst         .
           move      zero                 to   rf-ost-csr-aac         .
           move      zero                 to   rf-ost-psr-aac (1)     .
           move      zero                 to   rf-ost-psr-aac (2)     .
           move      zero                 to   rf-ost-psr-aac (3)     .
           move      zero                 to   rf-ost-psr-aac (4)     .
           move      zero                 to   rf-ost-psr-aac (5)     .
           move      zero                 to   rf-ost-csc-aac         .
           move      zero                 to   rf-ost-psc-aac         .
           move      zero                 to   rf-ost-cpv-aac         .
           move      zero                 to   rf-ost-ppv-aac (1)     .
           move      zero                 to   rf-ost-ppv-aac (2)     .
           move      zero                 to   rf-ost-ppv-aac (3)     .
           move      spaces               to   rf-ost-voc-des (1)     .
           move      spaces               to   rf-ost-voc-des (2)     .
           move      spaces               to   rf-ost-voc-des (3)     .
           move      spaces               to   rf-ost-voc-des (4)     .
           move      spaces               to   rf-ost-voc-des (5)     .
           move      spaces               to   rf-ost-voc-des (6)     .
           move      zero                 to   rf-ost-cod-fop         .
           move      zero                 to   rf-ost-scp-aap         .
           move      zero                 to   rf-ost-cod-abi         .
           move      zero                 to   rf-ost-cod-cab         .
           move      spaces               to   rf-ost-ccc-app         .
           move      spaces               to   rf-ost-nos-ban         .
           move      spaces               to   rf-ost-nos-ccp         .
           move      spaces               to   rf-ost-add-spi         .
           move      spaces               to   rf-ost-add-spb         .
           move      zero                 to   rf-ost-ipr-iel         .
           move      zero                 to   rf-ost-pag-dsm         .
           move      zero                 to   rf-ost-pag-qaf         .
           move      zero                 to   rf-ost-pag-act         .
           move      zero                 to   rf-ost-cod-age         .
           move      zero                 to   rf-ost-fsp-doc         .
           move      zero                 to   rf-ost-pvf-age         .
           move      zero                 to   rf-ost-tip-vpa         .
           move      zero                 to   rf-ost-cpv-aaa         .
           move      zero                 to   rf-ost-ppv-aaa (1)     .
           move      zero                 to   rf-ost-ppv-aaa (2)     .
           move      zero                 to   rf-ost-ppv-aaa (3)     .
           move      zero                 to   rf-ost-cod-ime         .
           move      zero                 to   rf-ost-pvf-ime         .
           move      zero                 to   rf-ost-tot-rig (1)     .
           move      zero                 to   rf-ost-tot-rig (2)     .
           move      zero                 to   rf-ost-tot-rig (3)     .
           move      zero                 to   rf-ost-tot-rig (4)     .
           move      zero                 to   rf-ost-tot-rig (5)     .
           move      zero                 to   rf-ost-tot-rig (6)     .
           move      zero                 to   rf-ost-tot-rig (7)     .
           move      zero                 to   rf-ost-tot-rig (8)     .
           move      zero                 to   rf-ost-tot-rig (9)     .
           move      zero                 to   rf-ost-tot-scc         .
           move      zero                 to   rf-ost-per-scc         .
           move      zero                 to   rf-ost-tot-scp         .
           move      zero                 to   rf-ost-per-scp         .
           move      zero                 to   w-c01                  .
       nor-rec-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-240.
           move      zero                 to   rf-ost-spe-snx (w-c01) .
           move      zero                 to   rf-ost-spe-mad (w-c01) .
           move      zero                 to   rf-ost-spe-per (w-c01) .
           move      zero                 to   rf-ost-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       nor-rec-log-200.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to nor-rec-log-220.
           move      spaces               to   rf-ost-ibx-spe
                                               (w-c01, w-c02)         .
           go to     nor-rec-log-200.
       nor-rec-log-220.
           move      zero                 to   rf-ost-spe-imp (w-c01) .
           go to     nor-rec-log-100.
       nor-rec-log-240.
           move      zero                 to   rf-ost-tot-doc         .
           move      zero                 to   rf-ost-tra-cur         .
           move      spaces               to   rf-ost-cau-tra         .
           move      spaces               to   rf-ost-asp-ben         .
           move      zero                 to   rf-ost-num-col         .
           move      zero                 to   rf-ost-pes-tot         .
           move      zero                 to   rf-ost-dat-itr         .
           move      zero                 to   rf-ost-ora-itr         .
           move      zero                 to   rf-ost-cod-vet         .
           move      zero                 to   rf-ost-cod-vt2         .
           move      zero                 to   rf-ost-cod-vt3         .
           move      zero                 to   rf-ost-cod-aps         .
           move      zero                 to   rf-ost-ctr-stp         .
           move      spaces               to   rf-ost-flg-sch         .
           move      spaces               to   rf-ost-flg-blo         .
           move      spaces               to   rf-ost-flg-nbl         .
           move      spaces               to   rf-ost-flg-pul         .
           move      spaces               to   rf-ost-flg-rfp         .
           move      zero                 to   rf-ost-prt-mag         .
           move      zero                 to   rf-ost-cod-rsm         .
           move      spaces               to   rf-ost-bcd-tip         .
           move      spaces               to   rf-ost-alx-exp         .
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
           move      rf-ost-ide-dat       to   fil-ide-dat            .
           move      rf-ost-ide-ute       to   fil-ide-ute            .
           move      rf-ost-ide-fas       to   fil-ide-fas            .
           move      rf-ost-num-prt       to   fil-num-prt            .
           move      rf-ost-cod-tms       to   fil-cod-tms            .
           move      rf-ost-cod-dpz       to   fil-cod-dpz            .
           move      rf-ost-dat-doc       to   fil-dat-doc            .
           move      rf-ost-tip-arc       to   fil-tip-arc            .
           move      rf-ost-cod-arc       to   fil-cod-arc            .
           move      rf-ost-dpz-arc       to   fil-dpz-arc            .
           move      rf-ost-tip-frn       to   fil-tip-frn            .
           move      rf-ost-arc-plf       to   fil-arc-plf            .
           move      rf-ost-dpz-plf       to   fil-dpz-plf            .
           move      rf-ost-tip-ftz       to   fil-tip-ftz            .
           move      rf-ost-tip-ids       to   fil-tip-ids            .
           move      rf-ost-cod-lng       to   fil-cod-lng            .
           move      rf-ost-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-ost-dec-vpf       to   fil-dec-vpf            .
           move      rf-ost-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-ost-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-ost-ass-iva       to   fil-ass-iva            .
           move      rf-ost-ctp-ven       to   fil-ctp-ven            .
           move      rf-ost-fat-sep       to   fil-fat-sep            .
           move      rf-ost-inl-dcm       to   fil-inl-dcm            .
           move      rf-ost-inl-pgt       to   fil-inl-pgt            .
           move      rf-ost-cod-lst       to   fil-cod-lst            .
           move      rf-ost-csr-aac       to   fil-csr-aac            .
           move      rf-ost-psr-aac (1)   to   fil-psr-aac (1)        .
           move      rf-ost-psr-aac (2)   to   fil-psr-aac (2)        .
           move      rf-ost-psr-aac (3)   to   fil-psr-aac (3)        .
           move      rf-ost-psr-aac (4)   to   fil-psr-aac (4)        .
           move      rf-ost-psr-aac (5)   to   fil-psr-aac (5)        .
           move      rf-ost-csc-aac       to   fil-csc-aac            .
           move      rf-ost-psc-aac       to   fil-psc-aac            .
           move      rf-ost-cpv-aac       to   fil-cpv-aac            .
           move      rf-ost-ppv-aac (1)   to   fil-ppv-aac (1)        .
           move      rf-ost-ppv-aac (2)   to   fil-ppv-aac (2)        .
           move      rf-ost-ppv-aac (3)   to   fil-ppv-aac (3)        .
           move      rf-ost-voc-des (1)   to   fil-voc-des (1)        .
           move      rf-ost-voc-des (2)   to   fil-voc-des (2)        .
           move      rf-ost-voc-des (3)   to   fil-voc-des (3)        .
           move      rf-ost-voc-des (4)   to   fil-voc-des (4)        .
           move      rf-ost-voc-des (5)   to   fil-voc-des (5)        .
           move      rf-ost-voc-des (6)   to   fil-voc-des (6)        .
           move      rf-ost-cod-fop       to   fil-cod-fop            .
           move      rf-ost-scp-aap       to   fil-scp-aap            .
           move      rf-ost-cod-abi       to   fil-cod-abi            .
           move      rf-ost-cod-cab       to   fil-cod-cab            .
           move      rf-ost-ccc-app       to   fil-ccc-app            .
           move      rf-ost-nos-ban       to   fil-nos-ban            .
           move      rf-ost-nos-ccp       to   fil-nos-ccp            .
           move      rf-ost-add-spi       to   fil-add-spi            .
           move      rf-ost-add-spb       to   fil-add-spb            .
           move      rf-ost-ipr-iel       to   fil-ipr-iel            .
           move      rf-ost-pag-dsm       to   fil-pag-dsm            .
           move      rf-ost-pag-qaf       to   fil-pag-qaf            .
           move      rf-ost-pag-act       to   fil-pag-act            .
           move      rf-ost-cod-age       to   fil-cod-age            .
           move      rf-ost-fsp-doc       to   fil-fsp-doc            .
           move      rf-ost-pvf-age       to   fil-pvf-age            .
           move      rf-ost-tip-vpa       to   fil-tip-vpa            .
           move      rf-ost-cpv-aaa       to   fil-cpv-aaa            .
           move      rf-ost-ppv-aaa (1)   to   fil-ppv-aaa (1)        .
           move      rf-ost-ppv-aaa (2)   to   fil-ppv-aaa (2)        .
           move      rf-ost-ppv-aaa (3)   to   fil-ppv-aaa (3)        .
           move      rf-ost-cod-ime       to   fil-cod-ime            .
           move      rf-ost-pvf-ime       to   fil-pvf-ime            .
           move      rf-ost-tot-rig (1)   to   fil-tot-rig (1)        .
           move      rf-ost-tot-rig (2)   to   fil-tot-rig (2)        .
           move      rf-ost-tot-rig (3)   to   fil-tot-rig (3)        .
           move      rf-ost-tot-rig (4)   to   fil-tot-rig (4)        .
           move      rf-ost-tot-rig (5)   to   fil-tot-rig (5)        .
           move      rf-ost-tot-rig (6)   to   fil-tot-rig (6)        .
           move      rf-ost-tot-rig (7)   to   fil-tot-rig (7)        .
           move      rf-ost-tot-rig (8)   to   fil-tot-rig (8)        .
           move      rf-ost-tot-rig (9)   to   fil-tot-rig (9)        .
           move      rf-ost-tot-scc       to   fil-tot-scc            .
           move      rf-ost-per-scc       to   fil-per-scc            .
           move      rf-ost-tot-scp       to   fil-tot-scp            .
           move      rf-ost-per-scp       to   fil-per-scp            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-340.
           move      rf-ost-spe-snx (w-c01)
                                          to   fil-spe-snx (w-c01)    .
           move      rf-ost-spe-mad (w-c01)
                                          to   fil-spe-mad (w-c01)    .
           move      rf-ost-spe-per (w-c01)
                                          to   fil-spe-per (w-c01)    .
           move      rf-ost-spe-ibl (w-c01)
                                          to   fil-spe-ibl (w-c01)    .
           move      zero                 to   w-c02                  .
       cmp-log-fis-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to cmp-log-fis-320.
           move      rf-ost-ibx-spe
                     (w-c01, w-c02)       to   fil-ibx-spe
                                              (w-c01, w-c02)          .
           go to     cmp-log-fis-300.
       cmp-log-fis-320.
           move      rf-ost-spe-imp (w-c01)
                                          to   fil-spe-imp (w-c01)    .
           go to     cmp-log-fis-200.
       cmp-log-fis-340.
           move      rf-ost-tot-doc       to   fil-tot-doc            .
           move      rf-ost-tra-cur       to   fil-tra-cur            .
           move      rf-ost-cau-tra       to   fil-cau-tra            .
           move      rf-ost-asp-ben       to   fil-asp-ben            .
           move      rf-ost-num-col       to   fil-num-col            .
           move      rf-ost-pes-tot       to   fil-pes-tot            .
           move      rf-ost-dat-itr       to   fil-dat-itr            .
           move      rf-ost-ora-itr       to   fil-ora-itr            .
           move      rf-ost-cod-vet       to   fil-cod-vet            .
           move      rf-ost-cod-vt2       to   fil-cod-vt2            .
           move      rf-ost-cod-vt3       to   fil-cod-vt3            .
           move      rf-ost-cod-aps       to   fil-cod-aps            .
           move      rf-ost-ctr-stp       to   fil-ctr-stp            .
           move      rf-ost-flg-sch       to   fil-flg-sch            .
           move      rf-ost-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-ost-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-ost-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-ost-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-ost-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-ost-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-ost-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-ost-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-ost-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-ost-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-ost-flg-pul       to   fil-flg-pul            .
           move      rf-ost-flg-rfp       to   fil-flg-rfp            .
           move      rf-ost-prt-mag       to   fil-prt-mag            .
           move      rf-ost-cod-rsm       to   fil-cod-rsm            .
           move      rf-ost-bcd-tip       to   fil-bcd-tip            .
           move      rf-ost-alx-exp       to   fil-alx-exp            .
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
           move      rf-ost-num-prt       to   fil-num-prt-1          .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-ost-ide-dat       to   fil-ide-dat-2          .
           move      rf-ost-dat-doc       to   fil-dat-doc-2          .
           move      rf-ost-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-ost-cod-dpz       to   fil-cod-dpz-3          .
           move      rf-ost-tip-arc       to   fil-tip-arc-3          .
           move      rf-ost-cod-arc       to   fil-cod-arc-3          .
           move      rf-ost-dat-doc       to   fil-dat-doc-3          .
           move      rf-ost-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-ost-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-ost-cod-tms       to   fil-cod-tms-4          .
           move      rf-ost-dat-doc       to   fil-dat-doc-4          .
           move      rf-ost-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-ost-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-ost-flg-sch       to   fil-flg-sch-5          .
           move      rf-ost-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-ost-cod-dpz       to   fil-cod-dpz-6          .
           move      rf-ost-dat-doc       to   fil-dat-doc-6          .
           move      rf-ost-num-prt       to   fil-num-prt-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ost                 .
           move      fil-ide-dat          to   rf-ost-ide-dat         .
           move      fil-ide-ute          to   rf-ost-ide-ute         .
           move      fil-ide-fas          to   rf-ost-ide-fas         .
           move      fil-num-prt          to   rf-ost-num-prt         .
           move      fil-cod-tms          to   rf-ost-cod-tms         .
           move      fil-cod-dpz          to   rf-ost-cod-dpz         .
           move      fil-dat-doc          to   rf-ost-dat-doc         .
           move      fil-tip-arc          to   rf-ost-tip-arc         .
           move      fil-cod-arc          to   rf-ost-cod-arc         .
           move      fil-dpz-arc          to   rf-ost-dpz-arc         .
           move      fil-tip-frn          to   rf-ost-tip-frn         .
           move      fil-arc-plf          to   rf-ost-arc-plf         .
           move      fil-dpz-plf          to   rf-ost-dpz-plf         .
           move      fil-tip-ftz          to   rf-ost-tip-ftz         .
           move      fil-tip-ids          to   rf-ost-tip-ids         .
           move      fil-cod-lng          to   rf-ost-cod-lng         .
           move      fil-sgl-vpf          to   rf-ost-sgl-vpf         .
           move      fil-dec-vpf          to   rf-ost-dec-vpf         .
           move      fil-tdc-vpf          to   rf-ost-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-ost-cdc-vpf         .
           move      fil-ass-iva          to   rf-ost-ass-iva         .
           move      fil-ctp-ven          to   rf-ost-ctp-ven         .
           move      fil-fat-sep          to   rf-ost-fat-sep         .
           move      fil-inl-dcm          to   rf-ost-inl-dcm         .
           move      fil-inl-pgt          to   rf-ost-inl-pgt         .
           move      fil-cod-lst          to   rf-ost-cod-lst         .
           move      fil-csr-aac          to   rf-ost-csr-aac         .
           move      fil-psr-aac (1)      to   rf-ost-psr-aac (1)     .
           move      fil-psr-aac (2)      to   rf-ost-psr-aac (2)     .
           move      fil-psr-aac (3)      to   rf-ost-psr-aac (3)     .
           move      fil-psr-aac (4)      to   rf-ost-psr-aac (4)     .
           move      fil-psr-aac (5)      to   rf-ost-psr-aac (5)     .
           move      fil-csc-aac          to   rf-ost-csc-aac         .
           move      fil-psc-aac          to   rf-ost-psc-aac         .
           move      fil-cpv-aac          to   rf-ost-cpv-aac         .
           move      fil-ppv-aac (1)      to   rf-ost-ppv-aac (1)     .
           move      fil-ppv-aac (2)      to   rf-ost-ppv-aac (2)     .
           move      fil-ppv-aac (3)      to   rf-ost-ppv-aac (3)     .
           move      fil-voc-des (1)      to   rf-ost-voc-des (1)     .
           move      fil-voc-des (2)      to   rf-ost-voc-des (2)     .
           move      fil-voc-des (3)      to   rf-ost-voc-des (3)     .
           move      fil-voc-des (4)      to   rf-ost-voc-des (4)     .
           move      fil-voc-des (5)      to   rf-ost-voc-des (5)     .
           move      fil-voc-des (6)      to   rf-ost-voc-des (6)     .
           move      fil-cod-fop          to   rf-ost-cod-fop         .
           move      fil-scp-aap          to   rf-ost-scp-aap         .
           move      fil-cod-abi          to   rf-ost-cod-abi         .
           move      fil-cod-cab          to   rf-ost-cod-cab         .
           move      fil-ccc-app          to   rf-ost-ccc-app         .
           move      fil-nos-ban          to   rf-ost-nos-ban         .
           move      fil-nos-ccp          to   rf-ost-nos-ccp         .
           move      fil-add-spi          to   rf-ost-add-spi         .
           move      fil-add-spb          to   rf-ost-add-spb         .
           move      fil-ipr-iel          to   rf-ost-ipr-iel         .
           move      fil-pag-dsm          to   rf-ost-pag-dsm         .
           move      fil-pag-qaf          to   rf-ost-pag-qaf         .
           move      fil-pag-act          to   rf-ost-pag-act         .
           move      fil-cod-age          to   rf-ost-cod-age         .
           move      fil-fsp-doc          to   rf-ost-fsp-doc         .
           move      fil-pvf-age          to   rf-ost-pvf-age         .
           move      fil-tip-vpa          to   rf-ost-tip-vpa         .
           move      fil-cpv-aaa          to   rf-ost-cpv-aaa         .
           move      fil-ppv-aaa (1)      to   rf-ost-ppv-aaa (1)     .
           move      fil-ppv-aaa (2)      to   rf-ost-ppv-aaa (2)     .
           move      fil-ppv-aaa (3)      to   rf-ost-ppv-aaa (3)     .
           move      fil-cod-ime          to   rf-ost-cod-ime         .
           move      fil-pvf-ime          to   rf-ost-pvf-ime         .
           move      fil-tot-rig (1)      to   rf-ost-tot-rig (1)     .
           move      fil-tot-rig (2)      to   rf-ost-tot-rig (2)     .
           move      fil-tot-rig (3)      to   rf-ost-tot-rig (3)     .
           move      fil-tot-rig (4)      to   rf-ost-tot-rig (4)     .
           move      fil-tot-rig (5)      to   rf-ost-tot-rig (5)     .
           move      fil-tot-rig (6)      to   rf-ost-tot-rig (6)     .
           move      fil-tot-rig (7)      to   rf-ost-tot-rig (7)     .
           move      fil-tot-rig (8)      to   rf-ost-tot-rig (8)     .
           move      fil-tot-rig (9)      to   rf-ost-tot-rig (9)     .
           move      fil-tot-scc          to   rf-ost-tot-scc         .
           move      fil-per-scc          to   rf-ost-per-scc         .
           move      fil-tot-scp          to   rf-ost-tot-scp         .
           move      fil-per-scp          to   rf-ost-per-scp         .
           move      zero                 to   w-c01                  .
       dec-fis-log-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-340.
           move      fil-spe-snx (w-c01)  to   rf-ost-spe-snx (w-c01) .
           move      fil-spe-mad (w-c01)  to   rf-ost-spe-mad (w-c01) .
           move      fil-spe-per (w-c01)  to   rf-ost-spe-per (w-c01) .
           move      fil-spe-ibl (w-c01)  to   rf-ost-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       dec-fis-log-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to dec-fis-log-320.
           move      fil-ibx-spe
                    (w-c01, w-c02)        to   rf-ost-ibx-spe
                                               (w-c01, w-c02)         .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      fil-spe-imp (w-c01)  to   rf-ost-spe-imp (w-c01) .
           go to     dec-fis-log-200.
       dec-fis-log-340.
           move      fil-tot-doc          to   rf-ost-tot-doc         .
           move      fil-tra-cur          to   rf-ost-tra-cur         .
           move      fil-asp-ben          to   rf-ost-asp-ben         .
           move      fil-num-col          to   rf-ost-num-col         .
           move      fil-pes-tot          to   rf-ost-pes-tot         .
           move      fil-dat-itr          to   rf-ost-dat-itr         .
           move      fil-ora-itr          to   rf-ost-ora-itr         .
           move      fil-cod-vet          to   rf-ost-cod-vet         .
           move      fil-cod-vt2          to   rf-ost-cod-vt2         .
           move      fil-cod-vt3          to   rf-ost-cod-vt3         .
           move      fil-cod-aps          to   rf-ost-cod-aps         .
           move      fil-ctr-stp          to   rf-ost-ctr-stp         .
           move      fil-flg-sch          to   rf-ost-flg-sch         .
           move      fil-flg-blx (1)      to   rf-ost-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-ost-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-ost-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-ost-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-ost-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-ost-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-ost-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-ost-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-ost-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-ost-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-ost-flg-pul         .
           move      fil-flg-rfp          to   rf-ost-flg-rfp         .
           move      fil-prt-mag          to   rf-ost-prt-mag         .
           move      fil-cod-rsm          to   rf-ost-cod-rsm         .
           move      fil-bcd-tip          to   rf-ost-bcd-tip         .
           move      fil-alx-exp          to   rf-ost-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ost               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ost
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

