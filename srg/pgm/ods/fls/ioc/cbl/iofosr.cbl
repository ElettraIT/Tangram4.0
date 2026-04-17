       Identification Division.
       Program-Id.                                 iofosr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File osr                         *
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
                   15  fil-num-prt-1      pic  9(11)       comp-3     .
                   15  fil-num-prg-1      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz-2      pic  9(02)                  .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
                   15  fil-dat-doc-2      pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFORC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dpz-3      pic  9(02)                  .
                   15  fil-coc-prt-3      pic  9(11)       comp-3     .
                   15  fil-coc-prg-3      pic  9(05)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-flg-rch-4      pic  x(01)                  .
                   15  fil-tip-mag-4      pic  9(02)                  .
                   15  fil-num-pro-4      pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : RCHARC                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-flg-rch-5      pic  x(01)                  .
                   15  fil-tip-arc-5      pic  x(01)                  .
                   15  fil-cod-arc-5      pic  9(07)       comp-3     .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
                   15  fil-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-num-prt            pic  9(11)       comp-3     .
               10  fil-num-prg            pic  9(05)       comp-3     .
               10  fil-cod-tms            pic  x(05)                  .
               10  fil-cod-dpz            pic  9(02)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-tip-arc            pic  x(01)                  .
               10  fil-cod-arc            pic  9(07)       comp-3     .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-bld-flb            pic  9(01)                  .
               10  fil-bld-tpb            pic  9(01)                  .
               10  fil-bld-rgb            pic  9(01)                  .
               10  fil-tip-rig            pic  x(05)                  .
               10  fil-tip-mag            pic  9(02)                  .
               10  fil-num-pro            pic  9(07)       comp-3     .
               10  fil-alf-pro            pic  x(14)                  .
               10  fil-sgl-vrn            pic  x(14)                  .
               10  fil-cop-scl            pic  x(14)                  .
               10  fil-des-ext            pic  9(01)                  .
               10  fil-des-rig            pic  x(40)                  .
               10  fil-tip-pro            pic  9(02)                  .
               10  fil-cod-iva            pic  9(05)       comp-3     .
               10  fil-ctp-ven            pic  9(07)       comp-3     .
               10  fil-umi-ven            pic  x(03)                  .
               10  fil-dec-qta            pic  9(01)                  .
               10  fil-qta-ven            pic s9(10)v9(03) comp-3     .
               10  fil-snx-2qt            pic  9(01)                  .
               10  fil-dec-2qt            pic  9(01)                  .
               10  fil-qta-a02            pic s9(10)v9(03) comp-3     .
               10  fil-snx-3qt            pic  9(01)                  .
               10  fil-dec-3qt            pic  9(01)                  .
               10  fil-qta-a03            pic s9(10)v9(03) comp-3     .
               10  fil-dec-prz            pic  9(01)                  .
               10  fil-vps.
                   15  fil-sgl-vps        pic  x(03)                  .
                   15  fil-dec-vps        pic  9(01)                  .
                   15  fil-tdc-vps        pic  x(01)                  .
                   15  fil-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  fil-prz-lrs            pic  9(09)       comp-3     .
               10  fil-prz-nts            pic  9(09)       comp-3     .
               10  fil-vpp.
                   15  fil-sgl-vpp        pic  x(03)                  .
                   15  fil-dec-vpp        pic  9(01)                  .
                   15  fil-tdc-vpp        pic  x(01)                  .
                   15  fil-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  fil-prz-ven            pic  9(09)       comp-3     .
               10  fil-snx-2pz            pic  9(01)                  .
               10  fil-prz-a02            pic  9(09)       comp-3     .
               10  fil-vpl.
                   15  fil-sgl-vpl        pic  x(03)                  .
                   15  fil-dec-vpl        pic  9(01)                  .
                   15  fil-tdc-vpl        pic  x(01)                  .
                   15  fil-prz-vpl        pic  9(09)       comp-3     .
                   15  fil-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  fil-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  fil-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  fil-tlm-vpl        pic  x(01)                  .
                   15  fil-map-vpl        pic  x(01)                  .
               10  fil-epz-rgf            pic  9(01)                  .
               10  fil-csr-aap            pic  9(05)       comp-3     .
               10  fil-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-prz-net            pic  9(09)       comp-3     .
               10  fil-epz-pes            pic  9(02)                  .
               10  fil-vpc.
                   15  fil-sgl-vpc        pic  x(03)                  .
                   15  fil-dec-vpc        pic  9(01)                  .
                   15  fil-tdc-vpc        pic  x(01)                  .
                   15  fil-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  fil-dec-cos            pic  9(01)                  .
               10  fil-cos-rif            pic  9(09)       comp-3     .
               10  fil-imp-rig            pic s9(11)       comp-3     .
               10  fil-iau-rig            pic s9(11)       comp-3     .
               10  fil-cpv-aap            pic  9(05)       comp-3     .
               10  fil-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-fsp-rig            pic  9(02)                  .
               10  fil-cpv-rig            pic  9(05)       comp-3     .
               10  fil-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-pvf-rig            pic s9(11)       comp-3     .
               10  fil-ocl-dat            pic  9(07)       comp-3     .
               10  fil-ocl-num            pic  x(10)                  .
               10  fil-cmc-tip            pic  x(05)                  .
               10  fil-cmc-dat            pic  9(07)       comp-3     .
               10  fil-cmc-num            pic  9(11)       comp-3     .
               10  fil-coc-tip            pic  x(05)                  .
               10  fil-coc-dat            pic  9(07)       comp-3     .
               10  fil-coc-num            pic  9(11)       comp-3     .
               10  fil-coc-prt            pic  9(11)       comp-3     .
               10  fil-coc-prg            pic  9(05)       comp-3     .
               10  fil-coc-fzs            pic  x(01)                  .
               10  fil-flg-rch            pic  x(01)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-flg-puq            pic  x(01)                  .
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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-prt-1      pic  9(11)       comp-3     .
                   15  pul-num-prg-1      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-cod-dpz-2      pic  9(02)                  .
                   15  pul-tip-mag-2      pic  9(02)                  .
                   15  pul-num-pro-2      pic  9(07)       comp-3     .
                   15  pul-dat-doc-2      pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFods                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dpz-3      pic  9(02)                  .
                   15  pul-coc-prt-3      pic  9(11)       comp-3     .
                   15  pul-coc-prg-3      pic  9(05)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-flg-rch-4      pic  x(01)                  .
                   15  pul-tip-mag-4      pic  9(02)                  .
                   15  pul-num-pro-4      pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : RCHARC                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-flg-rch-5      pic  x(01)                  .
                   15  pul-tip-arc-5      pic  x(01)                  .
                   15  pul-cod-arc-5      pic  9(07)       comp-3     .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
                   15  pul-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-num-prt            pic  9(11)       comp-3     .
               10  pul-num-prg            pic  9(05)       comp-3     .
               10  pul-cod-tms            pic  x(05)                  .
               10  pul-cod-dpz            pic  9(02)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-tip-arc            pic  x(01)                  .
               10  pul-cod-arc            pic  9(07)       comp-3     .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-bld-flb            pic  9(01)                  .
               10  pul-bld-tpb            pic  9(01)                  .
               10  pul-bld-rgb            pic  9(01)                  .
               10  pul-tip-rig            pic  x(05)                  .
               10  pul-tip-mag            pic  9(02)                  .
               10  pul-num-pro            pic  9(07)       comp-3     .
               10  pul-alf-pro            pic  x(14)                  .
               10  pul-sgl-vrn            pic  x(14)                  .
               10  pul-cop-scl            pic  x(14)                  .
               10  pul-des-ext            pic  9(01)                  .
               10  pul-des-rig            pic  x(40)                  .
               10  pul-tip-pro            pic  9(02)                  .
               10  pul-cod-iva            pic  9(05)       comp-3     .
               10  pul-ctp-ven            pic  9(07)       comp-3     .
               10  pul-umi-ven            pic  x(03)                  .
               10  pul-dec-qta            pic  9(01)                  .
               10  pul-qta-ven            pic s9(10)v9(03) comp-3     .
               10  pul-snx-2qt            pic  9(01)                  .
               10  pul-dec-2qt            pic  9(01)                  .
               10  pul-qta-a02            pic s9(10)v9(03) comp-3     .
               10  pul-snx-3qt            pic  9(01)                  .
               10  pul-dec-3qt            pic  9(01)                  .
               10  pul-qta-a03            pic s9(10)v9(03) comp-3     .
               10  pul-dec-prz            pic  9(01)                  .
               10  pul-vps.
                   15  pul-sgl-vps        pic  x(03)                  .
                   15  pul-dec-vps        pic  9(01)                  .
                   15  pul-tdc-vps        pic  x(01)                  .
                   15  pul-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  pul-prz-lrs            pic  9(09)       comp-3     .
               10  pul-prz-nts            pic  9(09)       comp-3     .
               10  pul-vpp.
                   15  pul-sgl-vpp        pic  x(03)                  .
                   15  pul-dec-vpp        pic  9(01)                  .
                   15  pul-tdc-vpp        pic  x(01)                  .
                   15  pul-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  pul-prz-ven            pic  9(09)       comp-3     .
               10  pul-snx-2pz            pic  9(01)                  .
               10  pul-prz-a02            pic  9(09)       comp-3     .
               10  pul-vpl.
                   15  pul-sgl-vpl        pic  x(03)                  .
                   15  pul-dec-vpl        pic  9(01)                  .
                   15  pul-tdc-vpl        pic  x(01)                  .
                   15  pul-prz-vpl        pic  9(09)       comp-3     .
                   15  pul-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  pul-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  pul-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  pul-tlm-vpl        pic  x(01)                  .
                   15  pul-map-vpl        pic  x(01)                  .
               10  pul-epz-rgf            pic  9(01)                  .
               10  pul-csr-aap            pic  9(05)       comp-3     .
               10  pul-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-prz-net            pic  9(09)       comp-3     .
               10  pul-epz-pes            pic  9(02)                  .
               10  pul-vpc.
                   15  pul-sgl-vpc        pic  x(03)                  .
                   15  pul-dec-vpc        pic  9(01)                  .
                   15  pul-tdc-vpc        pic  x(01)                  .
                   15  pul-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  pul-dec-cos            pic  9(01)                  .
               10  pul-cos-rif            pic  9(09)       comp-3     .
               10  pul-imp-rig            pic s9(11)       comp-3     .
               10  pul-iau-rig            pic s9(11)       comp-3     .
               10  pul-cpv-aap            pic  9(05)       comp-3     .
               10  pul-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-fsp-rig            pic  9(02)                  .
               10  pul-cpv-rig            pic  9(05)       comp-3     .
               10  pul-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-pvf-rig            pic s9(11)       comp-3     .
               10  pul-ocl-dat            pic  9(07)       comp-3     .
               10  pul-ocl-num            pic  x(10)                  .
               10  pul-cmc-tip            pic  x(05)                  .
               10  pul-cmc-dat            pic  9(07)       comp-3     .
               10  pul-cmc-num            pic  9(11)       comp-3     .
               10  pul-coc-tip            pic  x(05)                  .
               10  pul-coc-dat            pic  9(07)       comp-3     .
               10  pul-coc-num            pic  9(11)       comp-3     .
               10  pul-coc-prt            pic  9(11)       comp-3     .
               10  pul-coc-prg            pic  9(05)       comp-3     .
               10  pul-coc-fzs            pic  x(01)                  .
               10  pul-flg-rch            pic  x(01)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-flg-puq            pic  x(01)                  .
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
      *        * Sigla del file                                        *
      *        *-------------------------------------------------------*
           02  i-ide-sdf                  pic  x(04) value
                     "osr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/ods/fls/ioc/obj/iofosr              "       .

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
                            "MAGDAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RIFORC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RCHMAG    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RCHARC    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    5      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [osr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .

      ******************************************************************
       Procedure Division                using f rf-osr               .
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
           move      spaces               to   rf-osr                 .
           move      zero                 to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      spaces               to   rf-osr-cod-tms         .
           move      zero                 to   rf-osr-cod-dpz         .
           move      zero                 to   rf-osr-dat-doc         .
           move      spaces               to   rf-osr-tip-arc         .
           move      zero                 to   rf-osr-cod-arc         .
           move      spaces               to   rf-osr-dpz-arc         .
           move      spaces               to   rf-osr-cod-lng         .
           move      spaces               to   rf-osr-sgl-vpf         .
           move      zero                 to   rf-osr-dec-vpf         .
           move      spaces               to   rf-osr-tdc-vpf         .
           move      zero                 to   rf-osr-cdc-vpf         .
           move      zero                 to   rf-osr-bld-flb         .
           move      zero                 to   rf-osr-bld-tpb         .
           move      zero                 to   rf-osr-bld-rgb         .
           move      spaces               to   rf-osr-tip-rig         .
           move      zero                 to   rf-osr-tip-mag         .
           move      zero                 to   rf-osr-num-pro         .
           move      spaces               to   rf-osr-alf-pro         .
           move      spaces               to   rf-osr-sgl-vrn         .
           move      spaces               to   rf-osr-cop-scl         .
           move      zero                 to   rf-osr-des-ext         .
           move      spaces               to   rf-osr-des-rig         .
           move      zero                 to   rf-osr-tip-pro         .
           move      zero                 to   rf-osr-cod-iva         .
           move      zero                 to   rf-osr-ctp-ven         .
           move      spaces               to   rf-osr-umi-ven         .
           move      zero                 to   rf-osr-dec-qta         .
           move      zero                 to   rf-osr-qta-ven         .
           move      zero                 to   rf-osr-snx-2qt         .
           move      zero                 to   rf-osr-dec-2qt         .
           move      zero                 to   rf-osr-qta-a02         .
           move      zero                 to   rf-osr-snx-3qt         .
           move      zero                 to   rf-osr-dec-3qt         .
           move      zero                 to   rf-osr-qta-a03         .
           move      zero                 to   rf-osr-dec-prz         .
           move      spaces               to   rf-osr-sgl-vps         .
           move      zero                 to   rf-osr-dec-vps         .
           move      spaces               to   rf-osr-tdc-vps         .
           move      zero                 to   rf-osr-cdc-vps         .
           move      zero                 to   rf-osr-prz-lrs         .
           move      zero                 to   rf-osr-prz-nts         .
           move      spaces               to   rf-osr-sgl-vpp         .
           move      zero                 to   rf-osr-dec-vpp         .
           move      spaces               to   rf-osr-tdc-vpp         .
           move      zero                 to   rf-osr-cdc-vpp         .
           move      zero                 to   rf-osr-prz-ven         .
           move      zero                 to   rf-osr-snx-2pz         .
           move      zero                 to   rf-osr-prz-a02         .
           move      spaces               to   rf-osr-sgl-vpl         .
           move      zero                 to   rf-osr-dec-vpl         .
           move      spaces               to   rf-osr-tdc-vpl         .
           move      zero                 to   rf-osr-prz-vpl         .
           move      zero                 to   rf-osr-cdc-vpl         .
           move      zero                 to   rf-osr-ccr-vpl         .
           move      zero                 to   rf-osr-plm-vpl         .
           move      spaces               to   rf-osr-tlm-vpl         .
           move      spaces               to   rf-osr-map-vpl         .
           move      zero                 to   rf-osr-epz-rgf         .
           move      zero                 to   rf-osr-csr-aap         .
           move      zero                 to   rf-osr-psr-aap (1)     .
           move      zero                 to   rf-osr-psr-aap (2)     .
           move      zero                 to   rf-osr-psr-aap (3)     .
           move      zero                 to   rf-osr-psr-aap (4)     .
           move      zero                 to   rf-osr-psr-aap (5)     .
           move      zero                 to   rf-osr-per-scr (1)     .
           move      zero                 to   rf-osr-per-scr (2)     .
           move      zero                 to   rf-osr-per-scr (3)     .
           move      zero                 to   rf-osr-per-scr (4)     .
           move      zero                 to   rf-osr-per-scr (5)     .
           move      zero                 to   rf-osr-prz-net         .
           move      zero                 to   rf-osr-epz-pes         .
           move      spaces               to   rf-osr-sgl-vpc         .
           move      zero                 to   rf-osr-dec-vpc         .
           move      spaces               to   rf-osr-tdc-vpc         .
           move      zero                 to   rf-osr-cdc-vpc         .
           move      zero                 to   rf-osr-dec-cos         .
           move      zero                 to   rf-osr-cos-rif         .
           move      zero                 to   rf-osr-imp-rig         .
           move      zero                 to   rf-osr-iau-rig         .
           move      zero                 to   rf-osr-cpv-aap         .
           move      zero                 to   rf-osr-ppv-aap (1)     .
           move      zero                 to   rf-osr-ppv-aap (2)     .
           move      zero                 to   rf-osr-ppv-aap (3)     .
           move      zero                 to   rf-osr-fsp-rig         .
           move      zero                 to   rf-osr-cpv-rig         .
           move      zero                 to   rf-osr-ppv-rig (1)     .
           move      zero                 to   rf-osr-ppv-rig (2)     .
           move      zero                 to   rf-osr-ppv-rig (3)     .
           move      zero                 to   rf-osr-pvf-rig         .
           move      zero                 to   rf-osr-ocl-dat         .
           move      spaces               to   rf-osr-ocl-num         .
           move      spaces               to   rf-osr-cmc-tip         .
           move      zero                 to   rf-osr-cmc-dat         .
           move      zero                 to   rf-osr-cmc-num         .
           move      spaces               to   rf-osr-coc-tip         .
           move      zero                 to   rf-osr-coc-dat         .
           move      zero                 to   rf-osr-coc-num         .
           move      zero                 to   rf-osr-coc-prt         .
           move      zero                 to   rf-osr-coc-prg         .
           move      spaces               to   rf-osr-coc-fzs         .
           move      spaces               to   rf-osr-flg-rch         .
           move      spaces               to   rf-osr-flg-blx (1)     .
           move      spaces               to   rf-osr-flg-blx (2)     .
           move      spaces               to   rf-osr-flg-blx (3)     .
           move      spaces               to   rf-osr-flg-blx (4)     .
           move      spaces               to   rf-osr-flg-blx (5)     .
           move      spaces               to   rf-osr-flg-blx (6)     .
           move      spaces               to   rf-osr-flg-blx (7)     .
           move      spaces               to   rf-osr-flg-nbx (1)     .
           move      spaces               to   rf-osr-flg-nbx (2)     .
           move      spaces               to   rf-osr-flg-nbx (3)     .
           move      spaces               to   rf-osr-flg-pul         .
           move      spaces               to   rf-osr-flg-puq         .
           move      spaces               to   rf-osr-alx-exp         .
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
           move      rf-osr-num-prt       to   fil-num-prt            .
           move      rf-osr-num-prg       to   fil-num-prg            .
           move      rf-osr-cod-tms       to   fil-cod-tms            .
           move      rf-osr-cod-dpz       to   fil-cod-dpz            .
           move      rf-osr-dat-doc       to   fil-dat-doc            .
           move      rf-osr-tip-arc       to   fil-tip-arc            .
           move      rf-osr-cod-arc       to   fil-cod-arc            .
           move      rf-osr-dpz-arc       to   fil-dpz-arc            .
           move      rf-osr-cod-lng       to   fil-cod-lng            .
           move      rf-osr-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-osr-dec-vpf       to   fil-dec-vpf            .
           move      rf-osr-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-osr-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-osr-bld-flb       to   fil-bld-flb            .
           move      rf-osr-bld-tpb       to   fil-bld-tpb            .
           move      rf-osr-bld-rgb       to   fil-bld-rgb            .
           move      rf-osr-tip-rig       to   fil-tip-rig            .
           move      rf-osr-tip-mag       to   fil-tip-mag            .
           move      rf-osr-num-pro       to   fil-num-pro            .
           move      rf-osr-alf-pro       to   fil-alf-pro            .
           move      rf-osr-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-osr-cop-scl       to   fil-cop-scl            .
           move      rf-osr-des-ext       to   fil-des-ext            .
           move      rf-osr-des-rig       to   fil-des-rig            .
           move      rf-osr-tip-pro       to   fil-tip-pro            .
           move      rf-osr-cod-iva       to   fil-cod-iva            .
           move      rf-osr-ctp-ven       to   fil-ctp-ven            .
           move      rf-osr-umi-ven       to   fil-umi-ven            .
           move      rf-osr-dec-qta       to   fil-dec-qta            .
           move      rf-osr-qta-ven       to   fil-qta-ven            .
           move      rf-osr-snx-2qt       to   fil-snx-2qt            .
           move      rf-osr-dec-2qt       to   fil-dec-2qt            .
           move      rf-osr-qta-a02       to   fil-qta-a02            .
           move      rf-osr-snx-3qt       to   fil-snx-3qt            .
           move      rf-osr-dec-3qt       to   fil-dec-3qt            .
           move      rf-osr-qta-a03       to   fil-qta-a03            .
           move      rf-osr-dec-prz       to   fil-dec-prz            .
           move      rf-osr-sgl-vps       to   fil-sgl-vps            .
           move      rf-osr-dec-vps       to   fil-dec-vps            .
           move      rf-osr-tdc-vps       to   fil-tdc-vps            .
           move      rf-osr-cdc-vps       to   fil-cdc-vps            .
           move      rf-osr-prz-lrs       to   fil-prz-lrs            .
           move      rf-osr-prz-nts       to   fil-prz-nts            .
           move      rf-osr-sgl-vpp       to   fil-sgl-vpp            .
           move      rf-osr-dec-vpp       to   fil-dec-vpp            .
           move      rf-osr-tdc-vpp       to   fil-tdc-vpp            .
           move      rf-osr-cdc-vpp       to   fil-cdc-vpp            .
           move      rf-osr-prz-ven       to   fil-prz-ven            .
           move      rf-osr-snx-2pz       to   fil-snx-2pz            .
           move      rf-osr-prz-a02       to   fil-prz-a02            .
           move      rf-osr-sgl-vpl       to   fil-sgl-vpl            .
           move      rf-osr-dec-vpl       to   fil-dec-vpl            .
           move      rf-osr-tdc-vpl       to   fil-tdc-vpl            .
           move      rf-osr-prz-vpl       to   fil-prz-vpl            .
           move      rf-osr-cdc-vpl       to   fil-cdc-vpl            .
           move      rf-osr-ccr-vpl       to   fil-ccr-vpl            .
           move      rf-osr-plm-vpl       to   fil-plm-vpl            .
           move      rf-osr-tlm-vpl       to   fil-tlm-vpl            .
           move      rf-osr-map-vpl       to   fil-map-vpl            .
           move      rf-osr-epz-rgf       to   fil-epz-rgf            .
           move      rf-osr-csr-aap       to   fil-csr-aap            .
           move      rf-osr-psr-aap (1)   to   fil-psr-aap (1)        .
           move      rf-osr-psr-aap (2)   to   fil-psr-aap (2)        .
           move      rf-osr-psr-aap (3)   to   fil-psr-aap (3)        .
           move      rf-osr-psr-aap (4)   to   fil-psr-aap (4)        .
           move      rf-osr-psr-aap (5)   to   fil-psr-aap (5)        .
           move      rf-osr-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-osr-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-osr-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-osr-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-osr-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-osr-prz-net       to   fil-prz-net            .
           move      rf-osr-epz-pes       to   fil-epz-pes            .
           move      rf-osr-sgl-vpc       to   fil-sgl-vpc            .
           move      rf-osr-dec-vpc       to   fil-dec-vpc            .
           move      rf-osr-tdc-vpc       to   fil-tdc-vpc            .
           move      rf-osr-cdc-vpc       to   fil-cdc-vpc            .
           move      rf-osr-dec-cos       to   fil-dec-cos            .
           move      rf-osr-cos-rif       to   fil-cos-rif            .
           move      rf-osr-imp-rig       to   fil-imp-rig            .
           move      rf-osr-iau-rig       to   fil-iau-rig            .
           move      rf-osr-cpv-aap       to   fil-cpv-aap            .
           move      rf-osr-ppv-aap (1)   to   fil-ppv-aap (1)        .
           move      rf-osr-ppv-aap (2)   to   fil-ppv-aap (2)        .
           move      rf-osr-ppv-aap (3)   to   fil-ppv-aap (3)        .
           move      rf-osr-fsp-rig       to   fil-fsp-rig            .
           move      rf-osr-cpv-rig       to   fil-cpv-rig            .
           move      rf-osr-ppv-rig (1)   to   fil-ppv-rig (1)        .
           move      rf-osr-ppv-rig (2)   to   fil-ppv-rig (2)        .
           move      rf-osr-ppv-rig (3)   to   fil-ppv-rig (3)        .
           move      rf-osr-pvf-rig       to   fil-pvf-rig            .
           move      rf-osr-ocl-dat       to   fil-ocl-dat            .
           move      rf-osr-ocl-num       to   fil-ocl-num            .
           move      rf-osr-cmc-tip       to   fil-cmc-tip            .
           move      rf-osr-cmc-dat       to   fil-cmc-dat            .
           move      rf-osr-cmc-num       to   fil-cmc-num            .
           move      rf-osr-coc-tip       to   fil-coc-tip            .
           move      rf-osr-coc-dat       to   fil-coc-dat            .
           move      rf-osr-coc-num       to   fil-coc-num            .
           move      rf-osr-coc-prt       to   fil-coc-prt            .
           move      rf-osr-coc-prg       to   fil-coc-prg            .
           move      rf-osr-coc-fzs       to   fil-coc-fzs            .
           move      rf-osr-flg-rch       to   fil-flg-rch            .
           move      rf-osr-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-osr-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-osr-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-osr-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-osr-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-osr-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-osr-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-osr-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-osr-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-osr-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-osr-flg-pul       to   fil-flg-pul            .
           move      rf-osr-flg-puq       to   fil-flg-puq            .
           move      rf-osr-alx-exp       to   fil-alx-exp            .
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
           move      rf-osr-num-prt       to   fil-num-prt-1          .
           move      rf-osr-num-prg       to   fil-num-prg-1          .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-osr-cod-dpz       to   fil-cod-dpz-2          .
           move      rf-osr-tip-mag       to   fil-tip-mag-2          .
           move      rf-osr-num-pro       to   fil-num-pro-2          .
           move      rf-osr-dat-doc       to   fil-dat-doc-2          .
           move      rf-osr-num-prt       to   fil-num-prt-2          .
           move      rf-osr-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-osr-cod-dpz       to   fil-cod-dpz-3          .
           move      rf-osr-coc-prt       to   fil-coc-prt-3          .
           move      rf-osr-coc-prg       to   fil-coc-prg-3          .
           move      rf-osr-num-prt       to   fil-num-prt-3          .
           move      rf-osr-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-osr-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-osr-flg-rch       to   fil-flg-rch-4          .
           move      rf-osr-tip-mag       to   fil-tip-mag-4          .
           move      rf-osr-num-pro       to   fil-num-pro-4          .
           move      rf-osr-num-prt       to   fil-num-prt-4          .
           move      rf-osr-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-osr-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-osr-flg-rch       to   fil-flg-rch-5          .
           move      rf-osr-tip-arc       to   fil-tip-arc-5          .
           move      rf-osr-cod-arc       to   fil-cod-arc-5          .
           move      rf-osr-num-prt       to   fil-num-prt-5          .
           move      rf-osr-num-prg       to   fil-num-prg-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-osr                 .
           move      fil-num-prt          to   rf-osr-num-prt         .
           move      fil-num-prg          to   rf-osr-num-prg         .
           move      fil-cod-tms          to   rf-osr-cod-tms         .
           move      fil-cod-dpz          to   rf-osr-cod-dpz         .
           move      fil-dat-doc          to   rf-osr-dat-doc         .
           move      fil-tip-arc          to   rf-osr-tip-arc         .
           move      fil-cod-arc          to   rf-osr-cod-arc         .
           move      fil-dpz-arc          to   rf-osr-dpz-arc         .
           move      fil-cod-lng          to   rf-osr-cod-lng         .
           move      fil-sgl-vpf          to   rf-osr-sgl-vpf         .
           move      fil-dec-vpf          to   rf-osr-dec-vpf         .
           move      fil-tdc-vpf          to   rf-osr-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-osr-cdc-vpf         .
           move      fil-bld-flb          to   rf-osr-bld-flb         .
           move      fil-bld-tpb          to   rf-osr-bld-tpb         .
           move      fil-bld-rgb          to   rf-osr-bld-rgb         .
           move      fil-tip-rig          to   rf-osr-tip-rig         .
           move      fil-tip-mag          to   rf-osr-tip-mag         .
           move      fil-num-pro          to   rf-osr-num-pro         .
           move      fil-alf-pro          to   rf-osr-alf-pro         .
           move      fil-sgl-vrn          to   rf-osr-sgl-vrn         .
           move      fil-cop-scl          to   rf-osr-cop-scl         .
           move      fil-des-ext          to   rf-osr-des-ext         .
           move      fil-des-rig          to   rf-osr-des-rig         .
           move      fil-tip-pro          to   rf-osr-tip-pro         .
           move      fil-cod-iva          to   rf-osr-cod-iva         .
           move      fil-ctp-ven          to   rf-osr-ctp-ven         .
           move      fil-umi-ven          to   rf-osr-umi-ven         .
           move      fil-dec-qta          to   rf-osr-dec-qta         .
           move      fil-qta-ven          to   rf-osr-qta-ven         .
           move      fil-snx-2qt          to   rf-osr-snx-2qt         .
           move      fil-dec-2qt          to   rf-osr-dec-2qt         .
           move      fil-qta-a02          to   rf-osr-qta-a02         .
           move      fil-snx-3qt          to   rf-osr-snx-3qt         .
           move      fil-dec-3qt          to   rf-osr-dec-3qt         .
           move      fil-qta-a03          to   rf-osr-qta-a03         .
           move      fil-dec-prz          to   rf-osr-dec-prz         .
           move      fil-sgl-vps          to   rf-osr-sgl-vps         .
           move      fil-dec-vps          to   rf-osr-dec-vps         .
           move      fil-tdc-vps          to   rf-osr-tdc-vps         .
           move      fil-cdc-vps          to   rf-osr-cdc-vps         .
           move      fil-prz-lrs          to   rf-osr-prz-lrs         .
           move      fil-prz-nts          to   rf-osr-prz-nts         .
           move      fil-sgl-vpp          to   rf-osr-sgl-vpp         .
           move      fil-dec-vpp          to   rf-osr-dec-vpp         .
           move      fil-tdc-vpp          to   rf-osr-tdc-vpp         .
           move      fil-cdc-vpp          to   rf-osr-cdc-vpp         .
           move      fil-prz-ven          to   rf-osr-prz-ven         .
           move      fil-snx-2pz          to   rf-osr-snx-2pz         .
           move      fil-prz-a02          to   rf-osr-prz-a02         .
           move      fil-sgl-vpl          to   rf-osr-sgl-vpl         .
           move      fil-dec-vpl          to   rf-osr-dec-vpl         .
           move      fil-tdc-vpl          to   rf-osr-tdc-vpl         .
           move      fil-prz-vpl          to   rf-osr-prz-vpl         .
           move      fil-cdc-vpl          to   rf-osr-cdc-vpl         .
           move      fil-ccr-vpl          to   rf-osr-ccr-vpl         .
           move      fil-plm-vpl          to   rf-osr-plm-vpl         .
           move      fil-tlm-vpl          to   rf-osr-tlm-vpl         .
           move      fil-map-vpl          to   rf-osr-map-vpl         .
           move      fil-epz-rgf          to   rf-osr-epz-rgf         .
           move      fil-csr-aap          to   rf-osr-csr-aap         .
           move      fil-psr-aap (1)      to   rf-osr-psr-aap (1)     .
           move      fil-psr-aap (2)      to   rf-osr-psr-aap (2)     .
           move      fil-psr-aap (3)      to   rf-osr-psr-aap (3)     .
           move      fil-psr-aap (4)      to   rf-osr-psr-aap (4)     .
           move      fil-psr-aap (5)      to   rf-osr-psr-aap (5)     .
           move      fil-per-scr (1)      to   rf-osr-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-osr-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-osr-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-osr-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-osr-per-scr (5)     .
           move      fil-prz-net          to   rf-osr-prz-net         .
           move      fil-epz-pes          to   rf-osr-epz-pes         .
           move      fil-sgl-vpc          to   rf-osr-sgl-vpc         .
           move      fil-dec-vpc          to   rf-osr-dec-vpc         .
           move      fil-tdc-vpc          to   rf-osr-tdc-vpc         .
           move      fil-cdc-vpc          to   rf-osr-cdc-vpc         .
           move      fil-dec-cos          to   rf-osr-dec-cos         .
           move      fil-cos-rif          to   rf-osr-cos-rif         .
           move      fil-imp-rig          to   rf-osr-imp-rig         .
           move      fil-iau-rig          to   rf-osr-iau-rig         .
           move      fil-cpv-aap          to   rf-osr-cpv-aap         .
           move      fil-ppv-aap (1)      to   rf-osr-ppv-aap (1)     .
           move      fil-ppv-aap (2)      to   rf-osr-ppv-aap (2)     .
           move      fil-ppv-aap (3)      to   rf-osr-ppv-aap (3)     .
           move      fil-fsp-rig          to   rf-osr-fsp-rig         .
           move      fil-cpv-rig          to   rf-osr-cpv-rig         .
           move      fil-ppv-rig (1)      to   rf-osr-ppv-rig (1)     .
           move      fil-ppv-rig (2)      to   rf-osr-ppv-rig (2)     .
           move      fil-ppv-rig (3)      to   rf-osr-ppv-rig (3)     .
           move      fil-pvf-rig          to   rf-osr-pvf-rig         .
           move      fil-ocl-dat          to   rf-osr-ocl-dat         .
           move      fil-ocl-num          to   rf-osr-ocl-num         .
           move      fil-cmc-tip          to   rf-osr-cmc-tip         .
           move      fil-cmc-dat          to   rf-osr-cmc-dat         .
           move      fil-cmc-num          to   rf-osr-cmc-num         .
           move      fil-coc-tip          to   rf-osr-coc-tip         .
           move      fil-coc-dat          to   rf-osr-coc-dat         .
           move      fil-coc-num          to   rf-osr-coc-num         .
           move      fil-coc-prt          to   rf-osr-coc-prt         .
           move      fil-coc-prg          to   rf-osr-coc-prg         .
           move      fil-coc-fzs          to   rf-osr-coc-fzs         .
           move      fil-flg-rch          to   rf-osr-flg-rch         .
           move      fil-flg-blx (1)      to   rf-osr-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-osr-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-osr-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-osr-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-osr-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-osr-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-osr-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-osr-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-osr-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-osr-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-osr-flg-pul         .
           move      fil-flg-puq          to   rf-osr-flg-puq         .
           move      fil-alx-exp          to   rf-osr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-osr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-osr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

