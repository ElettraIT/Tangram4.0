       Identification Division.
       Program-Id.                                 ioffir             .
      *================================================================*
      *                                                                *
      *                  Input-Output File fir                         *
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
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRODAT                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-pro        pic  9(07)       comp-3     .
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(09)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATPRO                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-doc-3      pic  9(07)       comp-3     .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(09)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CLIPRO                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-cli        pic  9(07)       comp-3     .
                   15  fil-tip-mag-4      pic  9(02)                  .
                   15  fil-num-pro-4      pic  9(07)       comp-3     .
                   15  fil-dat-doc-4      pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(09)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-cod-tmo            pic  x(05)                  .
               10  fil-cod-dpz            pic  9(02)                  .
               10  fil-num-doc            pic  9(11)       comp-3     .
               10  fil-dpz-cli            pic  x(04)                  .
               10  fil-cli-pls            pic  9(07)       comp-3     .
               10  fil-dpc-pls            pic  x(04)                  .
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
               10  fil-bcc-tip            pic  x(05)                  .
               10  fil-bcc-dat            pic  9(07)       comp-3     .
               10  fil-bcc-num            pic  9(11)       comp-3     .
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
                   15  pul-num-prt        pic  9(09)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRODAT                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-pro        pic  9(07)       comp-3     .
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(09)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATPRO                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-doc-3      pic  9(07)       comp-3     .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-pro-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(09)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CLIPRO                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-cli        pic  9(07)       comp-3     .
                   15  pul-tip-mag-4      pic  9(02)                  .
                   15  pul-num-pro-4      pic  9(07)       comp-3     .
                   15  pul-dat-doc-4      pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(09)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-cod-tmo            pic  x(05)                  .
               10  pul-cod-dpz            pic  9(02)                  .
               10  pul-num-doc            pic  9(11)       comp-3     .
               10  pul-dpz-cli            pic  x(04)                  .
               10  pul-cli-pls            pic  9(07)       comp-3     .
               10  pul-dpc-pls            pic  x(04)                  .
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
               10  pul-bcc-tip            pic  x(05)                  .
               10  pul-bcc-dat            pic  9(07)       comp-3     .
               10  pul-bcc-num            pic  9(11)       comp-3     .
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
                     "fir "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/fat/fls/ioc/obj/ioffir              "       .

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
           05  k-ctr                      pic  9(02) value 4          .
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
                            "PRODAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATPRO    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CLIPRO    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    4      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [fir]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .

      ******************************************************************
       Procedure Division                using f rf-fir               .
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
           move      spaces               to   rf-fir                 .
           move      zero                 to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      spaces               to   rf-fir-cod-tmo         .
           move      zero                 to   rf-fir-cod-dpz         .
           move      zero                 to   rf-fir-dat-doc         .
           move      zero                 to   rf-fir-num-doc         .
           move      zero                 to   rf-fir-cod-cli         .
           move      spaces               to   rf-fir-dpz-cli         .
           move      zero                 to   rf-fir-cli-pls         .
           move      spaces               to   rf-fir-dpc-pls         .
           move      spaces               to   rf-fir-cod-lng         .
           move      spaces               to   rf-fir-sgl-vpf         .
           move      zero                 to   rf-fir-dec-vpf         .
           move      spaces               to   rf-fir-tdc-vpf         .
           move      zero                 to   rf-fir-cdc-vpf         .
           move      zero                 to   rf-fir-bld-flb         .
           move      zero                 to   rf-fir-bld-tpb         .
           move      zero                 to   rf-fir-bld-rgb         .
           move      spaces               to   rf-fir-tip-rig         .
           move      zero                 to   rf-fir-tip-mag         .
           move      zero                 to   rf-fir-num-pro         .
           move      spaces               to   rf-fir-alf-pro         .
           move      spaces               to   rf-fir-sgl-vrn         .
           move      spaces               to   rf-fir-cop-scl         .
           move      zero                 to   rf-fir-des-ext         .
           move      spaces               to   rf-fir-des-rig         .
           move      zero                 to   rf-fir-tip-pro         .
           move      zero                 to   rf-fir-cod-iva         .
           move      zero                 to   rf-fir-ctp-ven         .
           move      spaces               to   rf-fir-umi-ven         .
           move      zero                 to   rf-fir-dec-qta         .
           move      zero                 to   rf-fir-qta-ven         .
           move      zero                 to   rf-fir-snx-2qt         .
           move      zero                 to   rf-fir-dec-2qt         .
           move      zero                 to   rf-fir-qta-a02         .
           move      zero                 to   rf-fir-snx-3qt         .
           move      zero                 to   rf-fir-dec-3qt         .
           move      zero                 to   rf-fir-qta-a03         .
           move      zero                 to   rf-fir-dec-prz         .
           move      spaces               to   rf-fir-sgl-vps         .
           move      zero                 to   rf-fir-dec-vps         .
           move      spaces               to   rf-fir-tdc-vps         .
           move      zero                 to   rf-fir-cdc-vps         .
           move      zero                 to   rf-fir-prz-lrs         .
           move      zero                 to   rf-fir-prz-nts         .
           move      spaces               to   rf-fir-sgl-vpp         .
           move      zero                 to   rf-fir-dec-vpp         .
           move      spaces               to   rf-fir-tdc-vpp         .
           move      zero                 to   rf-fir-cdc-vpp         .
           move      zero                 to   rf-fir-prz-ven         .
           move      zero                 to   rf-fir-snx-2pz         .
           move      zero                 to   rf-fir-prz-a02         .
           move      spaces               to   rf-fir-sgl-vpl         .
           move      zero                 to   rf-fir-dec-vpl         .
           move      spaces               to   rf-fir-tdc-vpl         .
           move      zero                 to   rf-fir-prz-vpl         .
           move      zero                 to   rf-fir-cdc-vpl         .
           move      zero                 to   rf-fir-ccr-vpl         .
           move      zero                 to   rf-fir-plm-vpl         .
           move      spaces               to   rf-fir-tlm-vpl         .
           move      spaces               to   rf-fir-map-vpl         .
           move      zero                 to   rf-fir-epz-rgf         .
           move      zero                 to   rf-fir-csr-aap         .
           move      zero                 to   rf-fir-psr-aap (1)     .
           move      zero                 to   rf-fir-psr-aap (2)     .
           move      zero                 to   rf-fir-psr-aap (3)     .
           move      zero                 to   rf-fir-psr-aap (4)     .
           move      zero                 to   rf-fir-psr-aap (5)     .
           move      zero                 to   rf-fir-per-scr (1)     .
           move      zero                 to   rf-fir-per-scr (2)     .
           move      zero                 to   rf-fir-per-scr (3)     .
           move      zero                 to   rf-fir-per-scr (4)     .
           move      zero                 to   rf-fir-per-scr (5)     .
           move      zero                 to   rf-fir-prz-net         .
           move      zero                 to   rf-fir-epz-pes         .
           move      spaces               to   rf-fir-sgl-vpc         .
           move      zero                 to   rf-fir-dec-vpc         .
           move      spaces               to   rf-fir-tdc-vpc         .
           move      zero                 to   rf-fir-cdc-vpc         .
           move      zero                 to   rf-fir-dec-cos         .
           move      zero                 to   rf-fir-cos-rif         .
           move      zero                 to   rf-fir-imp-rig         .
           move      zero                 to   rf-fir-iau-rig         .
           move      zero                 to   rf-fir-cpv-aap         .
           move      zero                 to   rf-fir-ppv-aap (1)     .
           move      zero                 to   rf-fir-ppv-aap (2)     .
           move      zero                 to   rf-fir-ppv-aap (3)     .
           move      zero                 to   rf-fir-fsp-rig         .
           move      zero                 to   rf-fir-cpv-rig         .
           move      zero                 to   rf-fir-ppv-rig (1)     .
           move      zero                 to   rf-fir-ppv-rig (2)     .
           move      zero                 to   rf-fir-ppv-rig (3)     .
           move      zero                 to   rf-fir-pvf-rig         .
           move      zero                 to   rf-fir-ocl-dat         .
           move      spaces               to   rf-fir-ocl-num         .
           move      spaces               to   rf-fir-cmc-tip         .
           move      zero                 to   rf-fir-cmc-dat         .
           move      zero                 to   rf-fir-cmc-num         .
           move      spaces               to   rf-fir-coc-tip         .
           move      zero                 to   rf-fir-coc-dat         .
           move      zero                 to   rf-fir-coc-num         .
           move      spaces               to   rf-fir-bcc-tip         .
           move      zero                 to   rf-fir-bcc-dat         .
           move      zero                 to   rf-fir-bcc-num         .
           move      spaces               to   rf-fir-flg-blx (1)     .
           move      spaces               to   rf-fir-flg-blx (2)     .
           move      spaces               to   rf-fir-flg-blx (3)     .
           move      spaces               to   rf-fir-flg-blx (4)     .
           move      spaces               to   rf-fir-flg-blx (5)     .
           move      spaces               to   rf-fir-flg-blx (6)     .
           move      spaces               to   rf-fir-flg-blx (7)     .
           move      spaces               to   rf-fir-flg-nbx (1)     .
           move      spaces               to   rf-fir-flg-nbx (2)     .
           move      spaces               to   rf-fir-flg-nbx (3)     .
           move      spaces               to   rf-fir-flg-pul         .
           move      spaces               to   rf-fir-flg-puq         .
           move      spaces               to   rf-fir-alx-exp         .
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
           move      rf-fir-cod-tmo       to   fil-cod-tmo            .
           move      rf-fir-cod-dpz       to   fil-cod-dpz            .
           move      rf-fir-num-doc       to   fil-num-doc            .
           move      rf-fir-dpz-cli       to   fil-dpz-cli            .
           move      rf-fir-cli-pls       to   fil-cli-pls            .
           move      rf-fir-dpc-pls       to   fil-dpc-pls            .
           move      rf-fir-cod-lng       to   fil-cod-lng            .
           move      rf-fir-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-fir-dec-vpf       to   fil-dec-vpf            .
           move      rf-fir-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-fir-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-fir-bld-flb       to   fil-bld-flb            .
           move      rf-fir-bld-tpb       to   fil-bld-tpb            .
           move      rf-fir-bld-rgb       to   fil-bld-rgb            .
           move      rf-fir-tip-rig       to   fil-tip-rig            .
           move      rf-fir-num-pro       to   fil-num-pro            .
           move      rf-fir-alf-pro       to   fil-alf-pro            .
           move      rf-fir-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-fir-cop-scl       to   fil-cop-scl            .
           move      rf-fir-des-ext       to   fil-des-ext            .
           move      rf-fir-des-rig       to   fil-des-rig            .
           move      rf-fir-tip-pro       to   fil-tip-pro            .
           move      rf-fir-cod-iva       to   fil-cod-iva            .
           move      rf-fir-ctp-ven       to   fil-ctp-ven            .
           move      rf-fir-umi-ven       to   fil-umi-ven            .
           move      rf-fir-dec-qta       to   fil-dec-qta            .
           move      rf-fir-qta-ven       to   fil-qta-ven            .
           move      rf-fir-snx-2qt       to   fil-snx-2qt            .
           move      rf-fir-dec-2qt       to   fil-dec-2qt            .
           move      rf-fir-qta-a02       to   fil-qta-a02            .
           move      rf-fir-snx-3qt       to   fil-snx-3qt            .
           move      rf-fir-dec-3qt       to   fil-dec-3qt            .
           move      rf-fir-qta-a03       to   fil-qta-a03            .
           move      rf-fir-dec-prz       to   fil-dec-prz            .
           move      rf-fir-sgl-vps       to   fil-sgl-vps            .
           move      rf-fir-dec-vps       to   fil-dec-vps            .
           move      rf-fir-tdc-vps       to   fil-tdc-vps            .
           move      rf-fir-cdc-vps       to   fil-cdc-vps            .
           move      rf-fir-prz-lrs       to   fil-prz-lrs            .
           move      rf-fir-prz-nts       to   fil-prz-nts            .
           move      rf-fir-sgl-vpp       to   fil-sgl-vpp            .
           move      rf-fir-dec-vpp       to   fil-dec-vpp            .
           move      rf-fir-tdc-vpp       to   fil-tdc-vpp            .
           move      rf-fir-cdc-vpp       to   fil-cdc-vpp            .
           move      rf-fir-prz-ven       to   fil-prz-ven            .
           move      rf-fir-snx-2pz       to   fil-snx-2pz            .
           move      rf-fir-prz-a02       to   fil-prz-a02            .
           move      rf-fir-sgl-vpl       to   fil-sgl-vpl            .
           move      rf-fir-dec-vpl       to   fil-dec-vpl            .
           move      rf-fir-tdc-vpl       to   fil-tdc-vpl            .
           move      rf-fir-prz-vpl       to   fil-prz-vpl            .
           move      rf-fir-cdc-vpl       to   fil-cdc-vpl            .
           move      rf-fir-ccr-vpl       to   fil-ccr-vpl            .
           move      rf-fir-plm-vpl       to   fil-plm-vpl            .
           move      rf-fir-tlm-vpl       to   fil-tlm-vpl            .
           move      rf-fir-map-vpl       to   fil-map-vpl            .
           move      rf-fir-epz-rgf       to   fil-epz-rgf            .
           move      rf-fir-csr-aap       to   fil-csr-aap            .
           move      rf-fir-psr-aap (1)   to   fil-psr-aap (1)        .
           move      rf-fir-psr-aap (2)   to   fil-psr-aap (2)        .
           move      rf-fir-psr-aap (3)   to   fil-psr-aap (3)        .
           move      rf-fir-psr-aap (4)   to   fil-psr-aap (4)        .
           move      rf-fir-psr-aap (5)   to   fil-psr-aap (5)        .
           move      rf-fir-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-fir-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-fir-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-fir-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-fir-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-fir-prz-net       to   fil-prz-net            .
           move      rf-fir-epz-pes       to   fil-epz-pes            .
           move      rf-fir-sgl-vpc       to   fil-sgl-vpc            .
           move      rf-fir-dec-vpc       to   fil-dec-vpc            .
           move      rf-fir-tdc-vpc       to   fil-tdc-vpc            .
           move      rf-fir-cdc-vpc       to   fil-cdc-vpc            .
           move      rf-fir-dec-cos       to   fil-dec-cos            .
           move      rf-fir-cos-rif       to   fil-cos-rif            .
           move      rf-fir-imp-rig       to   fil-imp-rig            .
           move      rf-fir-iau-rig       to   fil-iau-rig            .
           move      rf-fir-cpv-aap       to   fil-cpv-aap            .
           move      rf-fir-ppv-aap (1)   to   fil-ppv-aap (1)        .
           move      rf-fir-ppv-aap (2)   to   fil-ppv-aap (2)        .
           move      rf-fir-ppv-aap (3)   to   fil-ppv-aap (3)        .
           move      rf-fir-fsp-rig       to   fil-fsp-rig            .
           move      rf-fir-cpv-rig       to   fil-cpv-rig            .
           move      rf-fir-ppv-rig (1)   to   fil-ppv-rig (1)        .
           move      rf-fir-ppv-rig (2)   to   fil-ppv-rig (2)        .
           move      rf-fir-ppv-rig (3)   to   fil-ppv-rig (3)        .
           move      rf-fir-pvf-rig       to   fil-pvf-rig            .
           move      rf-fir-ocl-dat       to   fil-ocl-dat            .
           move      rf-fir-ocl-num       to   fil-ocl-num            .
           move      rf-fir-cmc-tip       to   fil-cmc-tip            .
           move      rf-fir-cmc-dat       to   fil-cmc-dat            .
           move      rf-fir-cmc-num       to   fil-cmc-num            .
           move      rf-fir-coc-tip       to   fil-coc-tip            .
           move      rf-fir-coc-dat       to   fil-coc-dat            .
           move      rf-fir-coc-num       to   fil-coc-num            .
           move      rf-fir-bcc-tip       to   fil-bcc-tip            .
           move      rf-fir-bcc-dat       to   fil-bcc-dat            .
           move      rf-fir-bcc-num       to   fil-bcc-num            .
           move      rf-fir-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-fir-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-fir-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-fir-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-fir-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-fir-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-fir-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-fir-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-fir-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-fir-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-fir-flg-pul       to   fil-flg-pul            .
           move      rf-fir-flg-puq       to   fil-flg-puq            .
           move      rf-fir-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-fir-num-prt       to   fil-num-prt            .
           move      rf-fir-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-fir-tip-mag       to   fil-tip-mag            .
           move      rf-fir-num-pro       to   fil-num-pro            .
           move      rf-fir-dat-doc       to   fil-dat-doc            .
           move      rf-fir-num-prt       to   fil-num-prt-2          .
           move      rf-fir-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-fir-dat-doc       to   fil-dat-doc-3          .
           move      rf-fir-tip-mag       to   fil-tip-mag-3          .
           move      rf-fir-num-pro       to   fil-num-pro-3          .
           move      rf-fir-num-prt       to   fil-num-prt-3          .
           move      rf-fir-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-fir-cod-cli       to   fil-cod-cli            .
           move      rf-fir-tip-mag       to   fil-tip-mag-4          .
           move      rf-fir-num-pro       to   fil-num-pro-4          .
           move      rf-fir-dat-doc       to   fil-dat-doc-4          .
           move      rf-fir-num-prt       to   fil-num-prt-4          .
           move      rf-fir-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-fir                 .
           move      fil-num-prt          to   rf-fir-num-prt         .
           move      fil-num-prg          to   rf-fir-num-prg         .
           move      fil-cod-tmo          to   rf-fir-cod-tmo         .
           move      fil-cod-dpz          to   rf-fir-cod-dpz         .
           move      fil-dat-doc          to   rf-fir-dat-doc         .
           move      fil-num-doc          to   rf-fir-num-doc         .
           move      fil-cod-cli          to   rf-fir-cod-cli         .
           move      fil-dpz-cli          to   rf-fir-dpz-cli         .
           move      fil-cli-pls          to   rf-fir-cli-pls         .
           move      fil-dpc-pls          to   rf-fir-dpc-pls         .
           move      fil-cod-lng          to   rf-fir-cod-lng         .
           move      fil-sgl-vpf          to   rf-fir-sgl-vpf         .
           move      fil-dec-vpf          to   rf-fir-dec-vpf         .
           move      fil-tdc-vpf          to   rf-fir-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-fir-cdc-vpf         .
           move      fil-bld-flb          to   rf-fir-bld-flb         .
           move      fil-bld-tpb          to   rf-fir-bld-tpb         .
           move      fil-bld-rgb          to   rf-fir-bld-rgb         .
           move      fil-tip-rig          to   rf-fir-tip-rig         .
           move      fil-tip-mag          to   rf-fir-tip-mag         .
           move      fil-num-pro          to   rf-fir-num-pro         .
           move      fil-alf-pro          to   rf-fir-alf-pro         .
           move      fil-sgl-vrn          to   rf-fir-sgl-vrn         .
           move      fil-cop-scl          to   rf-fir-cop-scl         .
           move      fil-des-ext          to   rf-fir-des-ext         .
           move      fil-des-rig          to   rf-fir-des-rig         .
           move      fil-tip-pro          to   rf-fir-tip-pro         .
           move      fil-cod-iva          to   rf-fir-cod-iva         .
           move      fil-ctp-ven          to   rf-fir-ctp-ven         .
           move      fil-umi-ven          to   rf-fir-umi-ven         .
           move      fil-dec-qta          to   rf-fir-dec-qta         .
           move      fil-qta-ven          to   rf-fir-qta-ven         .
           move      fil-snx-2qt          to   rf-fir-snx-2qt         .
           move      fil-dec-2qt          to   rf-fir-dec-2qt         .
           move      fil-qta-a02          to   rf-fir-qta-a02         .
           move      fil-snx-3qt          to   rf-fir-snx-3qt         .
           move      fil-dec-3qt          to   rf-fir-dec-3qt         .
           move      fil-qta-a03          to   rf-fir-qta-a03         .
           move      fil-dec-prz          to   rf-fir-dec-prz         .
           move      fil-sgl-vps          to   rf-fir-sgl-vps         .
           move      fil-dec-vps          to   rf-fir-dec-vps         .
           move      fil-tdc-vps          to   rf-fir-tdc-vps         .
           move      fil-cdc-vps          to   rf-fir-cdc-vps         .
           move      fil-prz-lrs          to   rf-fir-prz-lrs         .
           move      fil-prz-nts          to   rf-fir-prz-nts         .
           move      fil-sgl-vpp          to   rf-fir-sgl-vpp         .
           move      fil-dec-vpp          to   rf-fir-dec-vpp         .
           move      fil-tdc-vpp          to   rf-fir-tdc-vpp         .
           move      fil-cdc-vpp          to   rf-fir-cdc-vpp         .
           move      fil-prz-ven          to   rf-fir-prz-ven         .
           move      fil-snx-2pz          to   rf-fir-snx-2pz         .
           move      fil-prz-a02          to   rf-fir-prz-a02         .
           move      fil-sgl-vpl          to   rf-fir-sgl-vpl         .
           move      fil-dec-vpl          to   rf-fir-dec-vpl         .
           move      fil-tdc-vpl          to   rf-fir-tdc-vpl         .
           move      fil-prz-vpl          to   rf-fir-prz-vpl         .
           move      fil-cdc-vpl          to   rf-fir-cdc-vpl         .
           move      fil-ccr-vpl          to   rf-fir-ccr-vpl         .
           move      fil-plm-vpl          to   rf-fir-plm-vpl         .
           move      fil-tlm-vpl          to   rf-fir-tlm-vpl         .
           move      fil-map-vpl          to   rf-fir-map-vpl         .
           move      fil-epz-rgf          to   rf-fir-epz-rgf         .
           move      fil-csr-aap          to   rf-fir-csr-aap         .
           move      fil-psr-aap (1)      to   rf-fir-psr-aap (1)     .
           move      fil-psr-aap (2)      to   rf-fir-psr-aap (2)     .
           move      fil-psr-aap (3)      to   rf-fir-psr-aap (3)     .
           move      fil-psr-aap (4)      to   rf-fir-psr-aap (4)     .
           move      fil-psr-aap (5)      to   rf-fir-psr-aap (5)     .
           move      fil-per-scr (1)      to   rf-fir-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-fir-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-fir-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-fir-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-fir-per-scr (5)     .
           move      fil-prz-net          to   rf-fir-prz-net         .
           move      fil-epz-pes          to   rf-fir-epz-pes         .
           move      fil-sgl-vpc          to   rf-fir-sgl-vpc         .
           move      fil-dec-vpc          to   rf-fir-dec-vpc         .
           move      fil-tdc-vpc          to   rf-fir-tdc-vpc         .
           move      fil-cdc-vpc          to   rf-fir-cdc-vpc         .
           move      fil-dec-cos          to   rf-fir-dec-cos         .
           move      fil-cos-rif          to   rf-fir-cos-rif         .
           move      fil-imp-rig          to   rf-fir-imp-rig         .
           move      fil-iau-rig          to   rf-fir-iau-rig         .
           move      fil-cpv-aap          to   rf-fir-cpv-aap         .
           move      fil-ppv-aap (1)      to   rf-fir-ppv-aap (1)     .
           move      fil-ppv-aap (2)      to   rf-fir-ppv-aap (2)     .
           move      fil-ppv-aap (3)      to   rf-fir-ppv-aap (3)     .
           move      fil-fsp-rig          to   rf-fir-fsp-rig         .
           move      fil-cpv-rig          to   rf-fir-cpv-rig         .
           move      fil-ppv-rig (1)      to   rf-fir-ppv-rig (1)     .
           move      fil-ppv-rig (2)      to   rf-fir-ppv-rig (2)     .
           move      fil-ppv-rig (3)      to   rf-fir-ppv-rig (3)     .
           move      fil-pvf-rig          to   rf-fir-pvf-rig         .
           move      fil-ocl-dat          to   rf-fir-ocl-dat         .
           move      fil-ocl-num          to   rf-fir-ocl-num         .
           move      fil-cmc-tip          to   rf-fir-cmc-tip         .
           move      fil-cmc-dat          to   rf-fir-cmc-dat         .
           move      fil-cmc-num          to   rf-fir-cmc-num         .
           move      fil-coc-tip          to   rf-fir-coc-tip         .
           move      fil-coc-dat          to   rf-fir-coc-dat         .
           move      fil-coc-num          to   rf-fir-coc-num         .
           move      fil-bcc-tip          to   rf-fir-bcc-tip         .
           move      fil-bcc-dat          to   rf-fir-bcc-dat         .
           move      fil-bcc-num          to   rf-fir-bcc-num         .
           move      fil-flg-blx (1)      to   rf-fir-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-fir-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-fir-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-fir-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-fir-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-fir-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-fir-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-fir-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-fir-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-fir-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-fir-flg-pul         .
           move      fil-flg-puq          to   rf-fir-flg-puq         .
           move      fil-alx-exp          to   rf-fir-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-fir               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-fir
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

