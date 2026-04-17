       Identification Division.
       Program-Id.                                 iofofr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ofr                         *
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
                   15  fil-num-prt        pic  9(11)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-doc        pic  9(11)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dpz-3      pic  9(02)                  .
                   15  fil-flg-rch        pic  x(01)                  .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-mag-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-flg-rch-4      pic  x(01)                  .
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : ARCPRO                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-tip-arc-5      pic  x(01)                  .
                   15  fil-cod-arc-5      pic  9(07)       comp-3     .
                   15  fil-tip-mag-5      pic  9(02)                  .
                   15  fil-num-mag-5      pic  9(07)       comp-3     .
                   15  fil-dat-doc-5      pic  9(07)       comp-3     .
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
                   15  fil-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tmo-orf            pic  x(05)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-cof-dat            pic  9(07)       comp-3     .
               10  fil-cof-num            pic  x(10)                  .
               10  fil-pri-eva            pic  x(02)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-bld-flb            pic  9(01)                  .
               10  fil-bld-tpb            pic  9(01)                  .
               10  fil-bld-rgb            pic  9(01)                  .
               10  fil-tip-rig            pic  x(05)                  .
               10  fil-alf-mag            pic  x(14)                  .
               10  fil-sgl-vrn            pic  x(14)                  .
               10  fil-fda-pif            pic  x(14)                  .
               10  fil-cop-sfn            pic  x(14)                  .
               10  fil-snx-tum            pic  x(01)                  .
               10  fil-umf-tum            pic  x(03)                  .
               10  fil-nde-tum            pic  9(01)                  .
               10  fil-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  fil-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  fil-des-ext            pic  9(01)                  .
               10  fil-des-rig            pic  x(40)                  .
               10  fil-tip-pro            pic  9(02)                  .
               10  fil-cod-iva            pic  9(05)       comp-3     .
               10  fil-ctp-acq            pic  9(07)       comp-3     .
               10  fil-umi-acq            pic  x(03)                  .
               10  fil-dec-qta            pic  9(01)                  .
               10  fil-qta-fda            pic s9(10)v9(03) comp-3     .
               10  fil-qta-ord            pic s9(10)v9(03) comp-3     .
               10  fil-sdr-ccs            pic  x(01)                  .
               10  fil-cod-dsl            pic  x(07)                  .
               10  fil-snx-2qt            pic  9(01)                  .
               10  fil-dec-2qt            pic  9(01)                  .
               10  fil-qta-a02            pic s9(10)v9(03) comp-3     .
               10  fil-snx-3qt            pic  9(01)                  .
               10  fil-dec-3qt            pic  9(01)                  .
               10  fil-qta-a03            pic s9(10)v9(03) comp-3     .
               10  fil-dec-prz            pic  9(01)                  .
               10  fil-vpp.
                   15  fil-sgl-vpp        pic  x(03)                  .
                   15  fil-dec-vpp        pic  9(01)                  .
                   15  fil-tdc-vpp        pic  x(01)                  .
                   15  fil-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  fil-prz-acq            pic  9(09)       comp-3     .
               10  fil-snx-2pz            pic  9(01)                  .
               10  fil-dec-2pz            pic  9(01)                  .
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
               10  fil-epz-rgo            pic  9(01)                  .
               10  fil-csr-aap            pic  9(05)       comp-3     .
               10  fil-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-prz-net            pic  9(09)       comp-3     .
               10  fil-imp-rig            pic s9(11)       comp-3     .
               10  fil-iau-rig            pic s9(11)       comp-3     .
               10  fil-cpv-aap            pic  9(05)       comp-3     .
               10  fil-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-fsp-rig            pic  9(02)                  .
               10  fil-cpv-rig            pic  9(05)       comp-3     .
               10  fil-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-pvf-rig            pic s9(11)       comp-3     .
               10  fil-dcn-ric            pic  9(07)       comp-3     .
               10  fil-fds-dcr            pic  x(01)                  .
               10  fil-dcn-prv            pic  9(07)       comp-3     .
               10  fil-dcn-cnf            pic  9(07)       comp-3     .
               10  fil-flg-cnf            pic  x(01)                  .
               10  fil-oda-tip            pic  x(05)                  .
               10  fil-oda-dat            pic  9(07)       comp-3     .
               10  fil-oda-num            pic  9(11)       comp-3     .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-rif-lst            pic  x(20)                  .
               10  fil-tip-ord            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 19  pic  x(01)                  .

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
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-doc        pic  9(11)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dpz-3      pic  9(02)                  .
                   15  pul-flg-rch        pic  x(01)                  .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-mag-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-flg-rch-4      pic  x(01)                  .
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : ARCPRO                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-tip-arc-5      pic  x(01)                  .
                   15  pul-cod-arc-5      pic  9(07)       comp-3     .
                   15  pul-tip-mag-5      pic  9(02)                  .
                   15  pul-num-mag-5      pic  9(07)       comp-3     .
                   15  pul-dat-doc-5      pic  9(07)       comp-3     .
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
                   15  pul-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tmo-orf            pic  x(05)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-cof-dat            pic  9(07)       comp-3     .
               10  pul-cof-num            pic  x(10)                  .
               10  pul-pri-eva            pic  x(02)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-bld-flb            pic  9(01)                  .
               10  pul-bld-tpb            pic  9(01)                  .
               10  pul-bld-rgb            pic  9(01)                  .
               10  pul-tip-rig            pic  x(05)                  .
               10  pul-alf-mag            pic  x(14)                  .
               10  pul-sgl-vrn            pic  x(14)                  .
               10  pul-fda-pif            pic  x(14)                  .
               10  pul-cop-sfn            pic  x(14)                  .
               10  pul-snx-tum            pic  x(01)                  .
               10  pul-umf-tum            pic  x(03)                  .
               10  pul-nde-tum            pic  9(01)                  .
               10  pul-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  pul-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  pul-des-ext            pic  9(01)                  .
               10  pul-des-rig            pic  x(40)                  .
               10  pul-tip-pro            pic  9(02)                  .
               10  pul-cod-iva            pic  9(05)       comp-3     .
               10  pul-ctp-acq            pic  9(07)       comp-3     .
               10  pul-umi-acq            pic  x(03)                  .
               10  pul-dec-qta            pic  9(01)                  .
               10  pul-qta-fda            pic s9(10)v9(03) comp-3     .
               10  pul-qta-ord            pic s9(10)v9(03) comp-3     .
               10  pul-sdr-ccs            pic  x(01)                  .
               10  pul-cod-dsl            pic  x(07)                  .
               10  pul-snx-2qt            pic  9(01)                  .
               10  pul-dec-2qt            pic  9(01)                  .
               10  pul-qta-a02            pic s9(10)v9(03) comp-3     .
               10  pul-snx-3qt            pic  9(01)                  .
               10  pul-dec-3qt            pic  9(01)                  .
               10  pul-qta-a03            pic s9(10)v9(03) comp-3     .
               10  pul-dec-prz            pic  9(01)                  .
               10  pul-vpp.
                   15  pul-sgl-vpp        pic  x(03)                  .
                   15  pul-dec-vpp        pic  9(01)                  .
                   15  pul-tdc-vpp        pic  x(01)                  .
                   15  pul-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  pul-prz-acq            pic  9(09)       comp-3     .
               10  pul-snx-2pz            pic  9(01)                  .
               10  pul-dec-2pz            pic  9(01)                  .
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
               10  pul-epz-rgo            pic  9(01)                  .
               10  pul-csr-aap            pic  9(05)       comp-3     .
               10  pul-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-prz-net            pic  9(09)       comp-3     .
               10  pul-imp-rig            pic s9(11)       comp-3     .
               10  pul-iau-rig            pic s9(11)       comp-3     .
               10  pul-cpv-aap            pic  9(05)       comp-3     .
               10  pul-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-fsp-rig            pic  9(02)                  .
               10  pul-cpv-rig            pic  9(05)       comp-3     .
               10  pul-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-pvf-rig            pic s9(11)       comp-3     .
               10  pul-dcn-ric            pic  9(07)       comp-3     .
               10  pul-fds-dcr            pic  x(01)                  .
               10  pul-dcn-prv            pic  9(07)       comp-3     .
               10  pul-dcn-cnf            pic  9(07)       comp-3     .
               10  pul-flg-cnf            pic  x(01)                  .
               10  pul-oda-tip            pic  x(05)                  .
               10  pul-oda-dat            pic  9(07)       comp-3     .
               10  pul-oda-num            pic  9(11)       comp-3     .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-rif-lst            pic  x(20)                  .
               10  pul-tip-ord            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 19  pic  x(01)                  .

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
                     "ofr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/orf/fls/ioc/obj/iofofr              "       .

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
                            "RCHMAG    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RCHARC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCPRO    "                              .
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
      *    * Record logico file [ofr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .

      ******************************************************************
       Procedure Division                using f rf-ofr               .
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
           move      spaces               to   rf-ofr                 .
           move      zero                 to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      spaces               to   rf-ofr-tmo-orf         .
           move      zero                 to   rf-ofr-cod-dpz         .
           move      zero                 to   rf-ofr-dat-doc         .
           move      zero                 to   rf-ofr-num-doc         .
           move      spaces               to   rf-ofr-tip-arc         .
           move      zero                 to   rf-ofr-cod-arc         .
           move      spaces               to   rf-ofr-dpz-arc         .
           move      spaces               to   rf-ofr-cod-lng         .
           move      zero                 to   rf-ofr-cof-dat         .
           move      zero                 to   rf-ofr-cof-num         .
           move      spaces               to   rf-ofr-pri-eva         .
           move      spaces               to   rf-ofr-sgl-vpf         .
           move      zero                 to   rf-ofr-dec-vpf         .
           move      spaces               to   rf-ofr-tdc-vpf         .
           move      zero                 to   rf-ofr-cdc-vpf         .
           move      zero                 to   rf-ofr-bld-flb         .
           move      zero                 to   rf-ofr-bld-tpb         .
           move      zero                 to   rf-ofr-bld-rgb         .
           move      spaces               to   rf-ofr-tip-rig         .
           move      zero                 to   rf-ofr-tip-mag         .
           move      zero                 to   rf-ofr-num-mag         .
           move      spaces               to   rf-ofr-alf-mag         .
           move      spaces               to   rf-ofr-sgl-vrn         .
           move      spaces               to   rf-ofr-fda-pif         .
           move      spaces               to   rf-ofr-cop-sfn         .
           move      spaces               to   rf-ofr-snx-tum         .
           move      spaces               to   rf-ofr-umf-tum         .
           move      zero                 to   rf-ofr-nde-tum         .
           move      zero                 to   rf-ofr-cmo-tum         .
           move      zero                 to   rf-ofr-cdi-tum         .
           move      zero                 to   rf-ofr-des-ext         .
           move      spaces               to   rf-ofr-des-rig         .
           move      zero                 to   rf-ofr-tip-pro         .
           move      zero                 to   rf-ofr-cod-iva         .
           move      zero                 to   rf-ofr-ctp-acq         .
           move      spaces               to   rf-ofr-umi-acq         .
           move      zero                 to   rf-ofr-dec-qta         .
           move      zero                 to   rf-ofr-qta-fda         .
           move      zero                 to   rf-ofr-qta-ord         .
           move      spaces               to   rf-ofr-sdr-ccs         .
           move      spaces               to   rf-ofr-cod-dsl         .
           move      zero                 to   rf-ofr-snx-2qt         .
           move      zero                 to   rf-ofr-dec-2qt         .
           move      zero                 to   rf-ofr-qta-a02         .
           move      zero                 to   rf-ofr-snx-3qt         .
           move      zero                 to   rf-ofr-dec-3qt         .
           move      zero                 to   rf-ofr-qta-a03         .
           move      zero                 to   rf-ofr-dec-prz         .
           move      spaces               to   rf-ofr-sgl-vpp         .
           move      zero                 to   rf-ofr-dec-vpp         .
           move      spaces               to   rf-ofr-tdc-vpp         .
           move      zero                 to   rf-ofr-cdc-vpp         .
           move      zero                 to   rf-ofr-prz-acq         .
           move      zero                 to   rf-ofr-snx-2pz         .
           move      zero                 to   rf-ofr-dec-2pz         .
           move      zero                 to   rf-ofr-prz-a02         .
           move      spaces               to   rf-ofr-sgl-vpl         .
           move      zero                 to   rf-ofr-dec-vpl         .
           move      spaces               to   rf-ofr-tdc-vpl         .
           move      zero                 to   rf-ofr-prz-vpl         .
           move      zero                 to   rf-ofr-cdc-vpl         .
           move      zero                 to   rf-ofr-ccr-vpl         .
           move      zero                 to   rf-ofr-plm-vpl         .
           move      spaces               to   rf-ofr-tlm-vpl         .
           move      spaces               to   rf-ofr-map-vpl         .
           move      zero                 to   rf-ofr-epz-rgo         .
           move      zero                 to   rf-ofr-csr-aap         .
           move      zero                 to   rf-ofr-psr-aap (1)     .
           move      zero                 to   rf-ofr-psr-aap (2)     .
           move      zero                 to   rf-ofr-psr-aap (3)     .
           move      zero                 to   rf-ofr-psr-aap (4)     .
           move      zero                 to   rf-ofr-psr-aap (5)     .
           move      zero                 to   rf-ofr-per-scr (1)     .
           move      zero                 to   rf-ofr-per-scr (2)     .
           move      zero                 to   rf-ofr-per-scr (3)     .
           move      zero                 to   rf-ofr-per-scr (4)     .
           move      zero                 to   rf-ofr-per-scr (5)     .
           move      zero                 to   rf-ofr-prz-net         .
           move      zero                 to   rf-ofr-imp-rig         .
           move      zero                 to   rf-ofr-iau-rig         .
           move      zero                 to   rf-ofr-cpv-aap         .
           move      zero                 to   rf-ofr-ppv-aap (1)     .
           move      zero                 to   rf-ofr-ppv-aap (2)     .
           move      zero                 to   rf-ofr-ppv-aap (3)     .
           move      zero                 to   rf-ofr-fsp-rig         .
           move      zero                 to   rf-ofr-cpv-rig         .
           move      zero                 to   rf-ofr-ppv-rig (1)     .
           move      zero                 to   rf-ofr-ppv-rig (2)     .
           move      zero                 to   rf-ofr-ppv-rig (3)     .
           move      zero                 to   rf-ofr-pvf-rig         .
           move      zero                 to   rf-ofr-dcn-ric         .
           move      spaces               to   rf-ofr-fds-dcr         .
           move      zero                 to   rf-ofr-dcn-prv         .
           move      zero                 to   rf-ofr-dcn-cnf         .
           move      spaces               to   rf-ofr-flg-cnf         .
           move      spaces               to   rf-ofr-oda-tip         .
           move      zero                 to   rf-ofr-oda-dat         .
           move      zero                 to   rf-ofr-oda-num         .
           move      spaces               to   rf-ofr-flg-rch         .
           move      spaces               to   rf-ofr-flg-blx (1)     .
           move      spaces               to   rf-ofr-flg-blx (2)     .
           move      spaces               to   rf-ofr-flg-blx (3)     .
           move      spaces               to   rf-ofr-flg-blx (4)     .
           move      spaces               to   rf-ofr-flg-blx (5)     .
           move      spaces               to   rf-ofr-flg-blx (6)     .
           move      spaces               to   rf-ofr-flg-blx (7)     .
           move      spaces               to   rf-ofr-flg-nbx (1)     .
           move      spaces               to   rf-ofr-flg-nbx (2)     .
           move      spaces               to   rf-ofr-flg-nbx (3)     .
           move      spaces               to   rf-ofr-flg-pul         .
           move      spaces               to   rf-ofr-rif-lst         .
           move      spaces               to   rf-ofr-tip-ord         .
           move      spaces               to   rf-ofr-alx-exp         .
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
           move      rf-ofr-tmo-orf       to   fil-tmo-orf            .
           move      rf-ofr-dpz-arc       to   fil-dpz-arc            .
           move      rf-ofr-cod-lng       to   fil-cod-lng            .
           move      rf-ofr-cof-dat       to   fil-cof-dat            .
           move      rf-ofr-cof-num       to   fil-cof-num            .
           move      rf-ofr-pri-eva       to   fil-pri-eva            .
           move      rf-ofr-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-ofr-dec-vpf       to   fil-dec-vpf            .
           move      rf-ofr-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-ofr-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-ofr-bld-flb       to   fil-bld-flb            .
           move      rf-ofr-bld-tpb       to   fil-bld-tpb            .
           move      rf-ofr-bld-rgb       to   fil-bld-rgb            .
           move      rf-ofr-tip-rig       to   fil-tip-rig            .
           move      rf-ofr-alf-mag       to   fil-alf-mag            .
           move      rf-ofr-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-ofr-fda-pif       to   fil-fda-pif            .
           move      rf-ofr-cop-sfn       to   fil-cop-sfn            .
           move      rf-ofr-snx-tum       to   fil-snx-tum            .
           move      rf-ofr-umf-tum       to   fil-umf-tum            .
           move      rf-ofr-nde-tum       to   fil-nde-tum            .
           move      rf-ofr-cmo-tum       to   fil-cmo-tum            .
           move      rf-ofr-cdi-tum       to   fil-cdi-tum            .
           move      rf-ofr-des-ext       to   fil-des-ext            .
           move      rf-ofr-des-rig       to   fil-des-rig            .
           move      rf-ofr-tip-pro       to   fil-tip-pro            .
           move      rf-ofr-cod-iva       to   fil-cod-iva            .
           move      rf-ofr-ctp-acq       to   fil-ctp-acq            .
           move      rf-ofr-umi-acq       to   fil-umi-acq            .
           move      rf-ofr-dec-qta       to   fil-dec-qta            .
           move      rf-ofr-qta-fda       to   fil-qta-fda            .
           move      rf-ofr-qta-ord       to   fil-qta-ord            .
           move      rf-ofr-sdr-ccs       to   fil-sdr-ccs            .
           move      rf-ofr-cod-dsl       to   fil-cod-dsl            .
           move      rf-ofr-snx-2qt       to   fil-snx-2qt            .
           move      rf-ofr-dec-2qt       to   fil-dec-2qt            .
           move      rf-ofr-qta-a02       to   fil-qta-a02            .
           move      rf-ofr-snx-3qt       to   fil-snx-3qt            .
           move      rf-ofr-dec-3qt       to   fil-dec-3qt            .
           move      rf-ofr-qta-a03       to   fil-qta-a03            .
           move      rf-ofr-dec-prz       to   fil-dec-prz            .
           move      rf-ofr-sgl-vpp       to   fil-sgl-vpp            .
           move      rf-ofr-dec-vpp       to   fil-dec-vpp            .
           move      rf-ofr-tdc-vpp       to   fil-tdc-vpp            .
           move      rf-ofr-cdc-vpp       to   fil-cdc-vpp            .
           move      rf-ofr-prz-acq       to   fil-prz-acq            .
           move      rf-ofr-snx-2pz       to   fil-snx-2pz            .
           move      rf-ofr-dec-2pz       to   fil-dec-2pz            .
           move      rf-ofr-prz-a02       to   fil-prz-a02            .
           move      rf-ofr-sgl-vpl       to   fil-sgl-vpl            .
           move      rf-ofr-dec-vpl       to   fil-dec-vpl            .
           move      rf-ofr-tdc-vpl       to   fil-tdc-vpl            .
           move      rf-ofr-prz-vpl       to   fil-prz-vpl            .
           move      rf-ofr-cdc-vpl       to   fil-cdc-vpl            .
           move      rf-ofr-ccr-vpl       to   fil-ccr-vpl            .
           move      rf-ofr-plm-vpl       to   fil-plm-vpl            .
           move      rf-ofr-tlm-vpl       to   fil-tlm-vpl            .
           move      rf-ofr-map-vpl       to   fil-map-vpl            .
           move      rf-ofr-epz-rgo       to   fil-epz-rgo            .
           move      rf-ofr-csr-aap       to   fil-csr-aap            .
           move      rf-ofr-psr-aap (1)   to   fil-psr-aap (1)        .
           move      rf-ofr-psr-aap (2)   to   fil-psr-aap (2)        .
           move      rf-ofr-psr-aap (3)   to   fil-psr-aap (3)        .
           move      rf-ofr-psr-aap (4)   to   fil-psr-aap (4)        .
           move      rf-ofr-psr-aap (5)   to   fil-psr-aap (5)        .
           move      rf-ofr-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-ofr-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-ofr-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-ofr-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-ofr-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-ofr-prz-net       to   fil-prz-net            .
           move      rf-ofr-imp-rig       to   fil-imp-rig            .
           move      rf-ofr-iau-rig       to   fil-iau-rig            .
           move      rf-ofr-cpv-aap       to   fil-cpv-aap            .
           move      rf-ofr-ppv-aap (1)   to   fil-ppv-aap (1)        .
           move      rf-ofr-ppv-aap (2)   to   fil-ppv-aap (2)        .
           move      rf-ofr-ppv-aap (3)   to   fil-ppv-aap (3)        .
           move      rf-ofr-fsp-rig       to   fil-fsp-rig            .
           move      rf-ofr-cpv-rig       to   fil-cpv-rig            .
           move      rf-ofr-ppv-rig (1)   to   fil-ppv-rig (1)        .
           move      rf-ofr-ppv-rig (2)   to   fil-ppv-rig (2)        .
           move      rf-ofr-ppv-rig (3)   to   fil-ppv-rig (3)        .
           move      rf-ofr-pvf-rig       to   fil-pvf-rig            .
           move      rf-ofr-dcn-ric       to   fil-dcn-ric            .
           move      rf-ofr-fds-dcr       to   fil-fds-dcr            .
           move      rf-ofr-dcn-prv       to   fil-dcn-prv            .
           move      rf-ofr-dcn-cnf       to   fil-dcn-cnf            .
           move      rf-ofr-flg-cnf       to   fil-flg-cnf            .
           move      rf-ofr-oda-tip       to   fil-oda-tip            .
           move      rf-ofr-oda-dat       to   fil-oda-dat            .
           move      rf-ofr-oda-num       to   fil-oda-num            .
           move      rf-ofr-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-ofr-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-ofr-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-ofr-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-ofr-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-ofr-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-ofr-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-ofr-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-ofr-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-ofr-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-ofr-flg-pul       to   fil-flg-pul            .
           move      rf-ofr-rif-lst       to   fil-rif-lst            .
           move      rf-ofr-tip-ord       to   fil-tip-ord            .
           move      rf-ofr-alx-exp       to   fil-alx-exp            .
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
           move      rf-ofr-num-prt       to   fil-num-prt            .
           move      rf-ofr-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-ofr-cod-dpz       to   fil-cod-dpz            .
           move      rf-ofr-tip-mag       to   fil-tip-mag            .
           move      rf-ofr-num-mag       to   fil-num-mag            .
           move      rf-ofr-dat-doc       to   fil-dat-doc            .
           move      rf-ofr-num-doc       to   fil-num-doc            .
           move      rf-ofr-num-prt       to   fil-num-prt-2          .
           move      rf-ofr-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-ofr-cod-dpz       to   fil-cod-dpz-3          .
           move      rf-ofr-flg-rch       to   fil-flg-rch            .
           move      rf-ofr-tip-mag       to   fil-tip-mag-3          .
           move      rf-ofr-num-mag       to   fil-num-mag-3          .
           move      rf-ofr-num-prt       to   fil-num-prt-3          .
           move      rf-ofr-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-ofr-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-ofr-flg-rch       to   fil-flg-rch-4          .
           move      rf-ofr-tip-arc       to   fil-tip-arc            .
           move      rf-ofr-cod-arc       to   fil-cod-arc            .
           move      rf-ofr-num-prt       to   fil-num-prt-4          .
           move      rf-ofr-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-ofr-tip-arc       to   fil-tip-arc-5          .
           move      rf-ofr-cod-arc       to   fil-cod-arc-5          .
           move      rf-ofr-tip-mag       to   fil-tip-mag-5          .
           move      rf-ofr-num-mag       to   fil-num-mag-5          .
           move      rf-ofr-dat-doc       to   fil-dat-doc-5          .
           move      rf-ofr-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-ofr-num-prt       to   fil-num-prt-5          .
           move      rf-ofr-num-prg       to   fil-num-prg-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ofr                 .
           move      fil-num-prt          to   rf-ofr-num-prt         .
           move      fil-num-prg          to   rf-ofr-num-prg         .
           move      fil-tmo-orf          to   rf-ofr-tmo-orf         .
           move      fil-cod-dpz          to   rf-ofr-cod-dpz         .
           move      fil-dat-doc          to   rf-ofr-dat-doc         .
           move      fil-num-doc          to   rf-ofr-num-doc         .
           move      fil-tip-arc          to   rf-ofr-tip-arc         .
           move      fil-cod-arc          to   rf-ofr-cod-arc         .
           move      fil-dpz-arc          to   rf-ofr-dpz-arc         .
           move      fil-cod-lng          to   rf-ofr-cod-lng         .
           move      fil-cof-dat          to   rf-ofr-cof-dat         .
           move      fil-cof-num          to   rf-ofr-cof-num         .
           move      fil-pri-eva          to   rf-ofr-pri-eva         .
           move      fil-sgl-vpf          to   rf-ofr-sgl-vpf         .
           move      fil-dec-vpf          to   rf-ofr-dec-vpf         .
           move      fil-tdc-vpf          to   rf-ofr-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-ofr-cdc-vpf         .
           move      fil-bld-flb          to   rf-ofr-bld-flb         .
           move      fil-bld-tpb          to   rf-ofr-bld-tpb         .
           move      fil-bld-rgb          to   rf-ofr-bld-rgb         .
           move      fil-tip-rig          to   rf-ofr-tip-rig         .
           move      fil-tip-mag          to   rf-ofr-tip-mag         .
           move      fil-num-mag          to   rf-ofr-num-mag         .
           move      fil-alf-mag          to   rf-ofr-alf-mag         .
           move      fil-sgl-vrn          to   rf-ofr-sgl-vrn         .
           move      fil-fda-pif          to   rf-ofr-fda-pif         .
           move      fil-cop-sfn          to   rf-ofr-cop-sfn         .
           move      fil-snx-tum          to   rf-ofr-snx-tum         .
           move      fil-umf-tum          to   rf-ofr-umf-tum         .
           move      fil-nde-tum          to   rf-ofr-nde-tum         .
           move      fil-cmo-tum          to   rf-ofr-cmo-tum         .
           move      fil-cdi-tum          to   rf-ofr-cdi-tum         .
           move      fil-des-ext          to   rf-ofr-des-ext         .
           move      fil-des-rig          to   rf-ofr-des-rig         .
           move      fil-tip-pro          to   rf-ofr-tip-pro         .
           move      fil-cod-iva          to   rf-ofr-cod-iva         .
           move      fil-ctp-acq          to   rf-ofr-ctp-acq         .
           move      fil-umi-acq          to   rf-ofr-umi-acq         .
           move      fil-dec-qta          to   rf-ofr-dec-qta         .
           move      fil-qta-fda          to   rf-ofr-qta-fda         .
           move      fil-qta-ord          to   rf-ofr-qta-ord         .
           move      fil-sdr-ccs          to   rf-ofr-sdr-ccs         .
           move      fil-cod-dsl          to   rf-ofr-cod-dsl         .
           move      fil-snx-2qt          to   rf-ofr-snx-2qt         .
           move      fil-dec-2qt          to   rf-ofr-dec-2qt         .
           move      fil-qta-a02          to   rf-ofr-qta-a02         .
           move      fil-snx-3qt          to   rf-ofr-snx-3qt         .
           move      fil-dec-3qt          to   rf-ofr-dec-3qt         .
           move      fil-qta-a03          to   rf-ofr-qta-a03         .
           move      fil-dec-prz          to   rf-ofr-dec-prz         .
           move      fil-sgl-vpp          to   rf-ofr-sgl-vpp         .
           move      fil-dec-vpp          to   rf-ofr-dec-vpp         .
           move      fil-tdc-vpp          to   rf-ofr-tdc-vpp         .
           move      fil-cdc-vpp          to   rf-ofr-cdc-vpp         .
           move      fil-prz-acq          to   rf-ofr-prz-acq         .
           move      fil-snx-2pz          to   rf-ofr-snx-2pz         .
           move      fil-dec-2pz          to   rf-ofr-dec-2pz         .
           move      fil-prz-a02          to   rf-ofr-prz-a02         .
           move      fil-sgl-vpl          to   rf-ofr-sgl-vpl         .
           move      fil-dec-vpl          to   rf-ofr-dec-vpl         .
           move      fil-tdc-vpl          to   rf-ofr-tdc-vpl         .
           move      fil-prz-vpl          to   rf-ofr-prz-vpl         .
           move      fil-cdc-vpl          to   rf-ofr-cdc-vpl         .
           move      fil-ccr-vpl          to   rf-ofr-ccr-vpl         .
           move      fil-plm-vpl          to   rf-ofr-plm-vpl         .
           move      fil-tlm-vpl          to   rf-ofr-tlm-vpl         .
           move      fil-map-vpl          to   rf-ofr-map-vpl         .
           move      fil-epz-rgo          to   rf-ofr-epz-rgo         .
           move      fil-csr-aap          to   rf-ofr-csr-aap         .
           move      fil-psr-aap (1)      to   rf-ofr-psr-aap (1)     .
           move      fil-psr-aap (2)      to   rf-ofr-psr-aap (2)     .
           move      fil-psr-aap (3)      to   rf-ofr-psr-aap (3)     .
           move      fil-psr-aap (4)      to   rf-ofr-psr-aap (4)     .
           move      fil-psr-aap (5)      to   rf-ofr-psr-aap (5)     .
           move      fil-per-scr (1)      to   rf-ofr-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-ofr-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-ofr-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-ofr-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-ofr-per-scr (5)     .
           move      fil-prz-net          to   rf-ofr-prz-net         .
           move      fil-imp-rig          to   rf-ofr-imp-rig         .
           move      fil-iau-rig          to   rf-ofr-iau-rig         .
           move      fil-cpv-aap          to   rf-ofr-cpv-aap         .
           move      fil-ppv-aap (1)      to   rf-ofr-ppv-aap (1)     .
           move      fil-ppv-aap (2)      to   rf-ofr-ppv-aap (2)     .
           move      fil-ppv-aap (3)      to   rf-ofr-ppv-aap (3)     .
           move      fil-fsp-rig          to   rf-ofr-fsp-rig         .
           move      fil-cpv-rig          to   rf-ofr-cpv-rig         .
           move      fil-ppv-rig (1)      to   rf-ofr-ppv-rig (1)     .
           move      fil-ppv-rig (2)      to   rf-ofr-ppv-rig (2)     .
           move      fil-ppv-rig (3)      to   rf-ofr-ppv-rig (3)     .
           move      fil-pvf-rig          to   rf-ofr-pvf-rig         .
           move      fil-dcn-ric          to   rf-ofr-dcn-ric         .
           move      fil-fds-dcr          to   rf-ofr-fds-dcr         .
           move      fil-dcn-prv          to   rf-ofr-dcn-prv         .
           move      fil-dcn-cnf          to   rf-ofr-dcn-cnf         .
           move      fil-flg-cnf          to   rf-ofr-flg-cnf         .
           move      fil-oda-tip          to   rf-ofr-oda-tip         .
           move      fil-oda-dat          to   rf-ofr-oda-dat         .
           move      fil-oda-num          to   rf-ofr-oda-num         .
           move      fil-flg-rch          to   rf-ofr-flg-rch         .
           move      fil-flg-blx (1)      to   rf-ofr-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-ofr-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-ofr-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-ofr-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-ofr-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-ofr-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-ofr-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-ofr-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-ofr-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-ofr-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-ofr-flg-pul         .
           move      fil-rif-lst          to   rf-ofr-rif-lst         .
           move      fil-tip-ord          to   rf-ofr-tip-ord         .
           move      fil-alx-exp          to   rf-ofr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ofr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ofr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

