       Identification Division.
       Program-Id.                                 iofffr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ffr                         *
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
                   15  fil-num-prt-1      pic  9(11)       comp-3     .
                   15  fil-num-prg-1      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz-2      pic  9(02)                  .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-num-mag-2      pic  9(07)       comp-3     .
                   15  fil-dat-reg-2      pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATMAG                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dpz-3      pic  9(02)                  .
                   15  fil-dat-reg-3      pic  9(07)       comp-3     .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-mag-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RIFBFO                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-bfo-prt-4      pic  9(11)       comp-3     .
                   15  fil-bfo-prg-4      pic  9(05)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-num-prt            pic  9(11)       comp-3     .
               10  fil-num-prg            pic  9(05)       comp-3     .
               10  fil-cod-tmf            pic  x(05)                  .
               10  fil-cod-dpz            pic  9(02)                  .
               10  fil-dat-reg            pic  9(07)       comp-3     .
               10  fil-tip-arc            pic  x(01)                  .
               10  fil-cod-arc            pic  9(07)       comp-3     .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-doc            pic  x(10)                  .
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
               10  fil-num-mag            pic  9(07)       comp-3     .
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
               10  fil-qta-acq            pic s9(10)v9(03) comp-3     .
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
               10  fil-epz-rgf            pic  9(01)                  .
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
               10  fil-bfo-tip            pic  x(05)                  .
               10  fil-bfo-dat            pic  9(07)       comp-3     .
               10  fil-bfo-prt            pic  9(11)       comp-3     .
               10  fil-bfo-prg            pic  9(05)       comp-3     .
               10  fil-bfo-ddo            pic  9(07)       comp-3     .
               10  fil-bfo-ndo            pic  x(10)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-lot-dat            pic  9(07)                  .
               10  fil-lot-sgl            pic  x(20)                  .
               10  fil-alx-exp.
                   15  filler  occurs 53  pic  x(01)                  .

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
                   15  pul-num-mag-2      pic  9(07)       comp-3     .
                   15  pul-dat-reg-2      pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATMAG                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dpz-3      pic  9(02)                  .
                   15  pul-dat-reg-3      pic  9(07)       comp-3     .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-mag-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RIFBFO                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-bfo-prt-4      pic  9(11)       comp-3     .
                   15  pul-bfo-prg-4      pic  9(05)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-num-prt            pic  9(11)       comp-3     .
               10  pul-num-prg            pic  9(05)       comp-3     .
               10  pul-cod-tmf            pic  x(05)                  .
               10  pul-cod-dpz            pic  9(02)                  .
               10  pul-dat-reg            pic  9(07)       comp-3     .
               10  pul-tip-arc            pic  x(01)                  .
               10  pul-cod-arc            pic  9(07)       comp-3     .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-doc            pic  x(10)                  .
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
               10  pul-num-mag            pic  9(07)       comp-3     .
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
               10  pul-qta-acq            pic s9(10)v9(03) comp-3     .
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
               10  pul-epz-rgf            pic  9(01)                  .
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
               10  pul-bfo-tip            pic  x(05)                  .
               10  pul-bfo-dat            pic  9(07)       comp-3     .
               10  pul-bfo-prt            pic  9(11)       comp-3     .
               10  pul-bfo-prg            pic  9(05)       comp-3     .
               10  pul-bfo-ddo            pic  9(07)       comp-3     .
               10  pul-bfo-ndo            pic  x(10)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-lot-dat            pic  9(07)                  .
               10  pul-lot-sgl            pic  x(20)                  .
               10  pul-alx-exp.
                   15  filler  occurs 53  pic  x(01)                  .

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
                     "ffr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/ffo/fls/ioc/obj/iofffr              "       .

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
                            "MAGDAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATMAG    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RIFBFO    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    4      pic  x(10)                  .

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
      *    * Record logico file [ffr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .

      ******************************************************************
       Procedure Division                using f rf-ffr               .
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
           move      spaces               to   rf-ffr                 .
           move      zero                 to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      spaces               to   rf-ffr-cod-tmf         .
           move      zero                 to   rf-ffr-cod-dpz         .
           move      zero                 to   rf-ffr-dat-reg         .
           move      spaces               to   rf-ffr-tip-arc         .
           move      zero                 to   rf-ffr-cod-arc         .
           move      spaces               to   rf-ffr-dpz-arc         .
           move      zero                 to   rf-ffr-dat-doc         .
           move      spaces               to   rf-ffr-num-doc         .
           move      spaces               to   rf-ffr-cod-lng         .
           move      spaces               to   rf-ffr-sgl-vpf         .
           move      zero                 to   rf-ffr-dec-vpf         .
           move      spaces               to   rf-ffr-tdc-vpf         .
           move      zero                 to   rf-ffr-cdc-vpf         .
           move      zero                 to   rf-ffr-bld-flb         .
           move      zero                 to   rf-ffr-bld-tpb         .
           move      zero                 to   rf-ffr-bld-rgb         .
           move      spaces               to   rf-ffr-tip-rig         .
           move      zero                 to   rf-ffr-tip-mag         .
           move      zero                 to   rf-ffr-num-mag         .
           move      spaces               to   rf-ffr-alf-mag         .
           move      spaces               to   rf-ffr-sgl-vrn         .
           move      spaces               to   rf-ffr-fda-pif         .
           move      spaces               to   rf-ffr-cop-sfn         .
           move      spaces               to   rf-ffr-snx-tum         .
           move      spaces               to   rf-ffr-umf-tum         .
           move      zero                 to   rf-ffr-nde-tum         .
           move      zero                 to   rf-ffr-cmo-tum         .
           move      zero                 to   rf-ffr-cdi-tum         .
           move      zero                 to   rf-ffr-des-ext         .
           move      spaces               to   rf-ffr-des-rig         .
           move      zero                 to   rf-ffr-tip-pro         .
           move      zero                 to   rf-ffr-cod-iva         .
           move      zero                 to   rf-ffr-ctp-acq         .
           move      spaces               to   rf-ffr-umi-acq         .
           move      zero                 to   rf-ffr-dec-qta         .
           move      zero                 to   rf-ffr-qta-fda         .
           move      zero                 to   rf-ffr-qta-acq         .
           move      spaces               to   rf-ffr-cod-dsl         .
           move      zero                 to   rf-ffr-snx-2qt         .
           move      zero                 to   rf-ffr-dec-2qt         .
           move      zero                 to   rf-ffr-qta-a02         .
           move      zero                 to   rf-ffr-snx-3qt         .
           move      zero                 to   rf-ffr-dec-3qt         .
           move      zero                 to   rf-ffr-qta-a03         .
           move      zero                 to   rf-ffr-dec-prz         .
           move      spaces               to   rf-ffr-sgl-vpp         .
           move      zero                 to   rf-ffr-dec-vpp         .
           move      spaces               to   rf-ffr-tdc-vpp         .
           move      zero                 to   rf-ffr-cdc-vpp         .
           move      zero                 to   rf-ffr-prz-acq         .
           move      zero                 to   rf-ffr-snx-2pz         .
           move      zero                 to   rf-ffr-dec-2pz         .
           move      zero                 to   rf-ffr-prz-a02         .
           move      spaces               to   rf-ffr-sgl-vpl         .
           move      zero                 to   rf-ffr-dec-vpl         .
           move      spaces               to   rf-ffr-tdc-vpl         .
           move      zero                 to   rf-ffr-prz-vpl         .
           move      zero                 to   rf-ffr-cdc-vpl         .
           move      zero                 to   rf-ffr-ccr-vpl         .
           move      zero                 to   rf-ffr-plm-vpl         .
           move      spaces               to   rf-ffr-tlm-vpl         .
           move      spaces               to   rf-ffr-map-vpl         .
           move      zero                 to   rf-ffr-epz-rgf         .
           move      zero                 to   rf-ffr-csr-aap         .
           move      zero                 to   rf-ffr-psr-aap (1)     .
           move      zero                 to   rf-ffr-psr-aap (2)     .
           move      zero                 to   rf-ffr-psr-aap (3)     .
           move      zero                 to   rf-ffr-psr-aap (4)     .
           move      zero                 to   rf-ffr-psr-aap (5)     .
           move      zero                 to   rf-ffr-per-scr (1)     .
           move      zero                 to   rf-ffr-per-scr (2)     .
           move      zero                 to   rf-ffr-per-scr (3)     .
           move      zero                 to   rf-ffr-per-scr (4)     .
           move      zero                 to   rf-ffr-per-scr (5)     .
           move      zero                 to   rf-ffr-prz-net         .
           move      zero                 to   rf-ffr-imp-rig         .
           move      zero                 to   rf-ffr-iau-rig         .
           move      zero                 to   rf-ffr-cpv-aap         .
           move      zero                 to   rf-ffr-ppv-aap (1)     .
           move      zero                 to   rf-ffr-ppv-aap (2)     .
           move      zero                 to   rf-ffr-ppv-aap (3)     .
           move      zero                 to   rf-ffr-fsp-rig         .
           move      zero                 to   rf-ffr-cpv-rig         .
           move      zero                 to   rf-ffr-ppv-rig (1)     .
           move      zero                 to   rf-ffr-ppv-rig (2)     .
           move      zero                 to   rf-ffr-ppv-rig (3)     .
           move      zero                 to   rf-ffr-pvf-rig         .
           move      spaces               to   rf-ffr-bfo-tip         .
           move      zero                 to   rf-ffr-bfo-dat         .
           move      zero                 to   rf-ffr-bfo-prt         .
           move      zero                 to   rf-ffr-bfo-prg         .
           move      zero                 to   rf-ffr-bfo-ddo         .
           move      spaces               to   rf-ffr-bfo-ndo         .
           move      spaces               to   rf-ffr-flg-ela         .
           move      spaces               to   rf-ffr-flg-pul         .
           move      zero                 to   rf-ffr-lot-dat         .
           move      spaces               to   rf-ffr-lot-sgl         .
           move      spaces               to   rf-ffr-alx-exp         .
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
           move      rf-ffr-num-prt       to   fil-num-prt            .
           move      rf-ffr-num-prg       to   fil-num-prg            .
           move      rf-ffr-cod-tmf       to   fil-cod-tmf            .
           move      rf-ffr-cod-dpz       to   fil-cod-dpz            .
           move      rf-ffr-dat-reg       to   fil-dat-reg            .
           move      rf-ffr-tip-arc       to   fil-tip-arc            .
           move      rf-ffr-cod-arc       to   fil-cod-arc            .
           move      rf-ffr-dpz-arc       to   fil-dpz-arc            .
           move      rf-ffr-dat-doc       to   fil-dat-doc            .
           move      rf-ffr-num-doc       to   fil-num-doc            .
           move      rf-ffr-cod-lng       to   fil-cod-lng            .
           move      rf-ffr-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-ffr-dec-vpf       to   fil-dec-vpf            .
           move      rf-ffr-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-ffr-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-ffr-bld-flb       to   fil-bld-flb            .
           move      rf-ffr-bld-tpb       to   fil-bld-tpb            .
           move      rf-ffr-bld-rgb       to   fil-bld-rgb            .
           move      rf-ffr-tip-rig       to   fil-tip-rig            .
           move      rf-ffr-tip-mag       to   fil-tip-mag            .
           move      rf-ffr-num-mag       to   fil-num-mag            .
           move      rf-ffr-alf-mag       to   fil-alf-mag            .
           move      rf-ffr-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-ffr-fda-pif       to   fil-fda-pif            .
           move      rf-ffr-cop-sfn       to   fil-cop-sfn            .
           move      rf-ffr-snx-tum       to   fil-snx-tum            .
           move      rf-ffr-umf-tum       to   fil-umf-tum            .
           move      rf-ffr-nde-tum       to   fil-nde-tum            .
           move      rf-ffr-cmo-tum       to   fil-cmo-tum            .
           move      rf-ffr-cdi-tum       to   fil-cdi-tum            .
           move      rf-ffr-des-ext       to   fil-des-ext            .
           move      rf-ffr-des-rig       to   fil-des-rig            .
           move      rf-ffr-tip-pro       to   fil-tip-pro            .
           move      rf-ffr-cod-iva       to   fil-cod-iva            .
           move      rf-ffr-ctp-acq       to   fil-ctp-acq            .
           move      rf-ffr-umi-acq       to   fil-umi-acq            .
           move      rf-ffr-dec-qta       to   fil-dec-qta            .
           move      rf-ffr-qta-fda       to   fil-qta-fda            .
           move      rf-ffr-qta-acq       to   fil-qta-acq            .
           move      rf-ffr-cod-dsl       to   fil-cod-dsl            .
           move      rf-ffr-snx-2qt       to   fil-snx-2qt            .
           move      rf-ffr-dec-2qt       to   fil-dec-2qt            .
           move      rf-ffr-qta-a02       to   fil-qta-a02            .
           move      rf-ffr-snx-3qt       to   fil-snx-3qt            .
           move      rf-ffr-dec-3qt       to   fil-dec-3qt            .
           move      rf-ffr-qta-a03       to   fil-qta-a03            .
           move      rf-ffr-dec-prz       to   fil-dec-prz            .
           move      rf-ffr-sgl-vpp       to   fil-sgl-vpp            .
           move      rf-ffr-dec-vpp       to   fil-dec-vpp            .
           move      rf-ffr-tdc-vpp       to   fil-tdc-vpp            .
           move      rf-ffr-cdc-vpp       to   fil-cdc-vpp            .
           move      rf-ffr-prz-acq       to   fil-prz-acq            .
           move      rf-ffr-snx-2pz       to   fil-snx-2pz            .
           move      rf-ffr-dec-2pz       to   fil-dec-2pz            .
           move      rf-ffr-prz-a02       to   fil-prz-a02            .
           move      rf-ffr-sgl-vpl       to   fil-sgl-vpl            .
           move      rf-ffr-dec-vpl       to   fil-dec-vpl            .
           move      rf-ffr-tdc-vpl       to   fil-tdc-vpl            .
           move      rf-ffr-prz-vpl       to   fil-prz-vpl            .
           move      rf-ffr-cdc-vpl       to   fil-cdc-vpl            .
           move      rf-ffr-ccr-vpl       to   fil-ccr-vpl            .
           move      rf-ffr-plm-vpl       to   fil-plm-vpl            .
           move      rf-ffr-tlm-vpl       to   fil-tlm-vpl            .
           move      rf-ffr-map-vpl       to   fil-map-vpl            .
           move      rf-ffr-epz-rgf       to   fil-epz-rgf            .
           move      rf-ffr-csr-aap       to   fil-csr-aap            .
           move      rf-ffr-psr-aap (1)   to   fil-psr-aap (1)        .
           move      rf-ffr-psr-aap (2)   to   fil-psr-aap (2)        .
           move      rf-ffr-psr-aap (3)   to   fil-psr-aap (3)        .
           move      rf-ffr-psr-aap (4)   to   fil-psr-aap (4)        .
           move      rf-ffr-psr-aap (5)   to   fil-psr-aap (5)        .
           move      rf-ffr-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-ffr-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-ffr-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-ffr-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-ffr-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-ffr-prz-net       to   fil-prz-net            .
           move      rf-ffr-imp-rig       to   fil-imp-rig            .
           move      rf-ffr-iau-rig       to   fil-iau-rig            .
           move      rf-ffr-cpv-aap       to   fil-cpv-aap            .
           move      rf-ffr-ppv-aap (1)   to   fil-ppv-aap (1)        .
           move      rf-ffr-ppv-aap (2)   to   fil-ppv-aap (2)        .
           move      rf-ffr-ppv-aap (3)   to   fil-ppv-aap (3)        .
           move      rf-ffr-fsp-rig       to   fil-fsp-rig            .
           move      rf-ffr-cpv-rig       to   fil-cpv-rig            .
           move      rf-ffr-ppv-rig (1)   to   fil-ppv-rig (1)        .
           move      rf-ffr-ppv-rig (2)   to   fil-ppv-rig (2)        .
           move      rf-ffr-ppv-rig (3)   to   fil-ppv-rig (3)        .
           move      rf-ffr-pvf-rig       to   fil-pvf-rig            .
           move      rf-ffr-bfo-tip       to   fil-bfo-tip            .
           move      rf-ffr-bfo-dat       to   fil-bfo-dat            .
           move      rf-ffr-bfo-prt       to   fil-bfo-prt            .
           move      rf-ffr-bfo-prg       to   fil-bfo-prg            .
           move      rf-ffr-bfo-ddo       to   fil-bfo-ddo            .
           move      rf-ffr-bfo-ndo       to   fil-bfo-ndo            .
           move      rf-ffr-flg-ela       to   fil-flg-ela            .
           move      rf-ffr-flg-pul       to   fil-flg-pul            .
           move      rf-ffr-lot-dat       to   fil-lot-dat            .
           move      rf-ffr-lot-sgl       to   fil-lot-sgl            .
           move      rf-ffr-alx-exp       to   fil-alx-exp            .
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
           move      rf-ffr-num-prt       to   fil-num-prt-1          .
           move      rf-ffr-num-prg       to   fil-num-prg-1          .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-ffr-cod-dpz       to   fil-cod-dpz-2          .
           move      rf-ffr-tip-mag       to   fil-tip-mag-2          .
           move      rf-ffr-num-mag       to   fil-num-mag-2          .
           move      rf-ffr-dat-reg       to   fil-dat-reg-2          .
           move      rf-ffr-num-prt       to   fil-num-prt-2          .
           move      rf-ffr-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-ffr-cod-dpz       to   fil-cod-dpz-3          .
           move      rf-ffr-dat-reg       to   fil-dat-reg-3          .
           move      rf-ffr-dat-reg       to   fil-dat-reg-3          .
           move      rf-ffr-tip-mag       to   fil-tip-mag-3          .
           move      rf-ffr-num-mag       to   fil-num-mag-3          .
           move      rf-ffr-num-prt       to   fil-num-prt-3          .
           move      rf-ffr-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-ffr-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-ffr-bfo-prt       to   fil-bfo-prt-4          .
           move      rf-ffr-bfo-prg       to   fil-bfo-prg-4          .
           move      rf-ffr-num-prt       to   fil-num-prt-4          .
           move      rf-ffr-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ffr                 .
           move      fil-num-prt          to   rf-ffr-num-prt         .
           move      fil-num-prg          to   rf-ffr-num-prg         .
           move      fil-cod-tmf          to   rf-ffr-cod-tmf         .
           move      fil-cod-dpz          to   rf-ffr-cod-dpz         .
           move      fil-dat-reg          to   rf-ffr-dat-reg         .
           move      fil-tip-arc          to   rf-ffr-tip-arc         .
           move      fil-cod-arc          to   rf-ffr-cod-arc         .
           move      fil-dpz-arc          to   rf-ffr-dpz-arc         .
           move      fil-dat-doc          to   rf-ffr-dat-doc         .
           move      fil-num-doc          to   rf-ffr-num-doc         .
           move      fil-cod-lng          to   rf-ffr-cod-lng         .
           move      fil-sgl-vpf          to   rf-ffr-sgl-vpf         .
           move      fil-dec-vpf          to   rf-ffr-dec-vpf         .
           move      fil-tdc-vpf          to   rf-ffr-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-ffr-cdc-vpf         .
           move      fil-bld-flb          to   rf-ffr-bld-flb         .
           move      fil-bld-tpb          to   rf-ffr-bld-tpb         .
           move      fil-bld-rgb          to   rf-ffr-bld-rgb         .
           move      fil-tip-rig          to   rf-ffr-tip-rig         .
           move      fil-tip-mag          to   rf-ffr-tip-mag         .
           move      fil-num-mag          to   rf-ffr-num-mag         .
           move      fil-alf-mag          to   rf-ffr-alf-mag         .
           move      fil-sgl-vrn          to   rf-ffr-sgl-vrn         .
           move      fil-fda-pif          to   rf-ffr-fda-pif         .
           move      fil-cop-sfn          to   rf-ffr-cop-sfn         .
           move      fil-snx-tum          to   rf-ffr-snx-tum         .
           move      fil-umf-tum          to   rf-ffr-umf-tum         .
           move      fil-nde-tum          to   rf-ffr-nde-tum         .
           move      fil-cmo-tum          to   rf-ffr-cmo-tum         .
           move      fil-cdi-tum          to   rf-ffr-cdi-tum         .
           move      fil-des-ext          to   rf-ffr-des-ext         .
           move      fil-des-rig          to   rf-ffr-des-rig         .
           move      fil-tip-pro          to   rf-ffr-tip-pro         .
           move      fil-cod-iva          to   rf-ffr-cod-iva         .
           move      fil-ctp-acq          to   rf-ffr-ctp-acq         .
           move      fil-umi-acq          to   rf-ffr-umi-acq         .
           move      fil-dec-qta          to   rf-ffr-dec-qta         .
           move      fil-qta-fda          to   rf-ffr-qta-fda         .
           move      fil-qta-acq          to   rf-ffr-qta-acq         .
           move      fil-cod-dsl          to   rf-ffr-cod-dsl         .
           move      fil-snx-2qt          to   rf-ffr-snx-2qt         .
           move      fil-dec-2qt          to   rf-ffr-dec-2qt         .
           move      fil-qta-a02          to   rf-ffr-qta-a02         .
           move      fil-snx-3qt          to   rf-ffr-snx-3qt         .
           move      fil-dec-3qt          to   rf-ffr-dec-3qt         .
           move      fil-qta-a03          to   rf-ffr-qta-a03         .
           move      fil-dec-prz          to   rf-ffr-dec-prz         .
           move      fil-sgl-vpp          to   rf-ffr-sgl-vpp         .
           move      fil-dec-vpp          to   rf-ffr-dec-vpp         .
           move      fil-tdc-vpp          to   rf-ffr-tdc-vpp         .
           move      fil-cdc-vpp          to   rf-ffr-cdc-vpp         .
           move      fil-prz-acq          to   rf-ffr-prz-acq         .
           move      fil-snx-2pz          to   rf-ffr-snx-2pz         .
           move      fil-dec-2pz          to   rf-ffr-dec-2pz         .
           move      fil-prz-a02          to   rf-ffr-prz-a02         .
           move      fil-sgl-vpl          to   rf-ffr-sgl-vpl         .
           move      fil-dec-vpl          to   rf-ffr-dec-vpl         .
           move      fil-tdc-vpl          to   rf-ffr-tdc-vpl         .
           move      fil-prz-vpl          to   rf-ffr-prz-vpl         .
           move      fil-cdc-vpl          to   rf-ffr-cdc-vpl         .
           move      fil-ccr-vpl          to   rf-ffr-ccr-vpl         .
           move      fil-plm-vpl          to   rf-ffr-plm-vpl         .
           move      fil-tlm-vpl          to   rf-ffr-tlm-vpl         .
           move      fil-map-vpl          to   rf-ffr-map-vpl         .
           move      fil-epz-rgf          to   rf-ffr-epz-rgf         .
           move      fil-csr-aap          to   rf-ffr-csr-aap         .
           move      fil-psr-aap (1)      to   rf-ffr-psr-aap (1)     .
           move      fil-psr-aap (2)      to   rf-ffr-psr-aap (2)     .
           move      fil-psr-aap (3)      to   rf-ffr-psr-aap (3)     .
           move      fil-psr-aap (4)      to   rf-ffr-psr-aap (4)     .
           move      fil-psr-aap (5)      to   rf-ffr-psr-aap (5)     .
           move      fil-per-scr (1)      to   rf-ffr-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-ffr-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-ffr-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-ffr-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-ffr-per-scr (5)     .
           move      fil-prz-net          to   rf-ffr-prz-net         .
           move      fil-imp-rig          to   rf-ffr-imp-rig         .
           move      fil-iau-rig          to   rf-ffr-iau-rig         .
           move      fil-cpv-aap          to   rf-ffr-cpv-aap         .
           move      fil-ppv-aap (1)      to   rf-ffr-ppv-aap (1)     .
           move      fil-ppv-aap (2)      to   rf-ffr-ppv-aap (2)     .
           move      fil-ppv-aap (3)      to   rf-ffr-ppv-aap (3)     .
           move      fil-fsp-rig          to   rf-ffr-fsp-rig         .
           move      fil-cpv-rig          to   rf-ffr-cpv-rig         .
           move      fil-ppv-rig (1)      to   rf-ffr-ppv-rig (1)     .
           move      fil-ppv-rig (2)      to   rf-ffr-ppv-rig (2)     .
           move      fil-ppv-rig (3)      to   rf-ffr-ppv-rig (3)     .
           move      fil-pvf-rig          to   rf-ffr-pvf-rig         .
           move      fil-bfo-tip          to   rf-ffr-bfo-tip         .
           move      fil-bfo-dat          to   rf-ffr-bfo-dat         .
           move      fil-bfo-prt          to   rf-ffr-bfo-prt         .
           move      fil-bfo-prg          to   rf-ffr-bfo-prg         .
           move      fil-bfo-ddo          to   rf-ffr-bfo-ddo         .
           move      fil-bfo-ndo          to   rf-ffr-bfo-ndo         .
           move      fil-flg-ela          to   rf-ffr-flg-ela         .
           move      fil-flg-pul          to   rf-ffr-flg-pul         .
           move      fil-lot-dat          to   rf-ffr-lot-dat         .
           move      fil-lot-sgl          to   rf-ffr-lot-sgl         .
           move      fil-alx-exp          to   rf-ffr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ffr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ffr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

