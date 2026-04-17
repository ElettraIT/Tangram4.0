       Identification Division.
       Program-Id.                                 iofsdb             .
      *================================================================*
      *                                                                *
      *                  Input-Output File sdb                         *
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
      *            * Chiave numero 01 : NUMSDB                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-sdb        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-sdb-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DTSNRS                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dts-sdb        pic  9(07)       comp-3     .
                   15  fil-num-sdb-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DBTDTS                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-tip-dbt        pic  9(02)                  .
                   15  fil-cod-dbt        pic  9(07)       comp-3     .
                   15  fil-dts-sdb-4      pic  9(07)       comp-3     .
                   15  fil-num-sdb-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DDPNRS                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-num-ddp        pic  9(11)       comp-3     .
                   15  fil-num-sdb-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : RISSDB                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-num-ris        pic  9(11)       comp-3     .
                   15  fil-num-sdb-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DBTRIS                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-tip-dbt-7      pic  9(02)                  .
                   15  fil-cod-dbt-7      pic  9(07)       comp-3     .
                   15  fil-num-ris-7      pic  9(11)       comp-3     .
                   15  fil-num-sdb-7      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : PFCNRS                         *
      *            *---------------------------------------------------*
               10  fil-k08.
                   15  fil-prt-fcl        pic  9(09)       comp-3     .
                   15  fil-num-sdb-8      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : DATCHS                         *
      *            *---------------------------------------------------*
               10  fil-k09.
                   15  fil-dat-chs        pic  9(07)       comp-3     .
                   15  fil-num-sdb-9      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dtr-emi            pic  9(07)       comp-3     .
               10  fil-drc-emi            pic  9(07)       comp-3     .
               10  fil-npc-emi            pic  9(07)       comp-3     .
               10  fil-nsc-org            pic  9(11)       comp-3     .
               10  fil-cod-age            pic  9(07)       comp-3     .
               10  fil-tip-sdb            pic  9(02)                  .
               10  fil-snx-dlc            pic  x(01)                  .
               10  fil-tac-sdb            pic  9(02)                  .
               10  fil-tvl-sdb            pic  9(02)                  .
               10  fil-dpz-dbt            pic  x(04)                  .
               10  fil-inl-sdb            pic  9(02)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-tdc-vlt            pic  x(01)                  .
               10  fil-cdc-sdb            pic  9(06)v9(05) comp-3     .
               10  fil-iiv-sdb            pic s9(11)       comp-3     .
               10  fil-imp-sdb            pic s9(11)       comp-3     .
               10  fil-dat-ddr            pic  9(07)       comp-3     .
               10  fil-num-ddr            pic  x(10)                  .
               10  fil-tip-ddr            pic  9(02)                  .
               10  fil-imp-ddr            pic s9(11)       comp-3     .
               10  fil-nra-ddr            pic  9(02)                  .
               10  fil-abi-app            pic  9(05)       comp-3     .
               10  fil-cab-app            pic  9(05)       comp-3     .
               10  fil-ccc-app            pic  x(12)                  .
               10  fil-cod-cbp            pic  x(10)                  .
               10  fil-dtr-sto            pic  9(07)       comp-3     .
               10  fil-drc-sto            pic  9(07)       comp-3     .
               10  fil-npc-sto            pic  9(07)       comp-3     .
               10  fil-ens-sto            pic  9(02)                  .
               10  fil-nns-sto            pic  9(11)       comp-3     .
               10  fil-tns-sto            pic  9(02)                  .
               10  fil-dtr-ris            pic  9(07)       comp-3     .
               10  fil-mod-ris            pic  9(02)                  .
               10  fil-imp-ris            pic s9(11)       comp-3     .
               10  fil-aos-ris            pic  9(02)                  .
               10  fil-nns-ris            pic  9(11)       comp-3     .
               10  fil-tav-ott            pic  9(02)                  .
               10  fil-dtr-rsp            pic  9(07)       comp-3     .
               10  fil-drc-rsp            pic  9(07)       comp-3     .
               10  fil-npc-rsp            pic  9(07)       comp-3     .
               10  fil-dcb-rsp            pic  9(07)       comp-3     .
               10  fil-ncb-rsp            pic  x(10)                  .
               10  fil-spe-rsp            pic s9(11)       comp-3     .
               10  fil-ens-rsp            pic  9(02)                  .
               10  fil-nns-rsp            pic  9(11)       comp-3     .
               10  fil-tns-rsp            pic  9(02)                  .
               10  fil-dtr-acs            pic  9(07)       comp-3     .
               10  fil-drc-acs            pic  9(07)       comp-3     .
               10  fil-npc-acs            pic  9(07)       comp-3     .
               10  fil-dcb-acs            pic  9(07)       comp-3     .
               10  fil-ncb-acs            pic  x(10)                  .
               10  fil-spe-acs            pic s9(11)       comp-3     .
               10  fil-dtr-nbe            pic  9(07)       comp-3     .
               10  fil-drc-nbe            pic  9(07)       comp-3     .
               10  fil-npc-nbe            pic  9(07)       comp-3     .
               10  fil-dcb-nbe            pic  9(07)       comp-3     .
               10  fil-ncb-nbe            pic  x(10)                  .
               10  fil-dtr-pbe            pic  9(07)       comp-3     .
               10  fil-drc-pbe            pic  9(07)       comp-3     .
               10  fil-npc-pbe            pic  9(07)       comp-3     .
               10  fil-dtr-isp            pic  9(07)       comp-3     .
               10  fil-drc-isp            pic  9(07)       comp-3     .
               10  fil-npc-isp            pic  9(07)       comp-3     .
               10  fil-dcb-isp            pic  9(07)       comp-3     .
               10  fil-ncb-isp            pic  x(10)                  .
               10  fil-spe-isp            pic s9(11)       comp-3     .
               10  fil-ens-isp            pic  9(02)                  .
               10  fil-nns-isp            pic  9(11)       comp-3     .
               10  fil-tns-isp            pic  9(02)                  .
               10  fil-liv-slc            pic  9(01)                  .
               10  fil-dso-l01            pic  9(07)       comp-3     .
               10  fil-dso-l02            pic  9(07)       comp-3     .
               10  fil-dso-l03            pic  9(07)       comp-3     .
               10  fil-flc-stp            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-snx-cts            pic  x(01)                  .
               10  fil-vet-cts            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler occurs 12   pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMSDB                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-sdb        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-sdb-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DTSNRS                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dts-sdb        pic  9(07)       comp-3     .
                   15  pul-num-sdb-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DBTDTS                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-tip-dbt        pic  9(02)                  .
                   15  pul-cod-dbt        pic  9(07)       comp-3     .
                   15  pul-dts-sdb-4      pic  9(07)       comp-3     .
                   15  pul-num-sdb-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DDPNRS                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-num-ddp        pic  9(11)       comp-3     .
                   15  pul-num-sdb-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : RISSDB                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-num-ris        pic  9(11)       comp-3     .
                   15  pul-num-sdb-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DBTRIS                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-tip-dbt-7      pic  9(02)                  .
                   15  pul-cod-dbt-7      pic  9(07)       comp-3     .
                   15  pul-num-ris-7      pic  9(11)       comp-3     .
                   15  pul-num-sdb-7      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : PFCNRS                         *
      *            *---------------------------------------------------*
               10  pul-k08.
                   15  pul-prt-fcl        pic  9(09)       comp-3     .
                   15  pul-num-sdb-8      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : DATCHS                         *
      *            *---------------------------------------------------*
               10  pul-k09.
                   15  pul-dat-chs        pic  9(07)       comp-3     .
                   15  pul-num-sdb-9      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dtr-emi            pic  9(07)       comp-3     .
               10  pul-drc-emi            pic  9(07)       comp-3     .
               10  pul-npc-emi            pic  9(07)       comp-3     .
               10  pul-nsc-org            pic  9(11)       comp-3     .
               10  pul-cod-age            pic  9(07)       comp-3     .
               10  pul-tip-sdb            pic  9(02)                  .
               10  pul-snx-dlc            pic  x(01)                  .
               10  pul-tac-sdb            pic  9(02)                  .
               10  pul-tvl-sdb            pic  9(02)                  .
               10  pul-dpz-dbt            pic  x(04)                  .
               10  pul-inl-sdb            pic  9(02)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-tdc-vlt            pic  x(01)                  .
               10  pul-cdc-sdb            pic  9(06)v9(05) comp-3     .
               10  pul-iiv-sdb            pic s9(11)       comp-3     .
               10  pul-imp-sdb            pic s9(11)       comp-3     .
               10  pul-dat-ddr            pic  9(07)       comp-3     .
               10  pul-num-ddr            pic  x(10)                  .
               10  pul-tip-ddr            pic  9(02)                  .
               10  pul-imp-ddr            pic s9(11)       comp-3     .
               10  pul-nra-ddr            pic  9(02)                  .
               10  pul-abi-app            pic  9(05)       comp-3     .
               10  pul-cab-app            pic  9(05)       comp-3     .
               10  pul-ccc-app            pic  x(12)                  .
               10  pul-cod-cbp            pic  x(10)                  .
               10  pul-dtr-sto            pic  9(07)       comp-3     .
               10  pul-drc-sto            pic  9(07)       comp-3     .
               10  pul-npc-sto            pic  9(07)       comp-3     .
               10  pul-ens-sto            pic  9(02)                  .
               10  pul-nns-sto            pic  9(11)       comp-3     .
               10  pul-tns-sto            pic  9(02)                  .
               10  pul-dtr-ris            pic  9(07)       comp-3     .
               10  pul-mod-ris            pic  9(02)                  .
               10  pul-imp-ris            pic s9(11)       comp-3     .
               10  pul-aos-ris            pic  9(02)                  .
               10  pul-nns-ris            pic  9(11)       comp-3     .
               10  pul-tav-ott            pic  9(02)                  .
               10  pul-dtr-rsp            pic  9(07)       comp-3     .
               10  pul-drc-rsp            pic  9(07)       comp-3     .
               10  pul-npc-rsp            pic  9(07)       comp-3     .
               10  pul-dcb-rsp            pic  9(07)       comp-3     .
               10  pul-ncb-rsp            pic  x(10)                  .
               10  pul-spe-rsp            pic s9(11)       comp-3     .
               10  pul-ens-rsp            pic  9(02)                  .
               10  pul-nns-rsp            pic  9(11)       comp-3     .
               10  pul-tns-rsp            pic  9(02)                  .
               10  pul-dtr-acs            pic  9(07)       comp-3     .
               10  pul-drc-acs            pic  9(07)       comp-3     .
               10  pul-npc-acs            pic  9(07)       comp-3     .
               10  pul-dcb-acs            pic  9(07)       comp-3     .
               10  pul-ncb-acs            pic  x(10)                  .
               10  pul-spe-acs            pic s9(11)       comp-3     .
               10  pul-dtr-nbe            pic  9(07)       comp-3     .
               10  pul-drc-nbe            pic  9(07)       comp-3     .
               10  pul-npc-nbe            pic  9(07)       comp-3     .
               10  pul-dcb-nbe            pic  9(07)       comp-3     .
               10  pul-ncb-nbe            pic  x(10)                  .
               10  pul-dtr-pbe            pic  9(07)       comp-3     .
               10  pul-drc-pbe            pic  9(07)       comp-3     .
               10  pul-npc-pbe            pic  9(07)       comp-3     .
               10  pul-dtr-isp            pic  9(07)       comp-3     .
               10  pul-drc-isp            pic  9(07)       comp-3     .
               10  pul-npc-isp            pic  9(07)       comp-3     .
               10  pul-dcb-isp            pic  9(07)       comp-3     .
               10  pul-ncb-isp            pic  x(10)                  .
               10  pul-spe-isp            pic s9(11)       comp-3     .
               10  pul-ens-isp            pic  9(02)                  .
               10  pul-nns-isp            pic  9(11)       comp-3     .
               10  pul-tns-isp            pic  9(02)                  .
               10  pul-liv-slc            pic  9(01)                  .
               10  pul-dso-l01            pic  9(07)       comp-3     .
               10  pul-dso-l02            pic  9(07)       comp-3     .
               10  pul-dso-l03            pic  9(07)       comp-3     .
               10  pul-flc-stp            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-snx-cts            pic  x(01)                  .
               10  pul-vet-cts            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler occurs 12   pic  x(01)                  .

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
                     "sdb "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/gep/fls/ioc/obj/iofsdb              "       .

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
                            "NUMSDB    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DTSNRS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DBTDTS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DDPNRS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RISSDB    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DBTRIS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 8                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PFCNRS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 9                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATCHS    "                              .
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
      *    * Record logico file [sdb]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .

      ******************************************************************
       Procedure Division                using f rf-sdb               .
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
           move      spaces               to   rf-sdb                 .
           move      zero                 to   rf-sdb-ide-dat         .
           move      spaces               to   rf-sdb-ide-ute         .
           move      spaces               to   rf-sdb-ide-fas         .
           move      zero                 to   rf-sdb-dtr-emi         .
           move      zero                 to   rf-sdb-drc-emi         .
           move      zero                 to   rf-sdb-npc-emi         .
           move      zero                 to   rf-sdb-prt-fcl         .
           move      zero                 to   rf-sdb-nsc-org         .
           move      zero                 to   rf-sdb-cod-age         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      zero                 to   rf-sdb-tip-sdb         .
           move      spaces               to   rf-sdb-snx-dlc         .
           move      zero                 to   rf-sdb-tac-sdb         .
           move      zero                 to   rf-sdb-tvl-sdb         .
           move      zero                 to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-tip-dbt         .
           move      zero                 to   rf-sdb-cod-dbt         .
           move      spaces               to   rf-sdb-dpz-dbt         .
           move      zero                 to   rf-sdb-inl-sdb         .
           move      spaces               to   rf-sdb-sgl-vlt         .
           move      zero                 to   rf-sdb-dec-vlt         .
           move      spaces               to   rf-sdb-tdc-vlt         .
           move      zero                 to   rf-sdb-cdc-sdb         .
           move      zero                 to   rf-sdb-iiv-sdb         .
           move      zero                 to   rf-sdb-imp-sdb         .
           move      zero                 to   rf-sdb-dat-ddr         .
           move      spaces               to   rf-sdb-num-ddr         .
           move      zero                 to   rf-sdb-tip-ddr         .
           move      zero                 to   rf-sdb-imp-ddr         .
           move      zero                 to   rf-sdb-nra-ddr         .
           move      zero                 to   rf-sdb-abi-app         .
           move      zero                 to   rf-sdb-cab-app         .
           move      spaces               to   rf-sdb-ccc-app         .
           move      spaces               to   rf-sdb-cod-cbp         .
           move      zero                 to   rf-sdb-dtr-sto         .
           move      zero                 to   rf-sdb-drc-sto         .
           move      zero                 to   rf-sdb-npc-sto         .
           move      zero                 to   rf-sdb-ens-sto         .
           move      zero                 to   rf-sdb-nns-sto         .
           move      zero                 to   rf-sdb-tns-sto         .
           move      zero                 to   rf-sdb-dtr-ris         .
           move      zero                 to   rf-sdb-mod-ris         .
           move      zero                 to   rf-sdb-num-ris         .
           move      zero                 to   rf-sdb-imp-ris         .
           move      zero                 to   rf-sdb-aos-ris         .
           move      zero                 to   rf-sdb-nns-ris         .
           move      zero                 to   rf-sdb-num-ddp         .
           move      zero                 to   rf-sdb-tav-ott         .
           move      zero                 to   rf-sdb-dtr-rsp         .
           move      zero                 to   rf-sdb-drc-rsp         .
           move      zero                 to   rf-sdb-npc-rsp         .
           move      zero                 to   rf-sdb-dcb-rsp         .
           move      spaces               to   rf-sdb-ncb-rsp         .
           move      zero                 to   rf-sdb-spe-rsp         .
           move      zero                 to   rf-sdb-ens-rsp         .
           move      zero                 to   rf-sdb-nns-rsp         .
           move      zero                 to   rf-sdb-tns-rsp         .
           move      zero                 to   rf-sdb-dtr-acs         .
           move      zero                 to   rf-sdb-drc-acs         .
           move      zero                 to   rf-sdb-npc-acs         .
           move      zero                 to   rf-sdb-dcb-acs         .
           move      spaces               to   rf-sdb-ncb-acs         .
           move      zero                 to   rf-sdb-spe-acs         .
           move      zero                 to   rf-sdb-dtr-nbe         .
           move      zero                 to   rf-sdb-drc-nbe         .
           move      zero                 to   rf-sdb-npc-nbe         .
           move      zero                 to   rf-sdb-dcb-nbe         .
           move      spaces               to   rf-sdb-ncb-nbe         .
           move      zero                 to   rf-sdb-dtr-pbe         .
           move      zero                 to   rf-sdb-drc-pbe         .
           move      zero                 to   rf-sdb-npc-pbe         .
           move      zero                 to   rf-sdb-dtr-isp         .
           move      zero                 to   rf-sdb-drc-isp         .
           move      zero                 to   rf-sdb-npc-isp         .
           move      zero                 to   rf-sdb-dcb-isp         .
           move      spaces               to   rf-sdb-ncb-isp         .
           move      zero                 to   rf-sdb-spe-isp         .
           move      zero                 to   rf-sdb-ens-isp         .
           move      zero                 to   rf-sdb-nns-isp         .
           move      zero                 to   rf-sdb-tns-isp         .
           move      zero                 to   rf-sdb-liv-slc         .
           move      zero                 to   rf-sdb-dso-l01         .
           move      zero                 to   rf-sdb-dso-l02         .
           move      zero                 to   rf-sdb-dso-l03         .
           move      zero                 to   rf-sdb-dat-chs         .
           move      zero                 to   rf-sdb-flc-stp         .
           move      spaces               to   rf-sdb-flg-ela         .
           move      spaces               to   rf-sdb-flg-pul         .
           move      spaces               to   rf-sdb-snx-cts         .
           move      zero                 to   rf-sdb-vet-cts         .
           move      spaces               to   rf-sdb-alx-exp         .
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
           move      rf-sdb-ide-ute       to   fil-ide-ute            .
           move      rf-sdb-ide-fas       to   fil-ide-fas            .
           move      rf-sdb-dtr-emi       to   fil-dtr-emi            .
           move      rf-sdb-drc-emi       to   fil-drc-emi            .
           move      rf-sdb-npc-emi       to   fil-npc-emi            .
           move      rf-sdb-nsc-org       to   fil-nsc-org            .
           move      rf-sdb-cod-age       to   fil-cod-age            .
           move      rf-sdb-tip-sdb       to   fil-tip-sdb            .
           move      rf-sdb-snx-dlc       to   fil-snx-dlc            .
           move      rf-sdb-tac-sdb       to   fil-tac-sdb            .
           move      rf-sdb-tvl-sdb       to   fil-tvl-sdb            .
           move      rf-sdb-dpz-dbt       to   fil-dpz-dbt            .
           move      rf-sdb-inl-sdb       to   fil-inl-sdb            .
           move      rf-sdb-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-sdb-dec-vlt       to   fil-dec-vlt            .
           move      rf-sdb-tdc-vlt       to   fil-tdc-vlt            .
           move      rf-sdb-cdc-sdb       to   fil-cdc-sdb            .
           move      rf-sdb-iiv-sdb       to   fil-iiv-sdb            .
           move      rf-sdb-imp-sdb       to   fil-imp-sdb            .
           move      rf-sdb-dat-ddr       to   fil-dat-ddr            .
           move      rf-sdb-num-ddr       to   fil-num-ddr            .
           move      rf-sdb-tip-ddr       to   fil-tip-ddr            .
           move      rf-sdb-imp-ddr       to   fil-imp-ddr            .
           move      rf-sdb-nra-ddr       to   fil-nra-ddr            .
           move      rf-sdb-abi-app       to   fil-abi-app            .
           move      rf-sdb-cab-app       to   fil-cab-app            .
           move      rf-sdb-ccc-app       to   fil-ccc-app            .
           move      rf-sdb-cod-cbp       to   fil-cod-cbp            .
           move      rf-sdb-dtr-sto       to   fil-dtr-sto            .
           move      rf-sdb-drc-sto       to   fil-drc-sto            .
           move      rf-sdb-npc-sto       to   fil-npc-sto            .
           move      rf-sdb-ens-sto       to   fil-ens-sto            .
           move      rf-sdb-nns-sto       to   fil-nns-sto            .
           move      rf-sdb-tns-sto       to   fil-tns-sto            .
           move      rf-sdb-dtr-ris       to   fil-dtr-ris            .
           move      rf-sdb-mod-ris       to   fil-mod-ris            .
           move      rf-sdb-imp-ris       to   fil-imp-ris            .
           move      rf-sdb-aos-ris       to   fil-aos-ris            .
           move      rf-sdb-nns-ris       to   fil-nns-ris            .
           move      rf-sdb-tav-ott       to   fil-tav-ott            .
           move      rf-sdb-dtr-rsp       to   fil-dtr-rsp            .
           move      rf-sdb-drc-rsp       to   fil-drc-rsp            .
           move      rf-sdb-npc-rsp       to   fil-npc-rsp            .
           move      rf-sdb-dcb-rsp       to   fil-dcb-rsp            .
           move      rf-sdb-ncb-rsp       to   fil-ncb-rsp            .
           move      rf-sdb-spe-rsp       to   fil-spe-rsp            .
           move      rf-sdb-ens-rsp       to   fil-ens-rsp            .
           move      rf-sdb-nns-rsp       to   fil-nns-rsp            .
           move      rf-sdb-tns-rsp       to   fil-tns-rsp            .
           move      rf-sdb-dtr-acs       to   fil-dtr-acs            .
           move      rf-sdb-drc-acs       to   fil-drc-acs            .
           move      rf-sdb-npc-acs       to   fil-npc-acs            .
           move      rf-sdb-dcb-acs       to   fil-dcb-acs            .
           move      rf-sdb-ncb-acs       to   fil-ncb-acs            .
           move      rf-sdb-spe-acs       to   fil-spe-acs            .
           move      rf-sdb-dtr-nbe       to   fil-dtr-nbe            .
           move      rf-sdb-drc-nbe       to   fil-drc-nbe            .
           move      rf-sdb-npc-nbe       to   fil-npc-nbe            .
           move      rf-sdb-dcb-nbe       to   fil-dcb-nbe            .
           move      rf-sdb-ncb-nbe       to   fil-ncb-nbe            .
           move      rf-sdb-dtr-pbe       to   fil-dtr-pbe            .
           move      rf-sdb-drc-pbe       to   fil-drc-pbe            .
           move      rf-sdb-npc-pbe       to   fil-npc-pbe            .
           move      rf-sdb-dtr-isp       to   fil-dtr-isp            .
           move      rf-sdb-drc-isp       to   fil-drc-isp            .
           move      rf-sdb-npc-isp       to   fil-npc-isp            .
           move      rf-sdb-dcb-isp       to   fil-dcb-isp            .
           move      rf-sdb-ncb-isp       to   fil-ncb-isp            .
           move      rf-sdb-spe-isp       to   fil-spe-isp            .
           move      rf-sdb-ens-isp       to   fil-ens-isp            .
           move      rf-sdb-nns-isp       to   fil-nns-isp            .
           move      rf-sdb-tns-isp       to   fil-tns-isp            .
           move      rf-sdb-liv-slc       to   fil-liv-slc            .
           move      rf-sdb-dso-l01       to   fil-dso-l01            .
           move      rf-sdb-dso-l02       to   fil-dso-l02            .
           move      rf-sdb-dso-l03       to   fil-dso-l03            .
           move      rf-sdb-flc-stp       to   fil-flc-stp            .
           move      rf-sdb-flg-ela       to   fil-flg-ela            .
           move      rf-sdb-flg-pul       to   fil-flg-pul            .
           move      rf-sdb-snx-cts       to   fil-snx-cts            .
           move      rf-sdb-vet-cts       to   fil-vet-cts            .
           move      rf-sdb-alx-exp       to   fil-alx-exp            .
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
           move      rf-sdb-num-sdb       to   fil-num-sdb            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-sdb-ide-dat       to   fil-ide-dat            .
           move      rf-sdb-num-sdb       to   fil-num-sdb-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-sdb-dts-sdb       to   fil-dts-sdb            .
           move      rf-sdb-num-sdb       to   fil-num-sdb-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-sdb-tip-dbt       to   fil-tip-dbt            .
           move      rf-sdb-cod-dbt       to   fil-cod-dbt            .
           move      rf-sdb-dts-sdb       to   fil-dts-sdb-4          .
           move      rf-sdb-num-sdb       to   fil-num-sdb-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-sdb-num-ddp       to   fil-num-ddp            .
           move      rf-sdb-num-sdb       to   fil-num-sdb-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-sdb-num-ris       to   fil-num-ris            .
           move      rf-sdb-num-sdb       to   fil-num-sdb-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-sdb-tip-dbt       to   fil-tip-dbt-7          .
           move      rf-sdb-cod-dbt       to   fil-cod-dbt-7          .
           move      rf-sdb-num-ris       to   fil-num-ris-7          .
           move      rf-sdb-num-sdb       to   fil-num-sdb-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-800.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 8                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k08                .
           move      rf-sdb-prt-fcl       to   fil-prt-fcl            .
           move      rf-sdb-num-sdb       to   fil-num-sdb-8          .
       cmp-key-fis-900.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 9                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k09                .
           move      rf-sdb-dat-chs       to   fil-dat-chs            .
           move      rf-sdb-num-sdb       to   fil-num-sdb-9          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-sdb                 .
           move      fil-ide-dat          to   rf-sdb-ide-dat         .
           move      fil-ide-ute          to   rf-sdb-ide-ute         .
           move      fil-ide-fas          to   rf-sdb-ide-fas         .
           move      fil-dtr-emi          to   rf-sdb-dtr-emi         .
           move      fil-drc-emi          to   rf-sdb-drc-emi         .
           move      fil-npc-emi          to   rf-sdb-npc-emi         .
           move      fil-prt-fcl          to   rf-sdb-prt-fcl         .
           move      fil-nsc-org          to   rf-sdb-nsc-org         .
           move      fil-cod-age          to   rf-sdb-cod-age         .
           move      fil-num-sdb          to   rf-sdb-num-sdb         .
           move      fil-tip-sdb          to   rf-sdb-tip-sdb         .
           move      fil-snx-dlc          to   rf-sdb-snx-dlc         .
           move      fil-tac-sdb          to   rf-sdb-tac-sdb         .
           move      fil-tvl-sdb          to   rf-sdb-tvl-sdb         .
           move      fil-dts-sdb          to   rf-sdb-dts-sdb         .
           move      fil-tip-dbt          to   rf-sdb-tip-dbt         .
           move      fil-cod-dbt          to   rf-sdb-cod-dbt         .
           move      fil-dpz-dbt          to   rf-sdb-dpz-dbt         .
           move      fil-inl-sdb          to   rf-sdb-inl-sdb         .
           move      fil-sgl-vlt          to   rf-sdb-sgl-vlt         .
           move      fil-dec-vlt          to   rf-sdb-dec-vlt         .
           move      fil-tdc-vlt          to   rf-sdb-tdc-vlt         .
           move      fil-cdc-sdb          to   rf-sdb-cdc-sdb         .
           move      fil-iiv-sdb          to   rf-sdb-iiv-sdb         .
           move      fil-imp-sdb          to   rf-sdb-imp-sdb         .
           move      fil-dat-ddr          to   rf-sdb-dat-ddr         .
           move      fil-num-ddr          to   rf-sdb-num-ddr         .
           move      fil-tip-ddr          to   rf-sdb-tip-ddr         .
           move      fil-imp-ddr          to   rf-sdb-imp-ddr         .
           move      fil-nra-ddr          to   rf-sdb-nra-ddr         .
           move      fil-abi-app          to   rf-sdb-abi-app         .
           move      fil-cab-app          to   rf-sdb-cab-app         .
           move      fil-ccc-app          to   rf-sdb-ccc-app         .
           move      fil-cod-cbp          to   rf-sdb-cod-cbp         .
           move      fil-dtr-sto          to   rf-sdb-dtr-sto         .
           move      fil-drc-sto          to   rf-sdb-drc-sto         .
           move      fil-npc-sto          to   rf-sdb-npc-sto         .
           move      fil-ens-sto          to   rf-sdb-ens-sto         .
           move      fil-nns-sto          to   rf-sdb-nns-sto         .
           move      fil-tns-sto          to   rf-sdb-tns-sto         .
           move      fil-dtr-ris          to   rf-sdb-dtr-ris         .
           move      fil-mod-ris          to   rf-sdb-mod-ris         .
           move      fil-num-ris          to   rf-sdb-num-ris         .
           move      fil-imp-ris          to   rf-sdb-imp-ris         .
           move      fil-aos-ris          to   rf-sdb-aos-ris         .
           move      fil-nns-ris          to   rf-sdb-nns-ris         .
           move      fil-num-ddp          to   rf-sdb-num-ddp         .
           move      fil-tav-ott          to   rf-sdb-tav-ott         .
           move      fil-dtr-rsp          to   rf-sdb-dtr-rsp         .
           move      fil-drc-rsp          to   rf-sdb-drc-rsp         .
           move      fil-npc-rsp          to   rf-sdb-npc-rsp         .
           move      fil-dcb-rsp          to   rf-sdb-dcb-rsp         .
           move      fil-ncb-rsp          to   rf-sdb-ncb-rsp         .
           move      fil-spe-rsp          to   rf-sdb-spe-rsp         .
           move      fil-ens-rsp          to   rf-sdb-ens-rsp         .
           move      fil-nns-rsp          to   rf-sdb-nns-rsp         .
           move      fil-tns-rsp          to   rf-sdb-tns-rsp         .
           move      fil-dtr-acs          to   rf-sdb-dtr-acs         .
           move      fil-drc-acs          to   rf-sdb-drc-acs         .
           move      fil-npc-acs          to   rf-sdb-npc-acs         .
           move      fil-dcb-acs          to   rf-sdb-dcb-acs         .
           move      fil-ncb-acs          to   rf-sdb-ncb-acs         .
           move      fil-spe-acs          to   rf-sdb-spe-acs         .
           move      fil-dtr-nbe          to   rf-sdb-dtr-nbe         .
           move      fil-drc-nbe          to   rf-sdb-drc-nbe         .
           move      fil-npc-nbe          to   rf-sdb-npc-nbe         .
           move      fil-dcb-nbe          to   rf-sdb-dcb-nbe         .
           move      fil-ncb-nbe          to   rf-sdb-ncb-nbe         .
           move      fil-dtr-pbe          to   rf-sdb-dtr-pbe         .
           move      fil-drc-pbe          to   rf-sdb-drc-pbe         .
           move      fil-npc-pbe          to   rf-sdb-npc-pbe         .
           move      fil-dtr-isp          to   rf-sdb-dtr-isp         .
           move      fil-drc-isp          to   rf-sdb-drc-isp         .
           move      fil-npc-isp          to   rf-sdb-npc-isp         .
           move      fil-dcb-isp          to   rf-sdb-dcb-isp         .
           move      fil-ncb-isp          to   rf-sdb-ncb-isp         .
           move      fil-spe-isp          to   rf-sdb-spe-isp         .
           move      fil-ens-isp          to   rf-sdb-ens-isp         .
           move      fil-nns-isp          to   rf-sdb-nns-isp         .
           move      fil-tns-isp          to   rf-sdb-tns-isp         .
           move      fil-liv-slc          to   rf-sdb-liv-slc         .
           move      fil-dso-l01          to   rf-sdb-dso-l01         .
           move      fil-dso-l02          to   rf-sdb-dso-l02         .
           move      fil-dso-l03          to   rf-sdb-dso-l03         .
           move      fil-dat-chs          to   rf-sdb-dat-chs         .
           move      fil-flc-stp          to   rf-sdb-flc-stp         .
           move      fil-flg-ela          to   rf-sdb-flg-ela         .
           move      fil-flg-pul          to   rf-sdb-flg-pul         .
           move      fil-snx-cts          to   rf-sdb-snx-cts         .
           move      fil-vet-cts          to   rf-sdb-vet-cts         .
           move      fil-alx-exp          to   rf-sdb-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-sdb               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-sdb
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
