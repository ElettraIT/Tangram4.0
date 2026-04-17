       Identification Division.
       Program-Id.                                 eurstd01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    eur                 *
      *                                   Fase:    eurstd01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/00    *
      *                       Ultima revisione:    NdK del 07/09/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per EURO                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * {DCF}   [aaf] v  Anagrafica condizioni di acquisto fornitore   *
      * {DCF}   [aaq] v  Anagrafica condizioni generali di acquisto    *
      * {CGE}   [ali] v  Riepilogo movimenti per stampa allegati Iva   *
      * {BFO}   [bfr] v  Bolle fornitori, righe                        *
      * {BFO}   [bft] v  Bolle fornitori, testate                      *
      * {BOL}   [bir] v  Bolle clienti, righe                          *
      * {BOL}   [bit] v  Bolle clienti, testate                        *
      * {GEP}   [ccc] v  Anagrafica controllo crediti clienti - fido   *
      * {GEP}   [cec] v  Controllo esposizione clienti                 *
      * {DCP}   [dcp]    Anagrafica prodotti                           *
      * {GEP}   [ddp]    Distinte di presentazione effetti             *
      * {FFO}   [ffr]    Fatture fornitori, righe                      *
      * {FFO}   [fft]    Fatture fornitori, testate                    *
      * {FAT}   [fir]    Fatture a clienti, righe                      *
      * {FAT}   [fit]    Fatture a clienti, testate                    *
      * {AGE}   [gpc]    Conteggi provvigionali                        *
      * {AGE}   [gpm]    Maturazioni su conteggi provvigionali         *
      * {IIC}   [iir]    Movimenti per Iva intracomunitaria, righe     *
      * {IIC}   [iit]    Movimenti per Iva intracomunitaria, testate   *
      * {CGE}   [ivp]    Riepilogo movimenti stampa dichiarazione Iva  *
      * {DCF}   [lfd]    Prezzi di acquisto fornitori storicizzati     *
      * {DCP}   [lsd]    Prezzi di listino storicizzati per data       *
      * {DCP}   [lst]    Prezzi di listino di vendita                  *
      * {CGE}   [mgr]    Primanota, righe                              *
      * {CGE}   [mgs]    Progressivi contabili                         *
      * {CGE}   [mgt]    Primanota, testate                            *
      * {MAG}   [mmr]    Movimenti per la gestione magazzino, righe    *
      * {MAG}   [mmt]    Movimenti per la gestione magazzino, testate  *
      * {MAG}   [mmv]    Saldi valore per gestione magazzino           *
      * {MAG}   [moi]    Primanota, estensione per registrazioni Iva   *
      * {ORC}   [ocr]    Ordini clienti, righe                         *
      * {ORC}   [oct]    Ordini clienti, testate                       *
      * {ORF}   [ofr]    Ordini fornitori, righe                       *
      * {ORF}   [oft]    Ordini fornitori, testate                     *
      * {ODS}   [osr]    Ordini di spedizione clienti, righe           *
      * {ODS}   [ost]    Ordini di spedizione clienti, testate         *
      * {CGE}   [rdb]    Movimenti di rettifica bilanci                *
      * {GEP}   [rsd]    Riscossioni scadenze debitori                 *
      * {GEP}   [sdb]    Scadenze in portafoglio                       *
      * {SCF}   [sfa]    Addebiti ordini pagamento scadenze fornitori  *
      * {SCF}   [sff]    Fatture fornitori                             *
      * {SCF}   [sfp]    Pagamenti fornitori                           *
      * {SCF}   [sfs]    Scadenze fornitori                            *
      * {BIL}   [vbv]    Voci di bilancio, valori                      *
      * {DCF}   [ybo]    Tabella addebito spese bollo fornitori        *
      * {DCF}   [yin]    Tabella addebito spese incasso fornitori      *
      * {DCF}   [ysf]    Tabella definizione voci di spese fornitori   *
      * {FAT}   [zac]    Tabella descrizioni addebiti o commenti       *
      * {DCC}   [zbo]    Tabella addebito spese bollo clienti          *
      * {DCC}   [zin]    Tabella addebito spese incasso clienti        *
      * {GEP}   [zsb]    Tabella calcolo aritmetico importo bolli      *
      * {DCC}   [zsf]    Tabella definizione voci spese fatturazione   *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      * Note : 1) Per i listini fornitori non si considerano gli sca-  *
      *           glioni di prezzo per quantita'                       *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.        N-d-K-Sia-PD .
       Object-Computer.        N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [aaf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  aaf   assign to disk           f-aaf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is aaf-k01
                   alternate record key   is aaf-k02
                   alternate record key   is aaf-k03
                   alternate record key   is aaf-k04
                   alternate record key   is aaf-k05
                   alternate record key   is aaf-k06
                             file status  is                f-aaf-sts .

      *    *===========================================================*
      *    * File Control [aaq]                                        *
      *    *-----------------------------------------------------------*
           select  optional  aaq   assign to disk           f-aaq-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is aaq-k01
                   alternate record key   is aaq-k02
                   alternate record key   is aaq-k03
                   alternate record key   is aaq-k04
                             file status  is                f-aaq-sts .

      *    *===========================================================*
      *    * File Control [ali]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ali   assign to disk           f-ali-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ali-k01
                             file status  is                f-ali-sts .

      *    *===========================================================*
      *    * File Control [bfr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bfr   assign to disk           f-bfr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bfr-k01
                   alternate record key   is bfr-k02
                   alternate record key   is bfr-k03
                   alternate record key   is bfr-k04
                   alternate record key   is bfr-k05
                             file status  is                f-bfr-sts .

      *    *===========================================================*
      *    * File Control [bft]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bft   assign to disk           f-bft-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bft-k01
                   alternate record key   is bft-k02
                   alternate record key   is bft-k03
                   alternate record key   is bft-k04
                   alternate record key   is bft-k05
                   alternate record key   is bft-k06
                             file status  is                f-bft-sts .

      *    *===========================================================*
      *    * File Control [bir]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bir   assign to disk           f-bir-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bir-k01
                   alternate record key   is bir-k02
                   alternate record key   is bir-k03
                   alternate record key   is bir-k04
                             file status  is                f-bir-sts .

      *    *===========================================================*
      *    * File Control [bit]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bit   assign to disk           f-bit-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bit-k01
                   alternate record key   is bit-k02
                   alternate record key   is bit-k03
                   alternate record key   is bit-k04
                   alternate record key   is bit-k05
                   alternate record key   is bit-k06
                   alternate record key   is bit-k07
                             file status  is                f-bit-sts .

      *    *===========================================================*
      *    * File Control [ccc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ccc   assign to disk           f-ccc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ccc-k01
                   alternate record key   is ccc-k02
                             file status  is                f-ccc-sts .

      *    *===========================================================*
      *    * File Control [cec]                                        *
      *    *-----------------------------------------------------------*
           select  optional  cec   assign to disk           f-cec-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is cec-k01
                             file status  is                f-cec-sts .

      *    *===========================================================*
      *    * File Control [dcp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  dcp   assign to disk           f-dcp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is dcp-k01
                   alternate record key   is dcp-k02
                   alternate record key   is dcp-k03
                   alternate record key   is dcp-k04
                   alternate record key   is dcp-k05
                   alternate record key   is dcp-k06
                   alternate record key   is dcp-k07
                   alternate record key   is dcp-k08
                             file status  is                f-dcp-sts .

      *    *===========================================================*
      *    * File Control [lsd]                                        *
      *    *-----------------------------------------------------------*
           select  optional  lsd   assign to disk           f-lsd-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is lsd-k01
                   alternate record key   is lsd-k02
                             file status  is                f-lsd-sts .

      *    *===========================================================*
      *    * File Control [lst]                                        *
      *    *-----------------------------------------------------------*
           select  optional  lst   assign to disk           f-lst-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is lst-k01
                   alternate record key   is lst-k02
                             file status  is                f-lst-sts .






      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [aaf]                                    *
      *    *-----------------------------------------------------------*
       fd  aaf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  aaf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  aaf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRFNFM                         *
      *            *---------------------------------------------------*
               10  aaf-k01.
                   15  aaf-tip-mag        pic  9(02)                  .
                   15  aaf-num-pro        pic  9(07)       comp-3     .
                   15  aaf-cod-dcf        pic  9(07)       comp-3     .
                   15  aaf-fda-pif        pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  aaf-k02.
                   15  aaf-ide-dat        pic  9(07)       comp-3     .
                   15  aaf-tip-mag-2      pic  9(02)                  .
                   15  aaf-num-pro-2      pic  9(07)       comp-3     .
                   15  aaf-cod-dcf-2      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-2      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNPRFM                         *
      *            *---------------------------------------------------*
               10  aaf-k03.
                   15  aaf-cod-dcf-3      pic  9(07)       comp-3     .
                   15  aaf-tip-mag-3      pic  9(02)                  .
                   15  aaf-num-pro-3      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-3      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PRFMFN                         *
      *            *---------------------------------------------------*
               10  aaf-k04.
                   15  aaf-tip-mag-4      pic  9(02)                  .
                   15  aaf-num-pro-4      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-4      pic  x(14)                  .
                   15  aaf-cod-dcf-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DCFCOPSFN                      *
      *            *---------------------------------------------------*
               10  aaf-k05.
                   15  aaf-cod-dcf-5      pic  9(07)       comp-3     .
                   15  aaf-cop-sfn        pic  x(14)                  .
                   15  aaf-tip-mag-5      pic  9(02)                  .
                   15  aaf-num-pro-5      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-5      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : COPSFN                         *
      *            *---------------------------------------------------*
               10  aaf-k06.
                   15  aaf-cop-sfn-6      pic  x(14)                  .
                   15  aaf-tip-mag-6      pic  9(02)                  .
                   15  aaf-cod-dcf-6      pic  9(07)       comp-3     .
                   15  aaf-num-pro-6      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-6      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  aaf-dat.
               10  aaf-ide-ute            pic  x(08)                  .
               10  aaf-ide-fas            pic  x(06)                  .
               10  aaf-dep-sfn            pic  x(40)                  .
               10  aaf-xdp-sfn            pic  9(01)                  .
               10  aaf-snx-tum            pic  x(01)                  .
               10  aaf-umf-tum            pic  x(03)                  .
               10  aaf-nde-tum            pic  9(01)                  .
               10  aaf-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  aaf-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  aaf-dpz-dcf            pic  x(04)                  .
               10  aaf-ann-not            pic  x(40)                  .
               10  aaf-tmp-cns            pic  9(03)       comp-3     .
               10  aaf-sgl-vlt            pic  x(03)                  .
               10  aaf-dec-vlt            pic  9(01)                  .
               10  aaf-dec-prz            pic  9(01)                  .
               10  aaf-tip-pza            pic  9(02)                  .
               10  aaf-lot-acq            pic  9(06)v9(03) comp-3     .
               10  aaf-tap-pes            pic  9(02)                  .
               10  aaf-uda-pes            pic  9(07)       comp-3     .
               10  aaf-lgv-vlt            pic  x(03)                  .
               10  aaf-lgv-dcv            pic  9(01)                  .
               10  aaf-lgv-tdc            pic  x(01)                  .
               10  aaf-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  aaf-lgv-pdt            pic  9(01)v9(02) comp-3     .
               10  aaf-tbl-pes.
                   15  aaf-ele-pes occurs 06.
                       20  aaf-qta-pes    pic  9(06)v9(03) comp-3     .
                       20  aaf-prz-pes    pic  9(09)       comp-3     .
                       20  aaf-csr-pes    pic  9(05)       comp-3     .
                       20  aaf-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  aaf-per-mpa            pic  9(02)v9(01)            .
               10  aaf-alx-exp.
                   15  filler  occurs 17  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [aaq]                                    *
      *    *-----------------------------------------------------------*
       fd  aaq       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  aaq-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  aaq-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  aaq-k01.
                   15  aaq-tip-mag        pic  9(02)                  .
                   15  aaq-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  aaq-k02.
                   15  aaq-ide-dat        pic  9(07)       comp-3     .
                   15  aaq-tip-mag-2      pic  9(02)                  .
                   15  aaq-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CDPPDT                         *
      *            *---------------------------------------------------*
               10  aaq-k03.
                   15  aaq-cdp-pdt        pic  x(14)                  .
                   15  aaq-tip-mag-3      pic  9(02)                  .
                   15  aaq-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PDTCDP                         *
      *            *---------------------------------------------------*
               10  aaq-k04.
                   15  aaq-cod-pdt        pic  9(07)       comp-3     .
                   15  aaq-cdp-pdt-4      pic  x(14)                  .
                   15  aaq-tip-mag-4      pic  9(02)                  .
                   15  aaq-num-pro-4      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  aaq-dat.
               10  aaq-ide-ute            pic  x(08)                  .
               10  aaq-ide-fas            pic  x(06)                  .
               10  aaq-cod-iva            pic  9(05)       comp-3     .
               10  aaq-ctp-acq            pic  9(07)       comp-3     .
               10  aaq-dcf-pfz            pic  9(07)       comp-3     .
               10  aaq-dpz-pfz            pic  x(04)                  .
               10  aaq-sgl-vlt            pic  x(03)                  .
               10  aaq-dec-vlt            pic  9(01)                  .
               10  aaq-dec-prz            pic  9(01)                  .
               10  aaq-prz-acr            pic  9(09)       comp-3     .
               10  aaq-uda-par            pic  9(07)       comp-3     .
               10  aaq-tmp-apv            pic  9(03)       comp-3     .
               10  aaq-epz-rgf            pic  9(01)                  .
               10  aaq-snx-2qt            pic  9(01)                  .
               10  aaq-dec-2qt            pic  9(01)                  .
               10  aaq-snx-3qt            pic  9(01)                  .
               10  aaq-dec-3qt            pic  9(01)                  .
               10  aaq-snx-2pz            pic  9(01)                  .
               10  aaq-dec-2pz            pic  9(01)                  .
               10  aaq-aut-lst            pic  x(03)                  .
               10  aaq-tip-vac            pic  x(03)                  .
               10  aaq-cdp-aqt            pic  9(05)       comp-3     .
               10  aaq-pdp-aqt  occurs 03
                                          pic  9(02)v9(01)            .
               10  aaq-lot-acq            pic  9(06)v9(03) comp-3     .
               10  aaq-cla-bdg            pic  9(05)       comp-3     .
               10  aaq-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ali]                                    *
      *    *-----------------------------------------------------------*
       fd  ali       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ali-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ali-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : "ALIARCIVA "                   *
      *            *---------------------------------------------------*
               10  ali-k01.
                   15  ali-ann-fis        pic  9(03)       comp-3     .
                   15  ali-tip-arc        pic  x(01)                  .
                   15  ali-cod-arc        pic  9(07)       comp-3     .
                   15  ali-cod-iva        pic  9(05)       comp-3     .
                   15  ali-ann-cop        pic  x(01)                  .

      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ali-dat.
               10  ali-num-doc            pic s9(05)       comp-3     .
               10  ali-num-dog            pic s9(05)       comp-3     .
               10  ali-ibl-dog            pic s9(11)       comp-3     .
               10  ali-iva-dog            pic s9(11)       comp-3     .
               10  ali-ibl-doc            pic s9(11)       comp-3     .
               10  ali-iva-doc            pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * File Description [bfr]                                    *
      *    *-----------------------------------------------------------*
       fd  bfr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bfr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bfr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bfr-k01.
                   15  bfr-num-prt        pic  9(11)       comp-3     .
                   15  bfr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  bfr-k02.
                   15  bfr-cod-dpz        pic  9(02)                  .
                   15  bfr-tip-mag        pic  9(02)                  .
                   15  bfr-num-mag        pic  9(07)       comp-3     .
                   15  bfr-dat-reg        pic  9(07)       comp-3     .
                   15  bfr-num-prt-2      pic  9(11)       comp-3     .
                   15  bfr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFORF                         *
      *            *---------------------------------------------------*
               10  bfr-k03.
                   15  bfr-cod-dpz-3      pic  9(02)                  .
                   15  bfr-orf-prt        pic  9(11)       comp-3     .
                   15  bfr-orf-prg        pic  9(05)       comp-3     .
                   15  bfr-num-prt-3      pic  9(11)       comp-3     .
                   15  bfr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  bfr-k04.
                   15  bfr-cod-dpz-4      pic  9(02)                  .
                   15  bfr-flg-rch        pic  x(01)                  .
                   15  bfr-tip-mag-4      pic  9(02)                  .
                   15  bfr-num-mag-4      pic  9(07)       comp-3     .
                   15  bfr-num-prt-4      pic  9(11)       comp-3     .
                   15  bfr-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : RCHARC                         *
      *            *---------------------------------------------------*
               10  bfr-k05.
                   15  bfr-cod-dpz-5      pic  9(02)                  .
                   15  bfr-flg-rch-5      pic  x(01)                  .
                   15  bfr-tip-arc        pic  x(01)                  .
                   15  bfr-cod-arc        pic  9(07)       comp-3     .
                   15  bfr-num-prt-5      pic  9(11)       comp-3     .
                   15  bfr-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bfr-dat.
               10  bfr-cod-tmb            pic  x(05)                  .
               10  bfr-dpz-arc            pic  x(04)                  .
               10  bfr-dat-doc            pic  9(07)                  .
               10  bfr-num-doc            pic  x(10)                  .
               10  bfr-cod-lng            pic  x(03)                  .
               10  bfr-vpf.
                   15  bfr-sgl-vpf        pic  x(03)                  .
                   15  bfr-dec-vpf        pic  9(01)                  .
                   15  bfr-tdc-vpf        pic  x(01)                  .
                   15  bfr-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bfr-bld-flb            pic  9(01)                  .
               10  bfr-bld-tpb            pic  9(01)                  .
               10  bfr-bld-rgb            pic  9(01)                  .
               10  bfr-tip-rig            pic  x(05)                  .
               10  bfr-alf-mag            pic  x(14)                  .
               10  bfr-sgl-vrn            pic  x(14)                  .
               10  bfr-fda-pif            pic  x(14)                  .
               10  bfr-cop-sfn            pic  x(14)                  .
               10  bfr-snx-tum            pic  x(01)                  .
               10  bfr-umf-tum            pic  x(03)                  .
               10  bfr-nde-tum            pic  9(01)                  .
               10  bfr-cmo-tum            pic  9(06)v9(03)            .
               10  bfr-cdi-tum            pic  9(06)v9(03)            .
               10  bfr-des-ext            pic  9(01)                  .
               10  bfr-des-rig            pic  x(40)                  .
               10  bfr-tip-pro            pic  9(02)                  .
               10  bfr-cod-iva            pic  9(05)       comp-3     .
               10  bfr-ctp-acq            pic  9(07)       comp-3     .
               10  bfr-umi-acq            pic  x(03)                  .
               10  bfr-dec-qta            pic  9(01)                  .
               10  bfr-qta-fda            pic s9(06)v9(03) comp-3     .
               10  bfr-qta-acq            pic s9(06)v9(03) comp-3     .
               10  bfr-cod-dsl            pic  x(07)                  .
               10  bfr-snx-2qt            pic  9(01)                  .
               10  bfr-dec-2qt            pic  9(01)                  .
               10  bfr-qta-a02            pic s9(06)v9(03) comp-3     .
               10  bfr-snx-3qt            pic  9(01)                  .
               10  bfr-dec-3qt            pic  9(01)                  .
               10  bfr-qta-a03            pic s9(06)v9(03) comp-3     .
               10  bfr-dec-prz            pic  9(01)                  .
               10  bfr-vpp.
                   15  bfr-sgl-vpp        pic  x(03)                  .
                   15  bfr-dec-vpp        pic  9(01)                  .
                   15  bfr-tdc-vpp        pic  x(01)                  .
                   15  bfr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  bfr-prz-acq            pic  9(09)       comp-3     .
               10  bfr-snx-2pz            pic  9(01)                  .
               10  bfr-dec-2pz            pic  9(01)                  .
               10  bfr-prz-a02            pic  9(09)       comp-3     .
               10  bfr-vpl.
                   15  bfr-sgl-vpl        pic  x(03)                  .
                   15  bfr-dec-vpl        pic  9(01)                  .
                   15  bfr-tdc-vpl        pic  x(01)                  .
                   15  bfr-prz-vpl        pic  9(09)       comp-3     .
                   15  bfr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  bfr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  bfr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  bfr-tlm-vpl        pic  x(01)                  .
                   15  bfr-map-vpl        pic  x(01)                  .
               10  bfr-epz-rgf            pic  9(01)                  .
               10  bfr-csr-aap            pic  9(05)       comp-3     .
               10  bfr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  bfr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  bfr-prz-net            pic  9(09)       comp-3     .
               10  bfr-imp-rig            pic s9(11)       comp-3     .
               10  bfr-iau-rig            pic s9(11)       comp-3     .
               10  bfr-cpv-aap            pic  9(05)       comp-3     .
               10  bfr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  bfr-fsp-rig            pic  9(02)                  .
               10  bfr-cpv-rig            pic  9(05)       comp-3     .
               10  bfr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  bfr-pvf-rig            pic s9(11)       comp-3     .
               10  bfr-rdo-tip            pic  x(05)                  .
               10  bfr-rdo-dat            pic  9(07)       comp-3     .
               10  bfr-rdo-num            pic  9(11)       comp-3     .
               10  bfr-orf-tip            pic  x(05)                  .
               10  bfr-orf-dat            pic  9(07)       comp-3     .
               10  bfr-orf-num            pic  9(11)       comp-3     .
               10  bfr-orf-fzs            pic  x(01)                  .
               10  bfr-orm-prt            pic  9(11)       comp-3     .
               10  bfr-flg-ela.
                   15  bfr-flg-blo.
                       20  bfr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bfr-flg-nbl.
                       20  bfr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bfr-flg-pul            pic  x(01)                  .
               10  bfr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [bft]                                    *
      *    *-----------------------------------------------------------*
       fd  bft       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bft-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bft-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bft-k01.
                   15  bft-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  bft-k02.
                   15  bft-ide-dat        pic  9(07)       comp-3     .
                   15  bft-dat-reg        pic  9(07)       comp-3     .
                   15  bft-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  bft-k03.
                   15  bft-dat-reg-3      pic  9(07)       comp-3     .
                   15  bft-cod-dpz        pic  9(02)                  .
                   15  bft-cod-tmb        pic  x(05)                  .
                   15  bft-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  bft-k04.
                   15  bft-cod-dpz-4      pic  9(02)                  .
                   15  bft-tip-arc        pic  x(01)                  .
                   15  bft-cod-arc        pic  9(07)       comp-3     .
                   15  bft-dat-reg-4      pic  9(07)       comp-3     .
                   15  bft-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  bft-k05.
                   15  bft-cod-dpz-5      pic  9(02)                  .
                   15  bft-cod-tmb-5      pic  x(05)                  .
                   15  bft-dat-reg-5      pic  9(07)       comp-3     .
                   15  bft-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZBCH                         *
      *            *---------------------------------------------------*
               10  bft-k06.
                   15  bft-cod-dpz-6      pic  9(02)                  .
                   15  bft-flg-bch        pic  x(01)                  .
                   15  bft-num-prt-6      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bft-dat.
               10  bft-ide-ute            pic  x(08)                  .
               10  bft-ide-fas            pic  x(06)                  .
               10  bft-prt-ftf            pic  9(11)       comp-3     .
               10  bft-prt-mag            pic  9(11)       comp-3     .
               10  bft-pr2-mag            pic  9(11)       comp-3     .
               10  bft-int-ftr            pic  9(02)                  .
               10  bft-tmo-ftr            pic  x(05)                  .
               10  bft-dpz-arc            pic  x(04)                  .
               10  bft-arc-plf            pic  9(07)       comp-3     .
               10  bft-dpz-plf            pic  x(04)                  .
               10  bft-tip-ids            pic  9(02)                  .
               10  bft-dat-doc            pic  9(07)       comp-3     .
               10  bft-num-doc            pic  x(10)                  .
               10  bft-cod-lng            pic  x(03)                  .
               10  bft-vpf.
                   15  bft-sgl-vpf        pic  x(03)                  .
                   15  bft-dec-vpf        pic  9(01)                  .
                   15  bft-tdc-vpf        pic  x(01)                  .
                   15  bft-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bft-ass-iva            pic  9(05)       comp-3     .
               10  bft-ctp-acq            pic  9(07)       comp-3     .
               10  bft-inl-pgt            pic  9(02)                  .
               10  bft-cod-lst            pic  x(03)                  .
               10  bft-csr-aaf            pic  9(05)       comp-3     .
               10  bft-psr-aaf occurs 05  pic  9(02)v9(01) comp-3     .
               10  bft-csc-aaf            pic  9(05)       comp-3     .
               10  bft-psc-aaf            pic  9(02)v9(01) comp-3     .
               10  bft-voc-des occurs 06  pic  x(03)                  .
               10  bft-cod-fop            pic  9(07)       comp-3     .
               10  bft-scp-aap            pic  9(02)v9(01) comp-3     .
               10  bft-nos-ban            pic  x(10)                  .
               10  bft-cod-abi            pic  9(05)       comp-3     .
               10  bft-cod-cab            pic  9(05)       comp-3     .
               10  bft-ccc-app            pic  x(12)                  .
               10  bft-ccp-app            pic  x(12)                  .
               10  bft-add-spi            pic  x(03)                  .
               10  bft-add-spb            pic  x(03)                  .
               10  bft-ipr-iel            pic  9(02)                  .
               10  bft-pag-dsm            pic  9(07)       comp-3     .
               10  bft-pag-qaf            pic  9(09)       comp-3     .
               10  bft-pag-act            pic  9(09)       comp-3     .
               10  bft-cod-aqt            pic  9(07)       comp-3     .
               10  bft-pvf-aqt            pic  9(11)       comp-3     .
               10  bft-cod-ime            pic  9(07)       comp-3     .
               10  bft-pvf-ime            pic  9(11)       comp-3     .
               10  bft-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  bft-tot-scc            pic s9(11)       comp-3     .
               10  bft-per-scc            pic  9(02)v9(01) comp-3     .
               10  bft-civ-scc            pic  9(05)       comp-3     .
               10  bft-ccp-scc            pic  9(07)       comp-3     .
               10  bft-tot-scp            pic s9(11)       comp-3     .
               10  bft-per-scp            pic  9(02)v9(01) comp-3     .
               10  bft-civ-scp            pic  9(05)       comp-3     .
               10  bft-ccp-scp            pic  9(07)       comp-3     .
               10  bft-spe-add occurs 06.
                   15  bft-spe-snx        pic  9(01)                  .
                   15  bft-spe-mad        pic  9(01)                  .
                   15  bft-spe-per        pic  9(02)v9(01) comp-3     .
                   15  bft-spe-ibl        pic  9(02)                  .
                   15  bft-ibt-spe.
                       20  bft-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  bft-spe-imp        pic s9(09)       comp-3     .
                   15  bft-spe-civ        pic  9(05)       comp-3     .
                   15  bft-spe-ccp        pic  9(07)       comp-3     .
               10  bft-civ-spi            pic  9(05)       comp-3     .
               10  bft-ccp-spi            pic  9(07)       comp-3     .
               10  bft-tot-sic            pic s9(09)       comp-3     .
               10  bft-tot-sia            pic s9(09)       comp-3     .
               10  bft-tot-spb            pic s9(09)       comp-3     .
               10  bft-civ-spb            pic  9(05)       comp-3     .
               10  bft-ccp-spb            pic  9(07)       comp-3     .
               10  bft-prt-mgd            pic  9(07)       comp-3     .
               10  bft-nrg-mgd            pic  9(02)                  .
               10  bft-dri-mgd            pic  9(07)       comp-3     .
               10  bft-nri-mgd            pic  x(10)                  .
               10  bft-nps-sdb            pic  9(11)                  .
               10  bft-ctr-sdb            pic  9(02)                  .
               10  bft-iva-cst.
                   15  bft-iva-rig occurs 06.
                       20  bft-iva-cod    pic  9(05)       comp-3     .
                       20  bft-iva-ibl    pic s9(11)       comp-3     .
               10  bft-ctp-cst.
                   15  bft-ctp-rig occurs 10.
                       20  bft-ctp-cod    pic  9(07)       comp-3     .
                       20  bft-ctp-imp    pic s9(11)       comp-3     .
               10  bft-flg-ela.
                   15  bft-flg-blo.
                       20  bft-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bft-flg-nbl.
                       20  bft-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bft-flg-pul            pic  x(01)                  .
               10  bft-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [bir]                                    *
      *    *-----------------------------------------------------------*
       fd  bir       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bir-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bir-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bir-k01.
                   15  bir-num-prt        pic  9(11)       comp-3     .
                   15  bir-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  bir-k02.
                   15  bir-cod-dpz        pic  9(02)                  .
                   15  bir-tip-mag        pic  9(02)                  .
                   15  bir-num-pro        pic  9(07)       comp-3     .
                   15  bir-dat-doc        pic  9(07)       comp-3     .
                   15  bir-num-doc        pic  9(11)       comp-3     .
                   15  bir-num-prt-2      pic  9(11)       comp-3     .
                   15  bir-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFORC                         *
      *            *---------------------------------------------------*
               10  bir-k03.
                   15  bir-cod-dpz-3      pic  9(02)                  .
                   15  bir-coc-prt        pic  9(11)       comp-3     .
                   15  bir-coc-prg        pic  9(05)       comp-3     .
                   15  bir-num-prt-3      pic  9(11)       comp-3     .
                   15  bir-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RIFODS                         *
      *            *---------------------------------------------------*
               10  bir-k04.
                   15  bir-cod-dpz-4      pic  9(02)                  .
                   15  bir-ods-prt        pic  9(11)       comp-3     .
                   15  bir-ods-prg        pic  9(05)       comp-3     .
                   15  bir-coc-prt-4      pic  9(11)       comp-3     .
                   15  bir-coc-prg-4      pic  9(05)       comp-3     .
                   15  bir-num-prt-4      pic  9(11)       comp-3     .
                   15  bir-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bir-dat.
               10  bir-cod-tmb            pic  x(05)                  .
               10  bir-tip-arc            pic  x(01)                  .
               10  bir-cod-arc            pic  9(07)       comp-3     .
               10  bir-dpz-arc            pic  x(04)                  .
               10  bir-cod-lng            pic  x(03)                  .
               10  bir-vpf.
                   15  bir-sgl-vpf        pic  x(03)                  .
                   15  bir-dec-vpf        pic  9(01)                  .
                   15  bir-tdc-vpf        pic  x(01)                  .
                   15  bir-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bir-bld-flb            pic  9(01)                  .
               10  bir-bld-tpb            pic  9(01)                  .
               10  bir-bld-rgb            pic  9(01)                  .
               10  bir-tip-rig            pic  x(05)                  .
               10  bir-alf-pro            pic  x(14)                  .
               10  bir-sgl-vrn            pic  x(14)                  .
               10  bir-cop-scl            pic  x(14)                  .
               10  bir-des-ext            pic  9(01)                  .
               10  bir-des-rig            pic  x(40)                  .
               10  bir-tip-pro            pic  9(02)                  .
               10  bir-cod-iva            pic  9(05)       comp-3     .
               10  bir-ctp-ven            pic  9(07)       comp-3     .
               10  bir-umi-ven            pic  x(03)                  .
               10  bir-dec-qta            pic  9(01)                  .
               10  bir-qta-ven            pic s9(06)v9(03) comp-3     .
               10  bir-cod-dsl            pic  x(07)                  .
               10  bir-snx-2qt            pic  9(01)                  .
               10  bir-dec-2qt            pic  9(01)                  .
               10  bir-qta-a02            pic s9(06)v9(03) comp-3     .
               10  bir-snx-3qt            pic  9(01)                  .
               10  bir-dec-3qt            pic  9(01)                  .
               10  bir-qta-a03            pic s9(06)v9(03) comp-3     .
               10  bir-dec-prz            pic  9(01)                  .
               10  bir-vps.
                   15  bir-sgl-vps        pic  x(03)                  .
                   15  bir-dec-vps        pic  9(01)                  .
                   15  bir-tdc-vps        pic  x(01)                  .
                   15  bir-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  bir-prz-lrs            pic  9(09)       comp-3     .
               10  bir-prz-nts            pic  9(09)       comp-3     .
               10  bir-vpp.
                   15  bir-sgl-vpp        pic  x(03)                  .
                   15  bir-dec-vpp        pic  9(01)                  .
                   15  bir-tdc-vpp        pic  x(01)                  .
                   15  bir-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  bir-prz-ven            pic  9(09)       comp-3     .
               10  bir-snx-2pz            pic  9(01)                  .
               10  bir-prz-a02            pic  9(09)       comp-3     .
               10  bir-vpl.
                   15  bir-sgl-vpl        pic  x(03)                  .
                   15  bir-dec-vpl        pic  9(01)                  .
                   15  bir-tdc-vpl        pic  x(01)                  .
                   15  bir-prz-vpl        pic  9(09)       comp-3     .
                   15  bir-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  bir-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  bir-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  bir-tlm-vpl        pic  x(01)                  .
                   15  bir-map-vpl        pic  x(01)                  .
               10  bir-epz-rgf            pic  9(01)                  .
               10  bir-csr-aap            pic  9(05)       comp-3     .
               10  bir-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  bir-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  bir-prz-net            pic  9(09)       comp-3     .
               10  bir-epz-pes            pic  9(02)                  .
               10  bir-vpc.
                   15  bir-sgl-vpc        pic  x(03)                  .
                   15  bir-dec-vpc        pic  9(01)                  .
                   15  bir-tdc-vpc        pic  x(01)                  .
                   15  bir-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  bir-dec-cos            pic  9(01)                  .
               10  bir-cos-rif            pic  9(09)       comp-3     .
               10  bir-imp-rig            pic s9(11)       comp-3     .
               10  bir-iau-rig            pic s9(11)       comp-3     .
               10  bir-cpv-aap            pic  9(05)       comp-3     .
               10  bir-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  bir-fsp-rig            pic  9(02)                  .
               10  bir-cpv-rig            pic  9(05)       comp-3     .
               10  bir-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  bir-pvf-rig            pic s9(11)       comp-3     .
               10  bir-ocl-dat            pic  9(07)       comp-3     .
               10  bir-ocl-num            pic  x(10)                  .
               10  bir-cmc-tip            pic  x(05)                  .
               10  bir-cmc-dat            pic  9(07)       comp-3     .
               10  bir-cmc-num            pic  9(11)       comp-3     .
               10  bir-coc-tip            pic  x(05)                  .
               10  bir-coc-dat            pic  9(07)       comp-3     .
               10  bir-coc-num            pic  9(11)       comp-3     .
               10  bir-coc-fzs            pic  x(01)                  .
               10  bir-fat-snx            pic  x(01)                  .
               10  bir-fat-dat            pic  9(07)       comp-3     .
               10  bir-fat-num            pic  9(11)       comp-3     .
               10  bir-fat-npb            pic  9(03)       comp-3     .
               10  bir-flg-ela.
                   15  bir-flg-blo.
                       20  bir-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bir-flg-nbl.
                       20  bir-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bir-flg-pul            pic  x(01)                  .
               10  bir-flg-puq            pic  x(01)                  .
               10  bir-alx-exp.
                   15  filler  occurs 19  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [bit]                                    *
      *    *-----------------------------------------------------------*
       fd  bit       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bit-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bit-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bit-k01.
                   15  bit-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  bit-k02.
                   15  bit-ide-dat        pic  9(07)       comp-3     .
                   15  bit-dat-doc        pic  9(07)       comp-3     .
                   15  bit-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  bit-k03.
                   15  bit-dat-doc-3      pic  9(07)       comp-3     .
                   15  bit-cod-dpz        pic  9(02)                  .
                   15  bit-num-doc        pic  9(11)       comp-3     .
                   15  bit-cod-tmb        pic  x(05)                  .
                   15  bit-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  bit-k04.
                   15  bit-snx-fac        pic  x(01)                  .
                   15  bit-scl-ann        pic  9(03)       comp-3     .
                   15  bit-cod-dpz-4      pic  9(02)                  .
                   15  bit-sgl-num        pic  x(03)                  .
                   15  bit-num-doc-4      pic  9(11)       comp-3     .
                   15  bit-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  bit-k05.
                   15  bit-cod-dpz-5      pic  9(02)                  .
                   15  bit-tip-arc        pic  x(01)                  .
                   15  bit-cod-arc        pic  9(07)       comp-3     .
                   15  bit-dat-doc-5      pic  9(07)       comp-3     .
                   15  bit-num-doc-5      pic  9(11)       comp-3     .
                   15  bit-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  bit-k06.
                   15  bit-cod-dpz-6      pic  9(02)                  .
                   15  bit-cod-tmb-6      pic  x(05)                  .
                   15  bit-dat-doc-6      pic  9(07)       comp-3     .
                   15  bit-num-doc-6      pic  9(11)       comp-3     .
                   15  bit-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : FATARC                         *
      *            *---------------------------------------------------*
               10  bit-k07.
                   15  bit-fat-snx        pic  x(01)                  .
                   15  bit-fat-dat        pic  9(07)       comp-3     .
                   15  bit-fat-num        pic  9(11)       comp-3     .
                   15  bit-fat-npb        pic  9(03)       comp-3     .
                   15  bit-tip-arc-7      pic  x(01)                  .
                   15  bit-arc-plf-7      pic  9(07)       comp-3     .
                   15  bit-num-prt-7      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bit-dat.
               10  bit-ide-ute            pic  x(08)                  .
               10  bit-ide-fas            pic  x(06)                  .
               10  bit-prt-fti            pic  9(11)       comp-3     .
               10  bit-prt-mag            pic  9(11)       comp-3     .
               10  bit-pr2-mag            pic  9(11)       comp-3     .
               10  bit-int-ftr            pic  9(02)                  .
               10  bit-tmo-ftr            pic  x(05)                  .
               10  bit-snx-acm            pic  x(01)                  .
               10  bit-dpz-arc            pic  x(04)                  .
               10  bit-tip-frn            pic  9(02)                  .
               10  bit-arc-plf            pic  9(07)       comp-3     .
               10  bit-dpz-plf            pic  x(04)                  .
               10  bit-tip-ftz            pic  9(02)                  .
               10  bit-tip-ids            pic  9(02)                  .
               10  bit-cod-lng            pic  x(03)                  .
               10  bit-vpf.
                   15  bit-sgl-vpf        pic  x(03)                  .
                   15  bit-dec-vpf        pic  9(01)                  .
                   15  bit-tdc-vpf        pic  x(01)                  .
                   15  bit-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bit-ass-iva            pic  9(05)       comp-3     .
               10  bit-ctp-ven            pic  9(07)       comp-3     .
               10  bit-fat-sep            pic  x(01)                  .
               10  bit-inl-dcm            pic  9(02)                  .
               10  bit-inl-pgt            pic  9(02)                  .
               10  bit-cod-lst            pic  x(03)                  .
               10  bit-csr-aac            pic  9(05)       comp-3     .
               10  bit-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  bit-csc-aac            pic  9(05)       comp-3     .
               10  bit-psc-aac            pic  9(02)v9(01) comp-3     .
               10  bit-cpv-aac            pic  9(05)       comp-3     .
               10  bit-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  bit-voc-des occurs 06  pic  x(03)                  .
               10  bit-cod-fop            pic  9(07)       comp-3     .
               10  bit-scp-aap            pic  9(02)v9(01) comp-3     .
               10  bit-cod-abi            pic  9(05)       comp-3     .
               10  bit-cod-cab            pic  9(05)       comp-3     .
               10  bit-ccc-app            pic  x(12)                  .
               10  bit-nos-ban            pic  x(10)                  .
               10  bit-nos-ccp            pic  x(10)                  .
               10  bit-add-spi            pic  x(03)                  .
               10  bit-add-spb            pic  x(03)                  .
               10  bit-ipr-iel            pic  9(02)                  .
               10  bit-pag-dsm            pic  9(07)       comp-3     .
               10  bit-pag-qaf            pic  9(09)       comp-3     .
               10  bit-pag-act            pic  9(09)       comp-3     .
               10  bit-cod-age            pic  9(07)       comp-3     .
               10  bit-fsp-doc            pic  9(02)                  .
               10  bit-pvf-age            pic  9(11)       comp-3     .
               10  bit-tip-vpa            pic  9(02)                  .
               10  bit-cpv-aaa            pic  9(05)       comp-3     .
               10  bit-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  bit-cod-ime            pic  9(07)       comp-3     .
               10  bit-pvf-ime            pic  9(11)       comp-3     .
               10  bit-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  bit-tot-scc            pic s9(11)       comp-3     .
               10  bit-per-scc            pic  9(02)v9(01) comp-3     .
               10  bit-civ-scc            pic  9(05)       comp-3     .
               10  bit-ccp-scc            pic  9(07)       comp-3     .
               10  bit-tot-scp            pic s9(11)       comp-3     .
               10  bit-per-scp            pic  9(02)v9(01) comp-3     .
               10  bit-civ-scp            pic  9(05)       comp-3     .
               10  bit-ccp-scp            pic  9(07)       comp-3     .
               10  bit-spe-add occurs 06.
                   15  bit-spe-snx        pic  9(01)                  .
                   15  bit-spe-mad        pic  9(01)                  .
                   15  bit-spe-per        pic  9(02)v9(01) comp-3     .
                   15  bit-spe-ibl        pic  9(02)                  .
                   15  bit-ibt-spe.
                       20  bit-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  bit-spe-imp        pic s9(09)       comp-3     .
                   15  bit-spe-civ        pic  9(05)       comp-3     .
                   15  bit-spe-ccp        pic  9(07)       comp-3     .
               10  bit-civ-spi            pic  9(05)       comp-3     .
               10  bit-ccp-spi            pic  9(07)       comp-3     .
               10  bit-tot-sic            pic s9(09)       comp-3     .
               10  bit-tot-sia            pic s9(09)       comp-3     .
               10  bit-tot-spb            pic s9(09)       comp-3     .
               10  bit-civ-spb            pic  9(05)       comp-3     .
               10  bit-ccp-spb            pic  9(07)       comp-3     .
               10  bit-dtr-tra.
                   15  bit-tra-cur        pic  9(02)                  .
                   15  bit-cau-tra        pic  x(03)                  .
                   15  bit-asp-ben        pic  x(03)                  .
                   15  bit-num-col        pic  9(05)       comp-3     .
                   15  bit-pes-tot        pic  9(06)v9(03) comp-3     .
                   15  bit-dat-itr        pic  9(07)       comp-3     .
                   15  bit-ora-itr        pic  9(04)                  .
                   15  bit-cod-vet        pic  9(07)       comp-3     .
                   15  bit-cod-vt2        pic  9(07)       comp-3     .
                   15  bit-cod-vt3        pic  9(07)       comp-3     .
                   15  bit-cod-ant        pic  9(03)       comp-3     .
               10  bit-prt-mgd            pic  9(07)       comp-3     .
               10  bit-nrg-mgd            pic  9(02)                  .
               10  bit-dri-mgd            pic  9(07)       comp-3     .
               10  bit-nri-mgd            pic  x(10)                  .
               10  bit-nps-sdb            pic  9(11)                  .
               10  bit-ctr-sdb            pic  9(02)                  .
               10  bit-iva-cst.
                   15  bit-iva-rig occurs 06.
                       20  bit-iva-cod    pic  9(05)       comp-3     .
                       20  bit-iva-ibl    pic s9(11)       comp-3     .
                       20  bit-iva-imp    pic s9(11)       comp-3     .
                   15  bit-iva-tdo        pic s9(11)       comp-3     .
               10  bit-ctp-cst.
                   15  bit-ctp-rig occurs 10.
                       20  bit-ctp-cod    pic  9(07)       comp-3     .
                       20  bit-ctp-imp    pic s9(11)       comp-3     .
               10  bit-ctr-stp            pic  9(02)                  .
               10  bit-flg-ela.
                   15  bit-flg-blo.
                       20  bit-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bit-flg-nbl.
                       20  bit-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bit-flg-pul            pic  x(01)                  .
               10  bit-alx-exp.
                   15  filler occurs 40   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ccc]                                    *
      *    *-----------------------------------------------------------*
       fd  ccc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ccc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ccc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  ccc-k01.
                   15  ccc-cod-cli        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  ccc-k02.
                   15  ccc-ide-dat        pic  9(07)       comp-3     .
                   15  ccc-cod-cli-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ccc-dat.
               10  ccc-ide-ute            pic  x(08)                  .
               10  ccc-ide-fas            pic  x(06)                  .
               10  ccc-emi-slc            pic  9(02)                  .
               10  ccc-tdt-slc            pic  9(02)                  .
               10  ccc-tso-l01            pic  9(05)       comp-3     .
               10  ccc-tso-l02            pic  9(05)       comp-3     .
               10  ccc-tso-l03            pic  9(05)       comp-3     .
               10  ccc-tas-api            pic  9(02)v9(02)            .
               10  ccc-max-fid            pic  9(13)                  .
               10  ccc-dat-fid            pic  9(07)                  .
               10  ccc-alx-exp.
                   15  filler  occurs 30  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [cec]                                    *
      *    *-----------------------------------------------------------*
       fd  cec       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  cec-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  cec-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : ESERECCLI                      *
      *            *---------------------------------------------------*
               10  cec-k01.
                   15  cec-ann-ese        pic  9(03)       comp-3     .
                   15  cec-tip-rec        pic  9(02)                  .
                   15  cec-cod-cli        pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  cec-dat.
               10  cec-dat-mns.
                   15  cec-mes-mns occurs 12.
                       20  cec-prg-mns    pic s9(13)       comp-3     .
               10  cec-ale-alf.
                   15  filler  occurs 40  pic  x(01)                  .
               10  cec-ale-num.
                   15  filler  occurs 40  pic  9(01)                  .

      *    *===========================================================*
      *    * File Description [dcp]                                    *
      *    *-----------------------------------------------------------*
       fd  dcp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  dcp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  dcp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k01.
                   15  dcp-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  dcp-k02.
                   15  dcp-ide-dat        pic  9(07)       comp-3     .
                   15  dcp-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  dcp-k03.
                   15  dcp-des-key        pic  x(40)                  .
                   15  dcp-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k04.
                   15  dcp-alf-pro        pic  x(14)                  .
                   15  dcp-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k05.
                   15  dcp-syn-pro        pic  x(13)                  .
                   15  dcp-num-pro-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  dcp-k06.
                   15  dcp-cla-pro        pic  9(05)       comp-3     .
                   15  dcp-gru-pro        pic  9(05)       comp-3     .
                   15  dcp-sgr-pro        pic  9(05)       comp-3     .
                   15  dcp-des-key-6      pic  x(40)                  .
                   15  dcp-num-pro-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  dcp-k07.
                   15  dcp-cla-pro-7      pic  9(05)       comp-3     .
                   15  dcp-gru-pro-7      pic  9(05)       comp-3     .
                   15  dcp-sgr-pro-7      pic  9(05)       comp-3     .
                   15  dcp-alf-pro-7      pic  x(14)                  .
                   15  dcp-num-pro-7      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : KLBPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k08.
                   15  dcp-klb-pro        pic  x(13)                  .
                   15  dcp-num-pro-8      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  dcp-dat.
               10  dcp-inf-gen.
                   15  dcp-ide-ute        pic  x(08)                  .
                   15  dcp-ide-fas        pic  x(06)                  .
                   15  dcp-des-pro        pic  x(40)                  .
                   15  dcp-des-pdx        pic  9(01)                  .
                   15  dcp-tip-pro        pic  9(02)                  .
                   15  dcp-tip-acp        pic  9(02)                  .
                   15  dcp-not-g01        pic  x(40)                  .
               10  dcp-inf-fis.
                   15  dcp-tip-cfz        pic  9(02)                  .
                   15  dcp-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  dcp-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  dcp-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  dcp-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  dcp-dim-pro.
                       20  dcp-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  dcp-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  dcp-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  dcp-pcl-fis        pic  x(10)                  .
                   15  dcp-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  dcp-coe-div        pic  9(04)v9(03) comp-3     .
                   15  dcp-spc-lib        pic  x(20)                  .
               10  dcp-inf-fat.
                   15  dcp-cod-iva        pic  9(05)       comp-3     .
                   15  dcp-ctp-ven        pic  9(07)       comp-3     .
                   15  dcp-umi-ven        pic  x(03)                  .
                   15  dcp-dec-qta        pic  9(01)                  .
                   15  dcp-sgl-vlt        pic  x(03)                  .
                   15  dcp-dec-vlt        pic  9(01)                  .
                   15  dcp-dec-prz        pic  9(01)                  .
                   15  dcp-prz-lst        pic  9(09)       comp-3     .
                   15  dcp-lot-ven        pic  9(06)v9(03) comp-3     .
                   15  dcp-epz-rgf        pic  9(01)                  .
                   15  dcp-snx-2qt        pic  9(01)                  .
                   15  dcp-dec-2qt        pic  9(01)                  .
                   15  dcp-snx-3qt        pic  9(01)                  .
                   15  dcp-dec-3qt        pic  9(01)                  .
                   15  dcp-snx-2pz        pic  9(01)                  .
                   15  dcp-tip-vve        pic  x(03)                  .
               10  dcp-inf-cdv.
                   15  dcp-cat-scr        pic  9(05)       comp-3     .
                   15  dcp-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  dcp-inf-gag.
                   15  dcp-cat-pvg        pic  9(05)       comp-3     .
                   15  dcp-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
                   15  dcp-amm-pvg        pic  9(09)       comp-3     .
               10  dcp-inf-lst.
                   15  dcp-snx-lst        pic  9(01)                  .
                   15  dcp-epz-lst.
                       20  dcp-epz-ele occurs 20
                                          pic  x(01)                  .
                   15  dcp-pag-lst        pic  x(10)                  .
                   15  dcp-rfl-lst        pic  x(12)                  .
                   15  dcp-tmc-lst        pic  9(03)       comp-3     .
                   15  dcp-daa-lst        pic  9(01)                  .
                   15  dcp-aut-lst        pic  x(03)                  .
               10  dcp-inf-pcs.
                   15  dcp-cod-s01        pic  9(05)       comp-3     .
                   15  dcp-cod-s02        pic  9(05)       comp-3     .
                   15  dcp-cod-s03        pic  9(05)       comp-3     .
                   15  dcp-dat-icm        pic  9(07)       comp-3     .
                   15  dcp-sta-tus        pic  9(02)                  .
                   15  dcp-sta-tud        pic  9(07)       comp-3     .
                   15  dcp-sta-tuc        pic  9(07)       comp-3     .
                   15  dcp-sta-tux        pic  9(02)                  .
               10  dcp-inf-mkt.
                   15  dcp-cld-imp        pic  x(01)                  .
                   15  dcp-pre-ctn        pic  9(02)                  .
                   15  dcp-gra-ico        pic  9(02)                  .
                   15  dcp-pcl-ccz        pic  9(02)                  .
                   15  dcp-cod-mkt        pic  9(05)       comp-3     .
                   15  dcp-ind-mkt        pic  x(10)                  .
               10  dcp-inf-bdg.
                   15  dcp-cla-bdg        pic  9(05)       comp-3     .
               10  dcp-inf-bol.
                   15  dcp-for-blo        pic  9(02)                  .
                   15  dcp-dor-blo        pic  9(07)       comp-3     .
                   15  dcp-fco-blo        pic  9(02)                  .
                   15  dcp-dco-blo        pic  9(07)       comp-3     .
               10  dcp-inf-iic.
                   15  dcp-cdn-cdm        pic  9(08)                  .
               10  dcp-inf-aps.
                   15  dcp-alx-exp.
                       20  filler occurs 80
                                          pic  x(01)                  .











      *    *===========================================================*
      *    * File Description [lsd]                                    *
      *    *-----------------------------------------------------------*
       fd  lsd       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  lsd-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  lsd-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTPRODVF                      *
      *            *---------------------------------------------------*
               10  lsd-k01.
                   15  lsd-tip-rec        pic  9(02)                  .
                   15  lsd-cod-lst        pic  x(03)                  .
                   15  lsd-cod-cli        pic  9(07)       comp-3     .
                   15  lsd-sgl-vlt        pic  x(03)                  .
                   15  lsd-num-pro        pic  9(07)       comp-3     .
                   15  lsd-dva-fin        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : LSTPRODFR                      *
      *            *---------------------------------------------------*
               10  lsd-k02.
                   15  lsd-tip-rec-2      pic  9(02)                  .
                   15  lsd-cod-lst-2      pic  x(03)                  .
                   15  lsd-cod-cli-2      pic  9(07)       comp-3     .
                   15  lsd-sgl-vlt-2      pic  x(03)                  .
                   15  lsd-num-pro-2      pic  9(07)       comp-3     .
                   15  lsd-dvf-rev        pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  lsd-dat.
               10  lsd-dec-vlt            pic  9(01)                  .
               10  lsd-prz-lst            pic  9(09)       comp-3     .
               10  lsd-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  lsd-per-sco occurs 05  pic  9(02)v9(01) comp-3     .
               10  lsd-snx-prz            pic  x(01)                  .
               10  lsd-snx-sco            pic  x(01)                  .
               10  lsd-snx-pvg            pic  x(01)                  .
               10  lsd-dva-ini            pic  9(07)                  .
               10  lsd-alx-exp.
                   15  lsdler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [lst]                                    *
      *    *-----------------------------------------------------------*
       fd  lst       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  lst-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  lst-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTPRO                         *
      *            *---------------------------------------------------*
               10  lst-k01.
                   15  lst-tip-rec        pic  9(02)                  .
                   15  lst-cod-lst        pic  x(03)                  .
                   15  lst-cod-cli        pic  9(07)       comp-3     .
                   15  lst-sgl-vlt        pic  x(03)                  .
                   15  lst-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PROLST                         *
      *            *---------------------------------------------------*
               10  lst-k02.
                   15  lst-tip-rec-2      pic  9(02)                  .
                   15  lst-num-pro-2      pic  9(07)       comp-3     .
                   15  lst-sgl-vlt-2      pic  x(03)                  .
                   15  lst-cod-lst-2      pic  x(03)                  .
                   15  lst-cod-cli-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  lst-dat.
               10  lst-dec-vlt            pic  9(01)                  .
               10  lst-prz-lst            pic  9(09)       comp-3     .
               10  lst-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  lst-per-sco occurs 05  pic  9(02)v9(01) comp-3     .
               10  lst-snx-prz            pic  x(01)                  .
               10  lst-snx-sco            pic  x(01)                  .
               10  lst-snx-pvg            pic  x(01)                  .
               10  lst-dva-ini            pic  9(07)                  .
               10  lst-dva-fin            pic  9(07)                  .
               10  lst-alx-exp.
                   15  filler occurs 06   pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [aaf]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaf.
           05  f-aaf-nam                  pic  x(04)                  .
           05  f-aaf-pat                  pic  x(40)                  .
           05  f-aaf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [aaq]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaq.
           05  f-aaq-nam                  pic  x(04)                  .
           05  f-aaq-pat                  pic  x(40)                  .
           05  f-aaq-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ali]                                       *
      *    *-----------------------------------------------------------*
       01  f-ali.
           05  f-ali-nam                  pic  x(04)                  .
           05  f-ali-pat                  pic  x(40)                  .
           05  f-ali-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bfr]                                       *
      *    *-----------------------------------------------------------*
       01  f-bfr.
           05  f-bfr-nam                  pic  x(04)                  .
           05  f-bfr-pat                  pic  x(40)                  .
           05  f-bfr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bft]                                       *
      *    *-----------------------------------------------------------*
       01  f-bft.
           05  f-bft-nam                  pic  x(04)                  .
           05  f-bft-pat                  pic  x(40)                  .
           05  f-bft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bir]                                       *
      *    *-----------------------------------------------------------*
       01  f-bir.
           05  f-bir-nam                  pic  x(04)                  .
           05  f-bir-pat                  pic  x(40)                  .
           05  f-bir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bit]                                       *
      *    *-----------------------------------------------------------*
       01  f-bit.
           05  f-bit-nam                  pic  x(04)                  .
           05  f-bit-pat                  pic  x(40)                  .
           05  f-bit-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ccc]                                       *
      *    *-----------------------------------------------------------*
       01  f-ccc.
           05  f-ccc-nam                  pic  x(04)                  .
           05  f-ccc-pat                  pic  x(40)                  .
           05  f-ccc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [cec]                                       *
      *    *-----------------------------------------------------------*
       01  f-cec.
           05  f-cec-nam                  pic  x(04)                  .
           05  f-cec-pat                  pic  x(40)                  .
           05  f-cec-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [dcp]                                       *
      *    *-----------------------------------------------------------*
       01  f-dcp.
           05  f-dcp-nam                  pic  x(04)                  .
           05  f-dcp-pat                  pic  x(40)                  .
           05  f-dcp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ddp]                                       *
      *    *-----------------------------------------------------------*
       01  f-ddp.
           05  f-ddp-nam                  pic  x(04)                  .
           05  f-ddp-pat                  pic  x(40)                  .
           05  f-ddp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ffr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ffr.
           05  f-ffr-nam                  pic  x(04)                  .
           05  f-ffr-pat                  pic  x(40)                  .
           05  f-ffr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fft]                                       *
      *    *-----------------------------------------------------------*
       01  f-fft.
           05  f-fft-nam                  pic  x(04)                  .
           05  f-fft-pat                  pic  x(40)                  .
           05  f-fft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fir]                                       *
      *    *-----------------------------------------------------------*
       01  f-fir.
           05  f-fir-nam                  pic  x(04)                  .
           05  f-fir-pat                  pic  x(40)                  .
           05  f-fir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fit]                                       *
      *    *-----------------------------------------------------------*
       01  f-fit.
           05  f-fit-nam                  pic  x(04)                  .
           05  f-fit-pat                  pic  x(40)                  .
           05  f-fit-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [gpc]                                       *
      *    *-----------------------------------------------------------*
       01  f-gpc.
           05  f-gpc-nam                  pic  x(04)                  .
           05  f-gpc-pat                  pic  x(40)                  .
           05  f-gpc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [gpm]                                       *
      *    *-----------------------------------------------------------*
       01  f-gpm.
           05  f-gpm-nam                  pic  x(04)                  .
           05  f-gpm-pat                  pic  x(40)                  .
           05  f-gpm-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [iir]                                       *
      *    *-----------------------------------------------------------*
       01  f-iir.
           05  f-iir-nam                  pic  x(04)                  .
           05  f-iir-pat                  pic  x(40)                  .
           05  f-iir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [iit]                                       *
      *    *-----------------------------------------------------------*
       01  f-iit.
           05  f-iit-nam                  pic  x(04)                  .
           05  f-iit-pat                  pic  x(40)                  .
           05  f-iit-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ivp]                                       *
      *    *-----------------------------------------------------------*
       01  f-ivp.
           05  f-ivp-nam                  pic  x(04)                  .
           05  f-ivp-pat                  pic  x(40)                  .
           05  f-ivp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [lfd]                                       *
      *    *-----------------------------------------------------------*
       01  f-lfd.
           05  f-lfd-nam                  pic  x(04)                  .
           05  f-lfd-pat                  pic  x(40)                  .
           05  f-lfd-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [lsd]                                       *
      *    *-----------------------------------------------------------*
       01  f-lsd.
           05  f-lsd-nam                  pic  x(04)                  .
           05  f-lsd-pat                  pic  x(40)                  .
           05  f-lsd-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [lst]                                       *
      *    *-----------------------------------------------------------*
       01  f-lst.
           05  f-lst-nam                  pic  x(04)                  .
           05  f-lst-pat                  pic  x(40)                  .
           05  f-lst-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mgr]                                       *
      *    *-----------------------------------------------------------*
       01  f-mgr.
           05  f-mgr-nam                  pic  x(04)                  .
           05  f-mgr-pat                  pic  x(40)                  .
           05  f-mgr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mgs]                                       *
      *    *-----------------------------------------------------------*
       01  f-mgs.
           05  f-mgs-nam                  pic  x(04)                  .
           05  f-mgs-pat                  pic  x(40)                  .
           05  f-mgs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mgt]                                       *
      *    *-----------------------------------------------------------*
       01  f-mgt.
           05  f-mgt-nam                  pic  x(04)                  .
           05  f-mgt-pat                  pic  x(40)                  .
           05  f-mgt-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mmr]                                       *
      *    *-----------------------------------------------------------*
       01  f-mmr.
           05  f-mmr-nam                  pic  x(04)                  .
           05  f-mmr-pat                  pic  x(40)                  .
           05  f-mmr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mmt]                                       *
      *    *-----------------------------------------------------------*
       01  f-mmt.
           05  f-mmt-nam                  pic  x(04)                  .
           05  f-mmt-pat                  pic  x(40)                  .
           05  f-mmt-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mmv]                                       *
      *    *-----------------------------------------------------------*
       01  f-mmv.
           05  f-mmv-nam                  pic  x(04)                  .
           05  f-mmv-pat                  pic  x(40)                  .
           05  f-mmv-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [moi]                                       *
      *    *-----------------------------------------------------------*
       01  f-moi.
           05  f-moi-nam                  pic  x(04)                  .
           05  f-moi-pat                  pic  x(40)                  .
           05  f-moi-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ocr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ocr.
           05  f-ocr-nam                  pic  x(04)                  .
           05  f-ocr-pat                  pic  x(40)                  .
           05  f-ocr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [oct]                                       *
      *    *-----------------------------------------------------------*
       01  f-oct.
           05  f-oct-nam                  pic  x(04)                  .
           05  f-oct-pat                  pic  x(40)                  .
           05  f-oct-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ofr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ofr.
           05  f-ofr-nam                  pic  x(04)                  .
           05  f-ofr-pat                  pic  x(40)                  .
           05  f-ofr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [oft]                                       *
      *    *-----------------------------------------------------------*
       01  f-oft.
           05  f-oft-nam                  pic  x(04)                  .
           05  f-oft-pat                  pic  x(40)                  .
           05  f-oft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [osr]                                       *
      *    *-----------------------------------------------------------*
       01  f-osr.
           05  f-osr-nam                  pic  x(04)                  .
           05  f-osr-pat                  pic  x(40)                  .
           05  f-osr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ost]                                       *
      *    *-----------------------------------------------------------*
       01  f-ost.
           05  f-ost-nam                  pic  x(04)                  .
           05  f-ost-pat                  pic  x(40)                  .
           05  f-ost-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [rdb]                                       *
      *    *-----------------------------------------------------------*
       01  f-rdb.
           05  f-rdb-nam                  pic  x(04)                  .
           05  f-rdb-pat                  pic  x(40)                  .
           05  f-rdb-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [rsd]                                       *
      *    *-----------------------------------------------------------*
       01  f-rsd.
           05  f-rsd-nam                  pic  x(04)                  .
           05  f-rsd-pat                  pic  x(40)                  .
           05  f-rsd-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [sdb]                                       *
      *    *-----------------------------------------------------------*
       01  f-sdb.
           05  f-sdb-nam                  pic  x(04)                  .
           05  f-sdb-pat                  pic  x(40)                  .
           05  f-sdb-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [sfa]                                       *
      *    *-----------------------------------------------------------*
       01  f-sfa.
           05  f-sfa-nam                  pic  x(04)                  .
           05  f-sfa-pat                  pic  x(40)                  .
           05  f-sfa-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [sff]                                       *
      *    *-----------------------------------------------------------*
       01  f-sff.
           05  f-sff-nam                  pic  x(04)                  .
           05  f-sff-pat                  pic  x(40)                  .
           05  f-sff-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [sfp]                                       *
      *    *-----------------------------------------------------------*
       01  f-sfp.
           05  f-sfp-nam                  pic  x(04)                  .
           05  f-sfp-pat                  pic  x(40)                  .
           05  f-sfp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [sfs]                                       *
      *    *-----------------------------------------------------------*
       01  f-sfs.
           05  f-sfs-nam                  pic  x(04)                  .
           05  f-sfs-pat                  pic  x(40)                  .
           05  f-sfs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [vbv]                                       *
      *    *-----------------------------------------------------------*
       01  f-vbv.
           05  f-vbv-nam                  pic  x(04)                  .
           05  f-vbv-pat                  pic  x(40)                  .
           05  f-vbv-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ybo]                                       *
      *    *-----------------------------------------------------------*
       01  f-ybo.
           05  f-ybo-nam                  pic  x(04)                  .
           05  f-ybo-pat                  pic  x(40)                  .
           05  f-ybo-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [yin]                                       *
      *    *-----------------------------------------------------------*
       01  f-yin.
           05  f-yin-nam                  pic  x(04)                  .
           05  f-yin-pat                  pic  x(40)                  .
           05  f-yin-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ysf]                                       *
      *    *-----------------------------------------------------------*
       01  f-ysf.
           05  f-ysf-nam                  pic  x(04)                  .
           05  f-ysf-pat                  pic  x(40)                  .
           05  f-ysf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zac]                                       *
      *    *-----------------------------------------------------------*
       01  f-zac.
           05  f-zac-nam                  pic  x(04)                  .
           05  f-zac-pat                  pic  x(40)                  .
           05  f-zac-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zbo]                                       *
      *    *-----------------------------------------------------------*
       01  f-zbo.
           05  f-zbo-nam                  pic  x(04)                  .
           05  f-zbo-pat                  pic  x(40)                  .
           05  f-zbo-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zin]                                       *
      *    *-----------------------------------------------------------*
       01  f-zin.
           05  f-zin-nam                  pic  x(04)                  .
           05  f-zin-pat                  pic  x(40)                  .
           05  f-zin-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zsb]                                       *
      *    *-----------------------------------------------------------*
       01  f-zsb.
           05  f-zsb-nam                  pic  x(04)                  .
           05  f-zsb-pat                  pic  x(40)                  .
           05  f-zsb-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zsf]                                       *
      *    *-----------------------------------------------------------*
       01  f-zsf.
           05  f-zsf-nam                  pic  x(04)                  .
           05  f-zsf-pat                  pic  x(40)                  .
           05  f-zsf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [ali]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfali"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [ccc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfccc"                          .
      *        *-------------------------------------------------------*
      *        * [cec]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcec"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [ffr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .
      *        *-------------------------------------------------------*
      *        * [fft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rffft"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [gpc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpc"                          .
      *        *-------------------------------------------------------*
      *        * [gpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpm"                          .
      *        *-------------------------------------------------------*
      *        * [iir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiir"                          .
      *        *-------------------------------------------------------*
      *        * [iit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiit"                          .
      *        *-------------------------------------------------------*
      *        * [ivp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfivp"                          .
      *        *-------------------------------------------------------*
      *        * [lfd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rflfd"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mmr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .
      *        *-------------------------------------------------------*
      *        * [mmt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmt"                          .
      *        *-------------------------------------------------------*
      *        * [mmv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmv"                          .
      *        *-------------------------------------------------------*
      *        * [moi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmoi"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .
      *        *-------------------------------------------------------*
      *        * [rdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfrdb"                          .
      *        *-------------------------------------------------------*
      *        * [rsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsd"                          .
      *        *-------------------------------------------------------*
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [sfa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfa"                          .
      *        *-------------------------------------------------------*
      *        * [sff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsff"                          .
      *        *-------------------------------------------------------*
      *        * [sfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .
      *        *-------------------------------------------------------*
      *        * [sfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfs"                          .
      *        *-------------------------------------------------------*
      *        * [vbv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbv"                          .
      *        *-------------------------------------------------------*
      *        * [ybo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfybo"                          .
      *        *-------------------------------------------------------*
      *        * [yin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyin"                          .
      *        *-------------------------------------------------------*
      *        * [ysf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfysf"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zsb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzsb"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cnv3p1"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnv3p1  "                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "          CONVERSIONE PER EURO          "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-nam                  pic  x(04)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-sts                  pic  x(02)                  .
           05  f-xxx-obj                  pic  x(40)                  .
           05  f-xxx-nrl                  pic  9(09)                  .
           05  f-xxx-nrs                  pic  9(09)                  .
           05  f-xxx-nrc                  pic  9(02)                  .
           05  f-xxx-nrd                  pic  9(01)                  .
           05  f-xxx-ope                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
      *            *---------------------------------------------------*
      *            * Da accettazione campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-acc-ric-sel      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ric-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts richieste                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati richieste                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste pre esecuzione interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico interrogazione        *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento automatico interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-aut      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Si/No primo giro di esecuzione            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-prm-gir      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento qry-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-qry.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-qry-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-liv.
                   15  w-cnt-qry-sav-l05  pic  x(64)                  .
                   15  w-cnt-qry-sav-l04  pic  x(64)                  .
                   15  w-cnt-qry-sav-l03  pic  x(64)                  .
                   15  w-cnt-qry-sav-l02  pic  x(64)                  .
                   15  w-cnt-qry-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Literals                                              *
      *        *-------------------------------------------------------*
           05  w-cnt-lit.
               10  w-cnt-lit-t80          pic  x(80) value all "="    .
      *        *-------------------------------------------------------*
      *        * Work per padding campi alfanumerici con 'z'           *
      *        *-------------------------------------------------------*
           05  w-pad-zzz.
               10  w-pad-zzz-alf.
                   15  w-pad-zzz-alf-chr
                                   occurs 20       
                                          pic  x(01)                  .
               10  w-pad-zzz-ctr          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Conversione automatica o manuale                      *
      *        *  - A1 : Automatica                                    *
      *        *  - M1 : Manuale                                       *
      *        *  - A2 : Automatica, riscrittura                       *
      *        *  - M2 : Manuale, riscrittura                          *
      *        *-------------------------------------------------------*
           05  rr-aut-man                 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice azienda                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-azi                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per presa visione                              *
      *        *-------------------------------------------------------*
           05  rr-pre-vis                 pic  x(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo conversione                           *
      *        *-------------------------------------------------------*
           05  w-exp-aut-man.
               10  w-exp-aut-man-num      pic  9(02)       value 04   .
               10  w-exp-aut-man-lun      pic  9(02)       value 40   .
               10  w-exp-aut-man-tbl.
                   15  filler             pic  x(40) value
                          "Automatica, per tutti gli archivi       "  .
                   15  filler             pic  x(40) value
                          "Manuale, un archivio alla volta         "  .
                   15  filler             pic  x(40) value
                          "Automatica, riscrittura archivi         "  .
                   15  filler             pic  x(40) value
                          "Manuale, riscrittura archivi            "  .
      *        *-------------------------------------------------------*
      *        * Work per : Risposta Si/No                             *
      *        *-------------------------------------------------------*
           05  w-exp-ris-snx.
               10  w-exp-ris-snx-num      pic  9(02)       value 02   .
               10  w-exp-ris-snx-lun      pic  9(02)       value 02   .
               10  w-exp-ris-snx-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det prezzo netto                             *
      *        *-------------------------------------------------------*
           05  w-det-prz-net.
               10  w-det-prz-net-prz      pic  9(09)                  .
               10  w-det-prz-net-psc occurs 05
                                          pic  9(02)v9(01)            .
               10  w-det-prz-net-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Cal                               *
      *    *-----------------------------------------------------------*
       01  w-cal.
      *        *-------------------------------------------------------*
      *        * Work per Cal importo scontato                         *
      *        *-------------------------------------------------------*
           05  w-cal-imp-sco.
               10  w-cal-imp-sco-iml      pic s9(11)                  .
               10  w-cal-imp-sco-psc      pic  9(02)v9(01)            .
               10  w-cal-imp-sco-w01      pic  9(03)v9(01)            .
               10  w-cal-imp-sco-w02      pic s9(14)v9(01)            .
               10  w-cal-imp-sco-imn      pic s9(11)                  .
               10  w-cal-imp-sco-ams      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Cal importo in EURO                          *
      *        *-------------------------------------------------------*
           05  w-cal-imp-eur.
               10  w-cal-imp-eur-imi      pic s9(13)v9(02)            .
               10  w-cal-imp-eur-ime      pic s9(13)v9(02)            .
               10  w-cal-imp-eur-int      pic  9(02)                  .
               10  w-cal-imp-eur-dec      pic  9(01)                  .
               10  w-cal-imp-eur-wcl      pic s9(13)v9(02)            .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  w-c01                      pic  9(02)                  .
           05  w-c02                      pic  9(02)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Se no richieste : a esecuzione conversione      *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-500.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-500.
      *              *-------------------------------------------------*
      *              * Esecuzione conversione file                     *
      *              *-------------------------------------------------*
           perform   exe-cnv-fil-000      thru exe-cnv-fil-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a fine programma              *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Test se fine esecuzione da operatore            *
      *              *-------------------------------------------------*
           if        w-cnt-qry-rou-pri    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-750
           else      go to main-250.
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione richieste di selezione                       *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-ric-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *                  *---------------------------------------------*
      *                  * Tipo conversione                            *
      *                  *---------------------------------------------*
           perform   acc-aut-man-000      thru acc-aut-man-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           perform   acc-cod-azi-000      thru acc-cod-azi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visualizzazione        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-910.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma esecuzione del programma (S/E) ?"
                                          to   v-not                  .
           move      " "                  to   v-alf                  .
           move      "SE"                 to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else      go to acc-ric-sel-910.
       acc-ric-sel-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     move  "S"            to   w-cnt-acc-ric-sel
                     go to acc-ric-sel-999
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-ric-sel-900.
       acc-ric-sel-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
           go to     acc-ric-sel-999.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo conversione                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conversione           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice azienda                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice azienda precedente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione linea di separazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Record letti                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Record letti               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Record scritti                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Record scritti             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo conversione           *
      *    *-----------------------------------------------------------*
       acc-aut-man-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-aut-man-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aut-man-lun    to   v-car                  .
           move      w-exp-aut-man-num    to   v-ldt                  .
           move      "1234#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aut-man-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-aut-man           =    "A1"
                     move  01             to   v-num
           else if   rr-aut-man           =    "M1"
                     move  02             to   v-num
           else if   rr-aut-man           =    "A2"
                     move  03             to   v-num
           else if   rr-aut-man           =    "M2"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-aut-man-999.
       acc-aut-man-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "A1"           to   rr-aut-man
           else if   v-num                =    02
                     move  "M1"           to   rr-aut-man
           else if   v-num                =    03
                     move  "A2"           to   rr-aut-man
           else if   v-num                =    04
                     move  "M2"           to   rr-aut-man
           else      move  spaces         to   rr-aut-man             .
       acc-aut-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    spaces
                     go to acc-aut-man-100.
       acc-aut-man-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-aut-man-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-aut-man-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-aut-man-100.
       acc-aut-man-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice azienda                             *
      *    *-----------------------------------------------------------*
       acc-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-azi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-azi           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-azi-999.
       acc-cod-azi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-azi             .
       acc-cod-azi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-cod-azi           =    spaces
                     go to acc-cod-azi-100.
       acc-cod-azi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-azi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-azi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-azi-100.
       acc-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice azienda                          *
      *    *-----------------------------------------------------------*
       vis-cod-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-azi           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controllo su tipo conversione                   *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "A1" or
                     rr-aut-man           =    "M1" or
                     rr-aut-man           =    "A2" or
                     rr-aut-man           =    "M2"
                     go to tdo-ric-sel-200.
           move      "ME"                 to   v-ope                  .
           move      "Tipo conversione errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su Codice azienda                     *
      *              *-------------------------------------------------*
           if        rr-cod-azi           not  = spaces
                     go to tdo-ric-sel-999.
           move      "ME"                 to   v-ope                  .
           move      "Manca il codice azienda !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-cod-azi             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di conversione                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fil-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-fil-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione tipo operazione da eseguire    *
      *              *-------------------------------------------------*
           if        rr-aut-man (2 : 1)   =    "1"
                     move  "PT"           to   f-xxx-ope
           else      move  "FP"           to   f-xxx-ope              .
       exe-cnv-fil-200.
      *              *-------------------------------------------------*
      *              * Conversione [aaf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaf-000      thru exe-cnv-aaf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [aaq]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaq-000      thru exe-cnv-aaq-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ali]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ali-000      thru exe-cnv-ali-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bfr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bfr-000      thru exe-cnv-bfr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bft]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bft-000      thru exe-cnv-bft-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bir]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bir-000      thru exe-cnv-bir-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bit]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bit-000      thru exe-cnv-bit-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ccc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ccc-000      thru exe-cnv-ccc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [cec]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-cec-000      thru exe-cnv-cec-999        .
      *              *-------------------------------------------------*
      *              * Conversione [dcp]                               *
      *              *-------------------------------------------------*
______*    perform   exe-cnv-dcp-000      thru exe-cnv-dcp-999        .
       exe-cnv-fil-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione rullino messaggi                *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-cnv-fil-999.
           exit.

      *    *===========================================================*
      *    * Conversione [aaf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-aaf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "aaf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-aaf-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-aaf-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-aaf-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-aaf-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    aaf                                       .
       exe-cnv-aaf-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   aaf-k01                .
           start     aaf    key not less
                            aaf-k01
                            invalid key
                            go to exe-cnv-aaf-800.
       exe-cnv-aaf-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      aaf    next
                            with no lock
                            at end
                            go to exe-cnv-aaf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-aaf-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
           if        aaf-sgl-vlt          =    c-sgl
                     go to exe-cnv-aaf-250.
           if        aaf-sgl-vlt          not  = "LIT"
                     go to exe-cnv-aaf-250.
       exe-cnv-aaf-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       exe-cnv-aaf-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-aaf                 .
           move      aaf-ide-dat          to   rf-aaf-ide-dat         .
           move      aaf-ide-ute          to   rf-aaf-ide-ute         .
           move      aaf-ide-fas          to   rf-aaf-ide-fas         .
           move      aaf-tip-mag          to   rf-aaf-tip-mag         .
           move      aaf-num-pro          to   rf-aaf-num-pro         .
           move      aaf-cod-dcf          to   rf-aaf-cod-dcf         .
           move      aaf-fda-pif          to   rf-aaf-fda-pif         .
           move      aaf-cop-sfn          to   rf-aaf-cop-sfn         .
           move      aaf-dep-sfn          to   rf-aaf-dep-sfn         .
           move      aaf-xdp-sfn          to   rf-aaf-xdp-sfn         .
           move      aaf-snx-tum          to   rf-aaf-snx-tum         .
           move      aaf-umf-tum          to   rf-aaf-umf-tum         .
           move      aaf-nde-tum          to   rf-aaf-nde-tum         .
           move      aaf-cmo-tum          to   rf-aaf-cmo-tum         .
           move      aaf-cdi-tum          to   rf-aaf-cdi-tum         .
           move      aaf-dpz-dcf          to   rf-aaf-dpz-dcf         .
           move      aaf-ann-not          to   rf-aaf-ann-not         .
           move      aaf-tmp-cns          to   rf-aaf-tmp-cns         .
           move      aaf-sgl-vlt          to   rf-aaf-sgl-vlt         .
           move      aaf-dec-vlt          to   rf-aaf-dec-vlt         .
           move      aaf-dec-prz          to   rf-aaf-dec-prz         .
           move      aaf-tip-pza          to   rf-aaf-tip-pza         .
           move      aaf-lot-acq          to   rf-aaf-lot-acq         .
           move      aaf-tap-pes          to   rf-aaf-tap-pes         .
           move      aaf-uda-pes          to   rf-aaf-uda-pes         .
           move      aaf-lgv-vlt          to   rf-aaf-lgv-vlt         .
           move      aaf-lgv-dcv          to   rf-aaf-lgv-dcv         .
           move      aaf-lgv-tdc          to   rf-aaf-lgv-tdc         .
           move      aaf-lgv-cdc          to   rf-aaf-lgv-cdc         .
           move      aaf-lgv-pdt          to   rf-aaf-lgv-pdt         .
           move      aaf-qta-pes (1)      to   rf-aaf-qta-pes (1)     .
           move      aaf-qta-pes (2)      to   rf-aaf-qta-pes (2)     .
           move      aaf-qta-pes (3)      to   rf-aaf-qta-pes (3)     .
           move      aaf-qta-pes (4)      to   rf-aaf-qta-pes (4)     .
           move      aaf-qta-pes (5)      to   rf-aaf-qta-pes (5)     .
           move      aaf-qta-pes (6)      to   rf-aaf-qta-pes (6)     .
           move      aaf-prz-pes (1)      to   rf-aaf-prz-pes (1)     .
           move      aaf-prz-pes (2)      to   rf-aaf-prz-pes (2)     .
           move      aaf-prz-pes (3)      to   rf-aaf-prz-pes (3)     .
           move      aaf-prz-pes (4)      to   rf-aaf-prz-pes (4)     .
           move      aaf-prz-pes (5)      to   rf-aaf-prz-pes (5)     .
           move      aaf-prz-pes (6)      to   rf-aaf-prz-pes (6)     .
           move      aaf-csr-pes (1)      to   rf-aaf-csr-pes (1)     .
           move      aaf-csr-pes (2)      to   rf-aaf-csr-pes (2)     .
           move      aaf-csr-pes (3)      to   rf-aaf-csr-pes (3)     .
           move      aaf-csr-pes (4)      to   rf-aaf-csr-pes (4)     .
           move      aaf-csr-pes (5)      to   rf-aaf-csr-pes (5)     .
           move      aaf-csr-pes (6)      to   rf-aaf-csr-pes (6)     .
           move      aaf-psr-pes (1, 1)   to   rf-aaf-psr-pes (1, 1)  .
           move      aaf-psr-pes (1, 2)   to   rf-aaf-psr-pes (1, 2)  .
           move      aaf-psr-pes (1, 3)   to   rf-aaf-psr-pes (1, 3)  .
           move      aaf-psr-pes (1, 4)   to   rf-aaf-psr-pes (1, 4)  .
           move      aaf-psr-pes (1, 5)   to   rf-aaf-psr-pes (1, 5)  .
           move      aaf-psr-pes (2, 1)   to   rf-aaf-psr-pes (2, 1)  .
           move      aaf-psr-pes (2, 2)   to   rf-aaf-psr-pes (2, 2)  .
           move      aaf-psr-pes (2, 3)   to   rf-aaf-psr-pes (2, 3)  .
           move      aaf-psr-pes (2, 4)   to   rf-aaf-psr-pes (2, 4)  .
           move      aaf-psr-pes (2, 5)   to   rf-aaf-psr-pes (2, 5)  .
           move      aaf-psr-pes (3, 1)   to   rf-aaf-psr-pes (3, 1)  .
           move      aaf-psr-pes (3, 2)   to   rf-aaf-psr-pes (3, 2)  .
           move      aaf-psr-pes (3, 3)   to   rf-aaf-psr-pes (3, 3)  .
           move      aaf-psr-pes (3, 4)   to   rf-aaf-psr-pes (3, 4)  .
           move      aaf-psr-pes (3, 5)   to   rf-aaf-psr-pes (3, 5)  .
           move      aaf-psr-pes (4, 1)   to   rf-aaf-psr-pes (4, 1)  .
           move      aaf-psr-pes (4, 2)   to   rf-aaf-psr-pes (4, 2)  .
           move      aaf-psr-pes (4, 3)   to   rf-aaf-psr-pes (4, 3)  .
           move      aaf-psr-pes (4, 4)   to   rf-aaf-psr-pes (4, 4)  .
           move      aaf-psr-pes (4, 5)   to   rf-aaf-psr-pes (4, 5)  .
           move      aaf-psr-pes (5, 1)   to   rf-aaf-psr-pes (5, 1)  .
           move      aaf-psr-pes (5, 2)   to   rf-aaf-psr-pes (5, 2)  .
           move      aaf-psr-pes (5, 3)   to   rf-aaf-psr-pes (5, 3)  .
           move      aaf-psr-pes (5, 4)   to   rf-aaf-psr-pes (5, 4)  .
           move      aaf-psr-pes (5, 5)   to   rf-aaf-psr-pes (5, 5)  .
           move      aaf-psr-pes (6, 1)   to   rf-aaf-psr-pes (6, 1)  .
           move      aaf-psr-pes (6, 2)   to   rf-aaf-psr-pes (6, 2)  .
           move      aaf-psr-pes (6, 3)   to   rf-aaf-psr-pes (6, 3)  .
           move      aaf-psr-pes (6, 4)   to   rf-aaf-psr-pes (6, 4)  .
           move      aaf-psr-pes (6, 5)   to   rf-aaf-psr-pes (6, 5)  .
           move      aaf-per-mpa          to   rf-aaf-per-mpa         .
           move      aaf-alx-exp          to   rf-aaf-alx-exp         .
       exe-cnv-aaf-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-aaf-ide-dat         .
           move      s-ute                to   rf-aaf-ide-ute         .
           move      s-fas                to   rf-aaf-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Valuta base                                 *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-aaf-sgl-vlt         .
           move      c-dec                to   rf-aaf-dec-vlt         .
      *                  *---------------------------------------------*
      *                  * Prezzo listino 1                            *
      *                  *                                             *
      *                  * N.B.: Solo primo elemento della tabella     *
      *                  *---------------------------------------------*
           move      rf-aaf-prz-pes (1)   to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-aaf-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-aaf-prz-pes (1)     .
       exe-cnv-aaf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-aaf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-aaf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-aaf-250.
       exe-cnv-aaf-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     aaf                                              .
       exe-cnv-aaf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-aaf-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-aaf-999.
           exit.

      *    *===========================================================*
      *    * Conversione [aaq]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-aaq-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "aaq "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-aaq-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-aaq-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-aaq-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-aaq-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    aaq                                       .
       exe-cnv-aaq-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   aaq-k01                .
           start     aaq    key not less
                            aaq-k01
                            invalid key
                            go to exe-cnv-aaq-800.
       exe-cnv-aaq-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      aaq    next
                            with no lock
                            at end
                            go to exe-cnv-aaq-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-aaq-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
           if        aaq-sgl-vlt          =    c-sgl
                     go to exe-cnv-aaq-250.
           if        aaq-sgl-vlt          not  = "LIT"
                     go to exe-cnv-aaq-250.
       exe-cnv-aaq-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       exe-cnv-aaq-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-aaq                 .
           move      aaq-ide-dat          to   rf-aaq-ide-dat         .
           move      aaq-ide-ute          to   rf-aaq-ide-ute         .
           move      aaq-ide-fas          to   rf-aaq-ide-fas         .
           move      aaq-tip-mag          to   rf-aaq-tip-mag         .
           move      aaq-num-pro          to   rf-aaq-num-pro         .
           move      aaq-cod-iva          to   rf-aaq-cod-iva         .
           move      aaq-ctp-acq          to   rf-aaq-ctp-acq         .
           move      aaq-dcf-pfz          to   rf-aaq-dcf-pfz         .
           move      aaq-dpz-pfz          to   rf-aaq-dpz-pfz         .
           move      aaq-sgl-vlt          to   rf-aaq-sgl-vlt         .
           move      aaq-dec-vlt          to   rf-aaq-dec-vlt         .
           move      aaq-dec-prz          to   rf-aaq-dec-prz         .
           move      aaq-prz-acr          to   rf-aaq-prz-acr         .
           move      aaq-uda-par          to   rf-aaq-uda-par         .
           move      aaq-cod-pdt          to   rf-aaq-cod-pdt         .
           move      aaq-cdp-pdt          to   rf-aaq-cdp-pdt         .
           move      aaq-tmp-apv          to   rf-aaq-tmp-apv         .
           move      aaq-epz-rgf          to   rf-aaq-epz-rgf         .
           move      aaq-snx-2qt          to   rf-aaq-snx-2qt         .
           move      aaq-dec-2qt          to   rf-aaq-dec-2qt         .
           move      aaq-snx-3qt          to   rf-aaq-snx-3qt         .
           move      aaq-dec-3qt          to   rf-aaq-dec-3qt         .
           move      aaq-snx-2pz          to   rf-aaq-snx-2pz         .
           move      aaq-dec-2pz          to   rf-aaq-dec-2pz         .
           move      aaq-aut-lst          to   rf-aaq-aut-lst         .
           move      aaq-tip-vac          to   rf-aaq-tip-vac         .
           move      aaq-cdp-aqt          to   rf-aaq-cdp-aqt         .
           move      aaq-pdp-aqt (1)      to   rf-aaq-pdp-aqt (1)     .
           move      aaq-pdp-aqt (2)      to   rf-aaq-pdp-aqt (2)     .
           move      aaq-pdp-aqt (3)      to   rf-aaq-pdp-aqt (3)     .
           move      aaq-lot-acq          to   rf-aaq-lot-acq         .
           move      aaq-cla-bdg          to   rf-aaq-cla-bdg         .
           move      aaq-alx-exp          to   rf-aaq-alx-exp         .
       exe-cnv-aaq-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-aaq-ide-dat         .
           move      s-ute                to   rf-aaq-ide-ute         .
           move      s-fas                to   rf-aaq-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Valuta base                                 *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-aaq-sgl-vlt         .
           move      c-dec                to   rf-aaq-dec-vlt         .
      *                  *---------------------------------------------*
      *                  * Prezzo di acquisto di riferimento           *
      *                  *---------------------------------------------*
           move      rf-aaq-prz-acr       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-aaq-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-aaq-prz-acr         .
       exe-cnv-aaq-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-aaq-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-aaq-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-aaq-250.
       exe-cnv-aaq-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     aaq                                              .
       exe-cnv-aaq-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-aaq-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-aaq-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ali]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ali-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ali "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofali"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ali-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-ali-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ali                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-ali-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-ali-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    ali                                       .
       exe-cnv-ali-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   ali-k01                .
           start     ali    key not less
                            ali-k01
                            invalid key
                            go to exe-cnv-ali-800.
       exe-cnv-ali-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      ali    next
                            with no lock
                            at end
                            go to exe-cnv-ali-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ali-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-ali-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ali                 .
       exe-cnv-ali-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ali                 .
           move      ali-ann-fis          to   rf-ali-ann-fis         .
           move      ali-tip-arc          to   rf-ali-tip-arc         .
           move      ali-cod-arc          to   rf-ali-cod-arc         .
           move      ali-cod-iva          to   rf-ali-cod-iva         .
           move      ali-ann-cop          to   rf-ali-ann-cop         .
           move      ali-num-doc          to   rf-ali-num-doc         .
           move      ali-num-dog          to   rf-ali-num-dog         .
           move      ali-ibl-dog          to   rf-ali-ibl-dog         .
           move      ali-iva-dog          to   rf-ali-iva-dog         .
           move      ali-ibl-doc          to   rf-ali-ibl-doc         .
           move      ali-iva-doc          to   rf-ali-iva-doc         .
       exe-cnv-ali-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Imponibile bolle doganali                   *
      *                  *---------------------------------------------*
           move      rf-ali-ibl-dog       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ali-ibl-dog         .
      *                  *---------------------------------------------*
      *                  * Importo bolle doganali                      *
      *                  *---------------------------------------------*
           move      rf-ali-iva-dog       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ali-iva-dog         .
      *                  *---------------------------------------------*
      *                  * Imponibile documenti                        *
      *                  *---------------------------------------------*
           move      rf-ali-ibl-doc       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ali-ibl-doc         .
      *                  *---------------------------------------------*
      *                  * Importo documenti                           *
      *                  *---------------------------------------------*
           move      rf-ali-iva-doc       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ali-iva-doc         .
       exe-cnv-ali-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ali                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ali-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ali-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ali-250.
       exe-cnv-ali-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ali                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     ali                                              .
       exe-cnv-ali-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ali-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-ali-999.
           exit.

      *    *===========================================================*
      *    * Conversione [bfr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bfr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bfr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bfr-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-bfr-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-bfr-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-bfr-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    bfr                                       .
       exe-cnv-bfr-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   bfr-k01                .
           start     bfr    key not less
                            bfr-k01
                            invalid key
                            go to exe-cnv-bfr-800.
       exe-cnv-bfr-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      bfr    next
                            with no lock
                            at end
                            go to exe-cnv-bfr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bfr-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
           if        bfr-sgl-vpf          =    c-sgl
                     go to exe-cnv-bfr-250.
           if        bfr-sgl-vpf          not  = "LIT"
                     go to exe-cnv-bfr-250.
       exe-cnv-bfr-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       exe-cnv-bfr-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bfr                 .
           move      bfr-num-prt          to   rf-bfr-num-prt         .
           move      bfr-num-prg          to   rf-bfr-num-prg         .
           move      bfr-cod-tmb          to   rf-bfr-cod-tmb         .
           move      bfr-cod-dpz          to   rf-bfr-cod-dpz         .
           move      bfr-dat-reg          to   rf-bfr-dat-reg         .
           move      bfr-tip-arc          to   rf-bfr-tip-arc         .
           move      bfr-cod-arc          to   rf-bfr-cod-arc         .
           move      bfr-dpz-arc          to   rf-bfr-dpz-arc         .
           move      bfr-dat-doc          to   rf-bfr-dat-doc         .
           move      bfr-num-doc          to   rf-bfr-num-doc         .
           move      bfr-cod-lng          to   rf-bfr-cod-lng         .
           move      bfr-sgl-vpf          to   rf-bfr-sgl-vpf         .
           move      bfr-dec-vpf          to   rf-bfr-dec-vpf         .
           move      bfr-tdc-vpf          to   rf-bfr-tdc-vpf         .
           move      bfr-cdc-vpf          to   rf-bfr-cdc-vpf         .
           move      bfr-bld-flb          to   rf-bfr-bld-flb         .
           move      bfr-bld-tpb          to   rf-bfr-bld-tpb         .
           move      bfr-bld-rgb          to   rf-bfr-bld-rgb         .
           move      bfr-tip-rig          to   rf-bfr-tip-rig         .
           move      bfr-tip-mag          to   rf-bfr-tip-mag         .
           move      bfr-num-mag          to   rf-bfr-num-mag         .
           move      bfr-alf-mag          to   rf-bfr-alf-mag         .
           move      bfr-sgl-vrn          to   rf-bfr-sgl-vrn         .
           move      bfr-fda-pif          to   rf-bfr-fda-pif         .
           move      bfr-cop-sfn          to   rf-bfr-cop-sfn         .
           move      bfr-snx-tum          to   rf-bfr-snx-tum         .
           move      bfr-umf-tum          to   rf-bfr-umf-tum         .
           move      bfr-nde-tum          to   rf-bfr-nde-tum         .
           move      bfr-cmo-tum          to   rf-bfr-cmo-tum         .
           move      bfr-cdi-tum          to   rf-bfr-cdi-tum         .
           move      bfr-des-ext          to   rf-bfr-des-ext         .
           move      bfr-des-rig          to   rf-bfr-des-rig         .
           move      bfr-tip-pro          to   rf-bfr-tip-pro         .
           move      bfr-cod-iva          to   rf-bfr-cod-iva         .
           move      bfr-ctp-acq          to   rf-bfr-ctp-acq         .
           move      bfr-umi-acq          to   rf-bfr-umi-acq         .
           move      bfr-dec-qta          to   rf-bfr-dec-qta         .
           move      bfr-qta-fda          to   rf-bfr-qta-fda         .
           move      bfr-qta-acq          to   rf-bfr-qta-acq         .
           move      bfr-cod-dsl          to   rf-bfr-cod-dsl         .
           move      bfr-snx-2qt          to   rf-bfr-snx-2qt         .
           move      bfr-dec-2qt          to   rf-bfr-dec-2qt         .
           move      bfr-qta-a02          to   rf-bfr-qta-a02         .
           move      bfr-snx-3qt          to   rf-bfr-snx-3qt         .
           move      bfr-dec-3qt          to   rf-bfr-dec-3qt         .
           move      bfr-qta-a03          to   rf-bfr-qta-a03         .
           move      bfr-dec-prz          to   rf-bfr-dec-prz         .
           move      bfr-sgl-vpp          to   rf-bfr-sgl-vpp         .
           move      bfr-dec-vpp          to   rf-bfr-dec-vpp         .
           move      bfr-tdc-vpp          to   rf-bfr-tdc-vpp         .
           move      bfr-cdc-vpp          to   rf-bfr-cdc-vpp         .
           move      bfr-prz-acq          to   rf-bfr-prz-acq         .
           move      bfr-snx-2pz          to   rf-bfr-snx-2pz         .
           move      bfr-dec-2pz          to   rf-bfr-dec-2pz         .
           move      bfr-prz-a02          to   rf-bfr-prz-a02         .
           move      bfr-sgl-vpl          to   rf-bfr-sgl-vpl         .
           move      bfr-dec-vpl          to   rf-bfr-dec-vpl         .
           move      bfr-tdc-vpl          to   rf-bfr-tdc-vpl         .
           move      bfr-prz-vpl          to   rf-bfr-prz-vpl         .
           move      bfr-cdc-vpl          to   rf-bfr-cdc-vpl         .
           move      bfr-ccr-vpl          to   rf-bfr-ccr-vpl         .
           move      bfr-plm-vpl          to   rf-bfr-plm-vpl         .
           move      bfr-tlm-vpl          to   rf-bfr-tlm-vpl         .
           move      bfr-map-vpl          to   rf-bfr-map-vpl         .
           move      bfr-epz-rgf          to   rf-bfr-epz-rgf         .
           move      bfr-csr-aap          to   rf-bfr-csr-aap         .
           move      bfr-psr-aap (1)      to   rf-bfr-psr-aap (1)     .
           move      bfr-psr-aap (2)      to   rf-bfr-psr-aap (2)     .
           move      bfr-psr-aap (3)      to   rf-bfr-psr-aap (3)     .
           move      bfr-psr-aap (4)      to   rf-bfr-psr-aap (4)     .
           move      bfr-psr-aap (5)      to   rf-bfr-psr-aap (5)     .
           move      bfr-per-scr (1)      to   rf-bfr-per-scr (1)     .
           move      bfr-per-scr (2)      to   rf-bfr-per-scr (2)     .
           move      bfr-per-scr (3)      to   rf-bfr-per-scr (3)     .
           move      bfr-per-scr (4)      to   rf-bfr-per-scr (4)     .
           move      bfr-per-scr (5)      to   rf-bfr-per-scr (5)     .
           move      bfr-prz-net          to   rf-bfr-prz-net         .
           move      bfr-imp-rig          to   rf-bfr-imp-rig         .
           move      bfr-iau-rig          to   rf-bfr-iau-rig         .
           move      bfr-cpv-aap          to   rf-bfr-cpv-aap         .
           move      bfr-ppv-aap (1)      to   rf-bfr-ppv-aap (1)     .
           move      bfr-ppv-aap (2)      to   rf-bfr-ppv-aap (2)     .
           move      bfr-ppv-aap (3)      to   rf-bfr-ppv-aap (3)     .
           move      bfr-fsp-rig          to   rf-bfr-fsp-rig         .
           move      bfr-cpv-rig          to   rf-bfr-cpv-rig         .
           move      bfr-ppv-rig (1)      to   rf-bfr-ppv-rig (1)     .
           move      bfr-ppv-rig (2)      to   rf-bfr-ppv-rig (2)     .
           move      bfr-ppv-rig (3)      to   rf-bfr-ppv-rig (3)     .
           move      bfr-pvf-rig          to   rf-bfr-pvf-rig         .
           move      bfr-rdo-tip          to   rf-bfr-rdo-tip         .
           move      bfr-rdo-dat          to   rf-bfr-rdo-dat         .
           move      bfr-rdo-num          to   rf-bfr-rdo-num         .
           move      bfr-orf-tip          to   rf-bfr-orf-tip         .
           move      bfr-orf-dat          to   rf-bfr-orf-dat         .
           move      bfr-orf-num          to   rf-bfr-orf-num         .
           move      bfr-orf-prt          to   rf-bfr-orf-prt         .
           move      bfr-orf-prg          to   rf-bfr-orf-prg         .
           move      bfr-orf-fzs          to   rf-bfr-orf-fzs         .
           move      bfr-orm-prt          to   rf-bfr-orm-prt         .
           move      bfr-flg-rch          to   rf-bfr-flg-rch         .
           move      bfr-flg-blx (1)      to   rf-bfr-flg-blx (1)     .
           move      bfr-flg-blx (2)      to   rf-bfr-flg-blx (2)     .
           move      bfr-flg-blx (3)      to   rf-bfr-flg-blx (3)     .
           move      bfr-flg-blx (4)      to   rf-bfr-flg-blx (4)     .
           move      bfr-flg-blx (5)      to   rf-bfr-flg-blx (5)     .
           move      bfr-flg-blx (6)      to   rf-bfr-flg-blx (6)     .
           move      bfr-flg-blx (7)      to   rf-bfr-flg-blx (7)     .
           move      bfr-flg-nbx (1)      to   rf-bfr-flg-nbx (1)     .
           move      bfr-flg-nbx (2)      to   rf-bfr-flg-nbx (2)     .
           move      bfr-flg-nbx (3)      to   rf-bfr-flg-nbx (3)     .
           move      bfr-flg-pul          to   rf-bfr-flg-pul         .
           move      bfr-alx-exp          to   rf-bfr-alx-exp         .
       exe-cnv-bfr-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bfr-sgl-vpf         .
           move      c-dec                to   rf-bfr-dec-vpf         .
           move      c-tdc                to   rf-bfr-tdc-vpf         .
           move      c-cdc                to   rf-bfr-cdc-vpf         .
      *                  *---------------------------------------------*
      *                  * Valuta per il prezzo                        *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bfr-sgl-vpp         .
           move      c-dec                to   rf-bfr-dec-vpp         .
           move      c-tdc                to   rf-bfr-tdc-vpp         .
           move      c-cdc                to   rf-bfr-cdc-vpp         .
      *                  *---------------------------------------------*
      *                  * Prezzo unitario                             *
      *                  *---------------------------------------------*
           move      rf-bfr-prz-acq       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bfr-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bfr-prz-acq         .
      *                  *---------------------------------------------*
      *                  * 2. Prezzo unitario                          *
      *                  *---------------------------------------------*
           move      rf-bfr-prz-a02       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bfr-dec-2pz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bfr-prz-a02         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto                                *
      *                  *---------------------------------------------*
           move      rf-bfr-prz-net       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bfr-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bfr-prz-net         .
      *                  *---------------------------------------------*
      *                  * Importo                                     *
      *                  *---------------------------------------------*
           move      rf-bfr-imp-rig       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bfr-imp-rig         .
      *                  *---------------------------------------------*
      *                  * Importo ausiliario                          *
      *                  *---------------------------------------------*
           move      rf-bfr-iau-rig       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bfr-iau-rig         .
       exe-cnv-bfr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bfr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bfr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bfr-250.
       exe-cnv-bfr-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     bfr                                              .
       exe-cnv-bfr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bfr-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-bfr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [bft]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bft-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bft "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bft-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-bft-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-bft-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-bft-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    bft                                       .
       exe-cnv-bft-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   bft-k01                .
           start     bft    key not less
                            bft-k01
                            invalid key
                            go to exe-cnv-bft-800.
       exe-cnv-bft-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      bft    next
                            with no lock
                            at end
                            go to exe-cnv-bft-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bft-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
           if        bft-sgl-vpf          =    c-sgl
                     go to exe-cnv-bft-250.
           if        bft-sgl-vpf          not  = "LIT"
                     go to exe-cnv-bft-250.
       exe-cnv-bft-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       exe-cnv-bft-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bft                 .
           move      bft-ide-dat          to   rf-bft-ide-dat         .
           move      bft-ide-ute          to   rf-bft-ide-ute         .
           move      bft-ide-fas          to   rf-bft-ide-fas         .
           move      bft-num-prt          to   rf-bft-num-prt         .
           move      bft-cod-tmb          to   rf-bft-cod-tmb         .
           move      bft-prt-ftf          to   rf-bft-prt-ftf         .
           move      bft-prt-mag          to   rf-bft-prt-mag         .
           move      bft-pr2-mag          to   rf-bft-pr2-mag         .
           move      bft-int-ftr          to   rf-bft-int-ftr         .
           move      bft-tmo-ftr          to   rf-bft-tmo-ftr         .
           move      bft-cod-dpz          to   rf-bft-cod-dpz         .
           move      bft-dat-reg          to   rf-bft-dat-reg         .
           move      bft-tip-arc          to   rf-bft-tip-arc         .
           move      bft-cod-arc          to   rf-bft-cod-arc         .
           move      bft-dpz-arc          to   rf-bft-dpz-arc         .
           move      bft-arc-plf          to   rf-bft-arc-plf         .
           move      bft-dpz-plf          to   rf-bft-dpz-plf         .
           move      bft-tip-ids          to   rf-bft-tip-ids         .
           move      bft-dat-doc          to   rf-bft-dat-doc         .
           move      bft-num-doc          to   rf-bft-num-doc         .
           move      bft-cod-lng          to   rf-bft-cod-lng         .
           move      bft-sgl-vpf          to   rf-bft-sgl-vpf         .
           move      bft-dec-vpf          to   rf-bft-dec-vpf         .
           move      bft-tdc-vpf          to   rf-bft-tdc-vpf         .
           move      bft-cdc-vpf          to   rf-bft-cdc-vpf         .
           move      bft-ass-iva          to   rf-bft-ass-iva         .
           move      bft-ctp-acq          to   rf-bft-ctp-acq         .
           move      bft-inl-pgt          to   rf-bft-inl-pgt         .
           move      bft-cod-lst          to   rf-bft-cod-lst         .
           move      bft-csr-aaf          to   rf-bft-csr-aaf         .
           move      bft-psr-aaf (1)      to   rf-bft-psr-aaf (1)     .
           move      bft-psr-aaf (2)      to   rf-bft-psr-aaf (2)     .
           move      bft-psr-aaf (3)      to   rf-bft-psr-aaf (3)     .
           move      bft-psr-aaf (4)      to   rf-bft-psr-aaf (4)     .
           move      bft-psr-aaf (5)      to   rf-bft-psr-aaf (5)     .
           move      bft-csc-aaf          to   rf-bft-csc-aaf         .
           move      bft-psc-aaf          to   rf-bft-psc-aaf         .
           move      bft-voc-des (1)      to   rf-bft-voc-des (1)     .
           move      bft-voc-des (2)      to   rf-bft-voc-des (2)     .
           move      bft-voc-des (3)      to   rf-bft-voc-des (3)     .
           move      bft-voc-des (4)      to   rf-bft-voc-des (4)     .
           move      bft-voc-des (5)      to   rf-bft-voc-des (5)     .
           move      bft-voc-des (6)      to   rf-bft-voc-des (6)     .
           move      bft-cod-fop          to   rf-bft-cod-fop         .
           move      bft-scp-aap          to   rf-bft-scp-aap         .
           move      bft-nos-ban          to   rf-bft-nos-ban         .
           move      bft-cod-abi          to   rf-bft-cod-abi         .
           move      bft-cod-cab          to   rf-bft-cod-cab         .
           move      bft-ccc-app          to   rf-bft-ccc-app         .
           move      bft-ccp-app          to   rf-bft-ccp-app         .
           move      bft-add-spi          to   rf-bft-add-spi         .
           move      bft-add-spb          to   rf-bft-add-spb         .
           move      bft-ipr-iel          to   rf-bft-ipr-iel         .
           move      bft-pag-dsm          to   rf-bft-pag-dsm         .
           move      bft-pag-qaf          to   rf-bft-pag-qaf         .
           move      bft-pag-act          to   rf-bft-pag-act         .
           move      bft-cod-aqt          to   rf-bft-cod-aqt         .
           move      bft-pvf-aqt          to   rf-bft-pvf-aqt         .
           move      bft-cod-ime          to   rf-bft-cod-ime         .
           move      bft-pvf-ime          to   rf-bft-pvf-ime         .
           move      bft-tot-rig (1)      to   rf-bft-tot-rig (1)     .
           move      bft-tot-rig (2)      to   rf-bft-tot-rig (2)     .
           move      bft-tot-rig (3)      to   rf-bft-tot-rig (3)     .
           move      bft-tot-rig (4)      to   rf-bft-tot-rig (4)     .
           move      bft-tot-rig (5)      to   rf-bft-tot-rig (5)     .
           move      bft-tot-rig (6)      to   rf-bft-tot-rig (6)     .
           move      bft-tot-rig (7)      to   rf-bft-tot-rig (7)     .
           move      bft-tot-rig (8)      to   rf-bft-tot-rig (8)     .
           move      bft-tot-rig (9)      to   rf-bft-tot-rig (9)     .
           move      bft-tot-scc          to   rf-bft-tot-scc         .
           move      bft-per-scc          to   rf-bft-per-scc         .
           move      bft-civ-scc          to   rf-bft-civ-scc         .
           move      bft-ccp-scc          to   rf-bft-ccp-scc         .
           move      bft-tot-scp          to   rf-bft-tot-scp         .
           move      bft-per-scp          to   rf-bft-per-scp         .
           move      bft-civ-scp          to   rf-bft-civ-scp         .
           move      bft-ccp-scp          to   rf-bft-ccp-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-bft-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-440.
           move      bft-spe-snx (w-c01)  to   rf-bft-spe-snx (w-c01) .
           move      bft-spe-mad (w-c01)  to   rf-bft-spe-mad (w-c01) .
           move      bft-spe-per (w-c01)  to   rf-bft-spe-per (w-c01) .
           move      bft-spe-ibl (w-c01)  to   rf-bft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-bft-420.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-bft-430.
           move      bft-ibx-spe
                    (w-c01, w-c02)        to   rf-bft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-bft-420.
       exe-cnv-bft-430.
           move      bft-spe-imp (w-c01)  to   rf-bft-spe-imp (w-c01) .
           move      bft-spe-civ (w-c01)  to   rf-bft-spe-civ (w-c01) .
           move      bft-spe-ccp (w-c01)  to   rf-bft-spe-ccp (w-c01) .
           go to     exe-cnv-bft-410.
       exe-cnv-bft-440.
           move      bft-civ-spi          to   rf-bft-civ-spi         .
           move      bft-ccp-spi          to   rf-bft-ccp-spi         .
           move      bft-tot-sic          to   rf-bft-tot-sic         .
           move      bft-tot-sia          to   rf-bft-tot-sia         .
           move      bft-tot-spb          to   rf-bft-tot-spb         .
           move      bft-civ-spb          to   rf-bft-civ-spb         .
           move      bft-ccp-spb          to   rf-bft-ccp-spb         .
           move      bft-prt-mgd          to   rf-bft-prt-mgd         .
           move      bft-nrg-mgd          to   rf-bft-nrg-mgd         .
           move      bft-dri-mgd          to   rf-bft-dri-mgd         .
           move      bft-nri-mgd          to   rf-bft-nri-mgd         .
           move      bft-nps-sdb          to   rf-bft-nps-sdb         .
           move      bft-ctr-sdb          to   rf-bft-ctr-sdb         .
           move      zero                 to   w-c01                  .
       exe-cnv-bft-450.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-460.
           move      bft-iva-cod (w-c01)  to   rf-bft-iva-cod (w-c01) .
           move      bft-iva-ibl (w-c01)  to   rf-bft-iva-ibl (w-c01) .
           go to     exe-cnv-bft-450.
       exe-cnv-bft-460.
           move      zero                 to   w-c01                  .
       exe-cnv-bft-470.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bft-480.
           move      bft-ctp-cod (w-c01)  to   rf-bft-ctp-cod (w-c01) .
           move      bft-ctp-imp (w-c01)  to   rf-bft-ctp-imp (w-c01) .
           go to     exe-cnv-bft-470.
       exe-cnv-bft-480.
           move      bft-flg-bch          to   rf-bft-flg-bch         .
           move      bft-flg-blx (1)      to   rf-bft-flg-blx (1)     .
           move      bft-flg-blx (2)      to   rf-bft-flg-blx (2)     .
           move      bft-flg-blx (3)      to   rf-bft-flg-blx (3)     .
           move      bft-flg-blx (4)      to   rf-bft-flg-blx (4)     .
           move      bft-flg-blx (5)      to   rf-bft-flg-blx (5)     .
           move      bft-flg-blx (6)      to   rf-bft-flg-blx (6)     .
           move      bft-flg-blx (7)      to   rf-bft-flg-blx (7)     .
           move      bft-flg-nbx (1)      to   rf-bft-flg-nbx (1)     .
           move      bft-flg-nbx (2)      to   rf-bft-flg-nbx (2)     .
           move      bft-flg-nbx (3)      to   rf-bft-flg-nbx (3)     .
           move      bft-flg-pul          to   rf-bft-flg-pul         .
           move      bft-alx-exp          to   rf-bft-alx-exp         .
       exe-cnv-bft-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-bft-ide-dat         .
           move      s-ute                to   rf-bft-ide-ute         .
           move      s-fas                to   rf-bft-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bft-sgl-vpf         .
           move      c-dec                to   rf-bft-dec-vpf         .
           move      c-tdc                to   rf-bft-tdc-vpf         .
           move      c-cdc                to   rf-bft-cdc-vpf         .
      *                  *---------------------------------------------*
      *                  * Quota a forfait                             *
      *                  *---------------------------------------------*
           move      rf-bft-pag-qaf       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-pag-qaf         .
      *                  *---------------------------------------------*
      *                  * Acconto                                     *
      *                  *---------------------------------------------*
           move      rf-bft-pag-act       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-pag-act         .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bft-510.
           add       1                    to   w-c01                  .
           if        w-c01                >    9
                     go to exe-cnv-bft-520.
      *
           move      rf-bft-tot-rig
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-tot-rig
                                              (w-c01)                 .
      *
           go to     exe-cnv-bft-510.
       exe-cnv-bft-520.
      *                  *---------------------------------------------*
      *                  * Importo sconto in chiusura                  *
      *                  *---------------------------------------------*
           move      rf-bft-tot-scc       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-tot-scc         .
      *                  *---------------------------------------------*
      *                  * Importo sconto pagamento                    *
      *                  *---------------------------------------------*
           move      rf-bft-tot-scp       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-tot-scp         .
      *                  *---------------------------------------------*
      *                  * Importo di addebito della spesa             *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bft-530.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-540.
      *
           move      rf-bft-spe-imp
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-spe-imp
                                              (w-c01)                 .
      *
           go to     exe-cnv-bft-530.
       exe-cnv-bft-540.
      *                  *---------------------------------------------*
      *                  * Importo spese incasso                       *
      *                  *---------------------------------------------*
           move      rf-bft-tot-sic       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-tot-sic         .
      *                  *---------------------------------------------*
      *                  * Importo spese incasso aggiuntive            *
      *                  *---------------------------------------------*
           move      rf-bft-tot-sia       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-tot-sia         .
      *                  *---------------------------------------------*
      *                  * Importo spese bollo                         *
      *                  *---------------------------------------------*
           move      rf-bft-tot-spb       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-tot-spb         .
      *                  *---------------------------------------------*
      *                  * Castelletto iva                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bft-550.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-560.
      *
           move      rf-bft-iva-ibl
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-iva-ibl
                                              (w-c01)                 .
      *
           go to     exe-cnv-bft-550.
       exe-cnv-bft-560.
      *                  *---------------------------------------------*
      *                  * Castelletto contropartite                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bft-570.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bft-700.
      *
           move      rf-bft-ctp-imp
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bft-ctp-imp
                                              (w-c01)                 .
      *
           go to     exe-cnv-bft-570.
       exe-cnv-bft-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bft-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bft-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bft-250.
       exe-cnv-bft-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     bft                                              .
       exe-cnv-bft-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bft-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-bft-999.
           exit.

      *    *===========================================================*
      *    * Conversione [bir]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bir-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bir "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bir-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-bir-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-bir-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-bir-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    bir                                       .
       exe-cnv-bir-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   bir-k01                .
           start     bir    key not less
                            bir-k01
                            invalid key
                            go to exe-cnv-bir-800.
       exe-cnv-bir-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      bir    next
                            with no lock
                            at end
                            go to exe-cnv-bir-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bir-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
           if        bir-sgl-vpf          =    c-sgl
                     go to exe-cnv-bir-250.
           if        bir-sgl-vpf          not  = "LIT"
                     go to exe-cnv-bir-250.
       exe-cnv-bir-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       exe-cnv-bir-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bir                 .
           move      bir-num-prt          to   rf-bir-num-prt         .
           move      bir-num-prg          to   rf-bir-num-prg         .
           move      bir-cod-tmb          to   rf-bir-cod-tmb         .
           move      bir-cod-dpz          to   rf-bir-cod-dpz         .
           move      bir-dat-doc          to   rf-bir-dat-doc         .
           move      bir-num-doc          to   rf-bir-num-doc         .
           move      bir-tip-arc          to   rf-bir-tip-arc         .
           move      bir-cod-arc          to   rf-bir-cod-arc         .
           move      bir-dpz-arc          to   rf-bir-dpz-arc         .
           move      bir-cod-lng          to   rf-bir-cod-lng         .
           move      bir-sgl-vpf          to   rf-bir-sgl-vpf         .
           move      bir-dec-vpf          to   rf-bir-dec-vpf         .
           move      bir-tdc-vpf          to   rf-bir-tdc-vpf         .
           move      bir-cdc-vpf          to   rf-bir-cdc-vpf         .
           move      bir-bld-flb          to   rf-bir-bld-flb         .
           move      bir-bld-tpb          to   rf-bir-bld-tpb         .
           move      bir-bld-rgb          to   rf-bir-bld-rgb         .
           move      bir-tip-rig          to   rf-bir-tip-rig         .
           move      bir-tip-mag          to   rf-bir-tip-mag         .
           move      bir-num-pro          to   rf-bir-num-pro         .
           move      bir-alf-pro          to   rf-bir-alf-pro         .
           move      bir-sgl-vrn          to   rf-bir-sgl-vrn         .
           move      bir-cop-scl          to   rf-bir-cop-scl         .
           move      bir-des-ext          to   rf-bir-des-ext         .
           move      bir-des-rig          to   rf-bir-des-rig         .
           move      bir-tip-pro          to   rf-bir-tip-pro         .
           move      bir-cod-iva          to   rf-bir-cod-iva         .
           move      bir-ctp-ven          to   rf-bir-ctp-ven         .
           move      bir-umi-ven          to   rf-bir-umi-ven         .
           move      bir-dec-qta          to   rf-bir-dec-qta         .
           move      bir-qta-ven          to   rf-bir-qta-ven         .
           move      bir-cod-dsl          to   rf-bir-cod-dsl         .
           move      bir-snx-2qt          to   rf-bir-snx-2qt         .
           move      bir-dec-2qt          to   rf-bir-dec-2qt         .
           move      bir-qta-a02          to   rf-bir-qta-a02         .
           move      bir-snx-3qt          to   rf-bir-snx-3qt         .
           move      bir-dec-3qt          to   rf-bir-dec-3qt         .
           move      bir-qta-a03          to   rf-bir-qta-a03         .
           move      bir-dec-prz          to   rf-bir-dec-prz         .
           move      bir-sgl-vps          to   rf-bir-sgl-vps         .
           move      bir-dec-vps          to   rf-bir-dec-vps         .
           move      bir-tdc-vps          to   rf-bir-tdc-vps         .
           move      bir-cdc-vps          to   rf-bir-cdc-vps         .
           move      bir-prz-lrs          to   rf-bir-prz-lrs         .
           move      bir-prz-nts          to   rf-bir-prz-nts         .
           move      bir-sgl-vpp          to   rf-bir-sgl-vpp         .
           move      bir-dec-vpp          to   rf-bir-dec-vpp         .
           move      bir-tdc-vpp          to   rf-bir-tdc-vpp         .
           move      bir-cdc-vpp          to   rf-bir-cdc-vpp         .
           move      bir-prz-ven          to   rf-bir-prz-ven         .
           move      bir-snx-2pz          to   rf-bir-snx-2pz         .
           move      bir-prz-a02          to   rf-bir-prz-a02         .
           move      bir-sgl-vpl          to   rf-bir-sgl-vpl         .
           move      bir-dec-vpl          to   rf-bir-dec-vpl         .
           move      bir-tdc-vpl          to   rf-bir-tdc-vpl         .
           move      bir-prz-vpl          to   rf-bir-prz-vpl         .
           move      bir-cdc-vpl          to   rf-bir-cdc-vpl         .
           move      bir-ccr-vpl          to   rf-bir-ccr-vpl         .
           move      bir-plm-vpl          to   rf-bir-plm-vpl         .
           move      bir-tlm-vpl          to   rf-bir-tlm-vpl         .
           move      bir-map-vpl          to   rf-bir-map-vpl         .
           move      bir-epz-rgf          to   rf-bir-epz-rgf         .
           move      bir-csr-aap          to   rf-bir-csr-aap         .
           move      bir-psr-aap (1)      to   rf-bir-psr-aap (1)     .
           move      bir-psr-aap (2)      to   rf-bir-psr-aap (2)     .
           move      bir-psr-aap (3)      to   rf-bir-psr-aap (3)     .
           move      bir-psr-aap (4)      to   rf-bir-psr-aap (4)     .
           move      bir-psr-aap (5)      to   rf-bir-psr-aap (5)     .
           move      bir-per-scr (1)      to   rf-bir-per-scr (1)     .
           move      bir-per-scr (2)      to   rf-bir-per-scr (2)     .
           move      bir-per-scr (3)      to   rf-bir-per-scr (3)     .
           move      bir-per-scr (4)      to   rf-bir-per-scr (4)     .
           move      bir-per-scr (5)      to   rf-bir-per-scr (5)     .
           move      bir-prz-net          to   rf-bir-prz-net         .
           move      bir-epz-pes          to   rf-bir-epz-pes         .
           move      bir-sgl-vpc          to   rf-bir-sgl-vpc         .
           move      bir-dec-vpc          to   rf-bir-dec-vpc         .
           move      bir-tdc-vpc          to   rf-bir-tdc-vpc         .
           move      bir-cdc-vpc          to   rf-bir-cdc-vpc         .
           move      bir-dec-cos          to   rf-bir-dec-cos         .
           move      bir-cos-rif          to   rf-bir-cos-rif         .
           move      bir-imp-rig          to   rf-bir-imp-rig         .
           move      bir-iau-rig          to   rf-bir-iau-rig         .
           move      bir-cpv-aap          to   rf-bir-cpv-aap         .
           move      bir-ppv-aap (1)      to   rf-bir-ppv-aap (1)     .
           move      bir-ppv-aap (2)      to   rf-bir-ppv-aap (2)     .
           move      bir-ppv-aap (3)      to   rf-bir-ppv-aap (3)     .
           move      bir-fsp-rig          to   rf-bir-fsp-rig         .
           move      bir-cpv-rig          to   rf-bir-cpv-rig         .
           move      bir-ppv-rig (1)      to   rf-bir-ppv-rig (1)     .
           move      bir-ppv-rig (2)      to   rf-bir-ppv-rig (2)     .
           move      bir-ppv-rig (3)      to   rf-bir-ppv-rig (3)     .
           move      bir-pvf-rig          to   rf-bir-pvf-rig         .
           move      bir-ocl-dat          to   rf-bir-ocl-dat         .
           move      bir-ocl-num          to   rf-bir-ocl-num         .
           move      bir-cmc-tip          to   rf-bir-cmc-tip         .
           move      bir-cmc-dat          to   rf-bir-cmc-dat         .
           move      bir-cmc-num          to   rf-bir-cmc-num         .
           move      bir-coc-tip          to   rf-bir-coc-tip         .
           move      bir-coc-dat          to   rf-bir-coc-dat         .
           move      bir-coc-num          to   rf-bir-coc-num         .
           move      bir-coc-prt          to   rf-bir-coc-prt         .
           move      bir-coc-prg          to   rf-bir-coc-prg         .
           move      bir-coc-fzs          to   rf-bir-coc-fzs         .
           move      bir-ods-prt          to   rf-bir-ods-prt         .
           move      bir-ods-prg          to   rf-bir-ods-prg         .
           move      bir-fat-snx          to   rf-bir-fat-snx         .
           move      bir-fat-dat          to   rf-bir-fat-dat         .
           move      bir-fat-num          to   rf-bir-fat-num         .
           move      bir-fat-npb          to   rf-bir-fat-npb         .
           move      bir-flg-blx (1)      to   rf-bir-flg-blx (1)     .
           move      bir-flg-blx (2)      to   rf-bir-flg-blx (2)     .
           move      bir-flg-blx (3)      to   rf-bir-flg-blx (3)     .
           move      bir-flg-blx (4)      to   rf-bir-flg-blx (4)     .
           move      bir-flg-blx (5)      to   rf-bir-flg-blx (5)     .
           move      bir-flg-blx (6)      to   rf-bir-flg-blx (6)     .
           move      bir-flg-blx (7)      to   rf-bir-flg-blx (7)     .
           move      bir-flg-nbx (1)      to   rf-bir-flg-nbx (1)     .
           move      bir-flg-nbx (2)      to   rf-bir-flg-nbx (2)     .
           move      bir-flg-nbx (3)      to   rf-bir-flg-nbx (3)     .
           move      bir-flg-pul          to   rf-bir-flg-pul         .
           move      bir-flg-puq          to   rf-bir-flg-puq         .
           move      bir-alx-exp          to   rf-bir-alx-exp         .
       exe-cnv-bir-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bir-sgl-vpf         .
           move      c-dec                to   rf-bir-dec-vpf         .
           move      c-tdc                to   rf-bir-tdc-vpf         .
           move      c-cdc                to   rf-bir-cdc-vpf         .
      *                  *---------------------------------------------*
      *                  * Valuta per l'espressione prezzi standard    *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bir-sgl-vps         .
           move      c-dec                to   rf-bir-dec-vps         .
           move      c-tdc                to   rf-bir-tdc-vps         .
           move      c-cdc                to   rf-bir-cdc-vps         .
      *                  *---------------------------------------------*
      *                  * Prezzo lordo standard                       *
      *                  *---------------------------------------------*
           move      rf-bir-prz-lrs       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bir-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-prz-lrs         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto standard                       *
      *                  *---------------------------------------------*
           move      rf-bir-prz-nts       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bir-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-prz-nts         .
      *                  *---------------------------------------------*
      *                  * Valuta per il prezzo                        *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bir-sgl-vpp         .
           move      c-dec                to   rf-bir-dec-vpp         .
           move      c-tdc                to   rf-bir-tdc-vpp         .
           move      c-cdc                to   rf-bir-cdc-vpp         .
      *                  *---------------------------------------------*
      *                  * Prezzo unitario                             *
      *                  *---------------------------------------------*
           move      rf-bir-prz-ven       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bir-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-prz-ven         .
      *                  *---------------------------------------------*
      *                  * 2. Prezzo unitario                          *
      *                  *---------------------------------------------*
           move      rf-bir-prz-a02       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bir-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-prz-a02         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto                                *
      *                  *---------------------------------------------*
           move      rf-bir-prz-net       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      rf-bir-dec-prz       to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-prz-net         .
      *                  *---------------------------------------------*
      *                  * Importo                                     *
      *                  *---------------------------------------------*
           move      rf-bir-imp-rig       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-imp-rig         .
      *                  *---------------------------------------------*
      *                  * Importo ausiliario                          *
      *                  *---------------------------------------------*
           move      rf-bir-iau-rig       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-iau-rig         .
      *                  *---------------------------------------------*
      *                  * Importo provvigione a forfait               *
      *                  *---------------------------------------------*
           move      rf-bir-pvf-rig       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bir-pvf-rig         .
       exe-cnv-bir-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bir-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bir-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bir-250.
       exe-cnv-bir-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     bir                                              .
       exe-cnv-bir-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bir-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-bir-999.
           exit.

      *    *===========================================================*
      *    * Conversione [bit]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bit-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bit "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bit-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-bit-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-bit-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-bit-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    bit                                       .
       exe-cnv-bit-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   bit-k01                .
           start     bit    key not less
                            bit-k01
                            invalid key
                            go to exe-cnv-bit-800.
       exe-cnv-bit-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      bit    next
                            with no lock
                            at end
                            go to exe-cnv-bit-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bit-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
           if        bit-sgl-vpf          =    c-sgl
                     go to exe-cnv-bit-250.
           if        bit-sgl-vpf          not  = "LIT"
                     go to exe-cnv-bit-250.
       exe-cnv-bit-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
       exe-cnv-bit-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bit                 .
           move      bit-ide-dat          to   rf-bit-ide-dat         .
           move      bit-ide-ute          to   rf-bit-ide-ute         .
           move      bit-ide-fas          to   rf-bit-ide-fas         .
           move      bit-num-prt          to   rf-bit-num-prt         .
           move      bit-cod-tmb          to   rf-bit-cod-tmb         .
           move      bit-prt-fti          to   rf-bit-prt-fti         .
           move      bit-prt-mag          to   rf-bit-prt-mag         .
           move      bit-pr2-mag          to   rf-bit-pr2-mag         .
           move      bit-int-ftr          to   rf-bit-int-ftr         .
           move      bit-tmo-ftr          to   rf-bit-tmo-ftr         .
           move      bit-fat-snx          to   rf-bit-fat-snx         .
           move      bit-fat-dat          to   rf-bit-fat-dat         .
           move      bit-fat-num          to   rf-bit-fat-num         .
           move      bit-fat-npb          to   rf-bit-fat-npb         .
           move      bit-snx-acm          to   rf-bit-snx-acm         .
           move      bit-cod-dpz          to   rf-bit-cod-dpz         .
           move      bit-dat-doc          to   rf-bit-dat-doc         .
           move      bit-num-doc          to   rf-bit-num-doc         .
           move      bit-snx-fac          to   rf-bit-snx-fac         .
           move      bit-scl-ann          to   rf-bit-scl-ann         .
           move      bit-sgl-num          to   rf-bit-sgl-num         .
           move      bit-tip-arc          to   rf-bit-tip-arc         .
           move      bit-cod-arc          to   rf-bit-cod-arc         .
           move      bit-dpz-arc          to   rf-bit-dpz-arc         .
           move      bit-tip-frn          to   rf-bit-tip-frn         .
           move      bit-arc-plf          to   rf-bit-arc-plf         .
           move      bit-dpz-plf          to   rf-bit-dpz-plf         .
           move      bit-tip-ftz          to   rf-bit-tip-ftz         .
           move      bit-tip-ids          to   rf-bit-tip-ids         .
           move      bit-cod-lng          to   rf-bit-cod-lng         .
           move      bit-sgl-vpf          to   rf-bit-sgl-vpf         .
           move      bit-dec-vpf          to   rf-bit-dec-vpf         .
           move      bit-tdc-vpf          to   rf-bit-tdc-vpf         .
           move      bit-cdc-vpf          to   rf-bit-cdc-vpf         .
           move      bit-ass-iva          to   rf-bit-ass-iva         .
           move      bit-ctp-ven          to   rf-bit-ctp-ven         .
           move      bit-fat-sep          to   rf-bit-fat-sep         .
           move      bit-inl-dcm          to   rf-bit-inl-dcm         .
           move      bit-inl-pgt          to   rf-bit-inl-pgt         .
           move      bit-cod-lst          to   rf-bit-cod-lst         .
           move      bit-csr-aac          to   rf-bit-csr-aac         .
           move      bit-psr-aac (1)      to   rf-bit-psr-aac (1)     .
           move      bit-psr-aac (2)      to   rf-bit-psr-aac (2)     .
           move      bit-psr-aac (3)      to   rf-bit-psr-aac (3)     .
           move      bit-psr-aac (4)      to   rf-bit-psr-aac (4)     .
           move      bit-psr-aac (5)      to   rf-bit-psr-aac (5)     .
           move      bit-csc-aac          to   rf-bit-csc-aac         .
           move      bit-psc-aac          to   rf-bit-psc-aac         .
           move      bit-cpv-aac          to   rf-bit-cpv-aac         .
           move      bit-ppv-aac (1)      to   rf-bit-ppv-aac (1)     .
           move      bit-ppv-aac (2)      to   rf-bit-ppv-aac (2)     .
           move      bit-ppv-aac (3)      to   rf-bit-ppv-aac (3)     .
           move      bit-voc-des (1)      to   rf-bit-voc-des (1)     .
           move      bit-voc-des (2)      to   rf-bit-voc-des (2)     .
           move      bit-voc-des (3)      to   rf-bit-voc-des (3)     .
           move      bit-voc-des (4)      to   rf-bit-voc-des (4)     .
           move      bit-voc-des (5)      to   rf-bit-voc-des (5)     .
           move      bit-voc-des (6)      to   rf-bit-voc-des (6)     .
           move      bit-cod-fop          to   rf-bit-cod-fop         .
           move      bit-scp-aap          to   rf-bit-scp-aap         .
           move      bit-cod-abi          to   rf-bit-cod-abi         .
           move      bit-cod-cab          to   rf-bit-cod-cab         .
           move      bit-ccc-app          to   rf-bit-ccc-app         .
           move      bit-nos-ban          to   rf-bit-nos-ban         .
           move      bit-nos-ccp          to   rf-bit-nos-ccp         .
           move      bit-add-spi          to   rf-bit-add-spi         .
           move      bit-add-spb          to   rf-bit-add-spb         .
           move      bit-ipr-iel          to   rf-bit-ipr-iel         .
           move      bit-pag-dsm          to   rf-bit-pag-dsm         .
           move      bit-pag-qaf          to   rf-bit-pag-qaf         .
           move      bit-pag-act          to   rf-bit-pag-act         .
           move      bit-cod-age          to   rf-bit-cod-age         .
           move      bit-fsp-doc          to   rf-bit-fsp-doc         .
           move      bit-pvf-age          to   rf-bit-pvf-age         .
           move      bit-tip-vpa          to   rf-bit-tip-vpa         .
           move      bit-cpv-aaa          to   rf-bit-cpv-aaa         .
           move      bit-ppv-aaa (1)      to   rf-bit-ppv-aaa (1)     .
           move      bit-ppv-aaa (2)      to   rf-bit-ppv-aaa (2)     .
           move      bit-ppv-aaa (3)      to   rf-bit-ppv-aaa (3)     .
           move      bit-cod-ime          to   rf-bit-cod-ime         .
           move      bit-pvf-ime          to   rf-bit-pvf-ime         .
           move      bit-tot-rig (1)      to   rf-bit-tot-rig (1)     .
           move      bit-tot-rig (2)      to   rf-bit-tot-rig (2)     .
           move      bit-tot-rig (3)      to   rf-bit-tot-rig (3)     .
           move      bit-tot-rig (4)      to   rf-bit-tot-rig (4)     .
           move      bit-tot-rig (5)      to   rf-bit-tot-rig (5)     .
           move      bit-tot-rig (6)      to   rf-bit-tot-rig (6)     .
           move      bit-tot-rig (7)      to   rf-bit-tot-rig (7)     .
           move      bit-tot-rig (8)      to   rf-bit-tot-rig (8)     .
           move      bit-tot-rig (9)      to   rf-bit-tot-rig (9)     .
           move      bit-tot-scc          to   rf-bit-tot-scc         .
           move      bit-per-scc          to   rf-bit-per-scc         .
           move      bit-civ-scc          to   rf-bit-civ-scc         .
           move      bit-ccp-scc          to   rf-bit-ccp-scc         .
           move      bit-tot-scp          to   rf-bit-tot-scp         .
           move      bit-per-scp          to   rf-bit-per-scp         .
           move      bit-civ-scp          to   rf-bit-civ-scp         .
           move      bit-ccp-scp          to   rf-bit-ccp-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-bit-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bit-440.
           move      bit-spe-snx (w-c01)  to   rf-bit-spe-snx (w-c01) .
           move      bit-spe-mad (w-c01)  to   rf-bit-spe-mad (w-c01) .
           move      bit-spe-per (w-c01)  to   rf-bit-spe-per (w-c01) .
           move      bit-spe-ibl (w-c01)  to   rf-bit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-bit-420.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-bit-430.
           move      bit-ibx-spe
                    (w-c01, w-c02)        to   rf-bit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-bit-420.
       exe-cnv-bit-430.
           move      bit-spe-imp (w-c01)  to   rf-bit-spe-imp (w-c01) .
           move      bit-spe-civ (w-c01)  to   rf-bit-spe-civ (w-c01) .
           move      bit-spe-ccp (w-c01)  to   rf-bit-spe-ccp (w-c01) .
           go to     exe-cnv-bit-410.
       exe-cnv-bit-440.
           move      bit-civ-spi          to   rf-bit-civ-spi         .
           move      bit-ccp-spi          to   rf-bit-ccp-spi         .
           move      bit-tot-sic          to   rf-bit-tot-sic         .
           move      bit-tot-sia          to   rf-bit-tot-sia         .
           move      bit-tot-spb          to   rf-bit-tot-spb         .
           move      bit-civ-spb          to   rf-bit-civ-spb         .
           move      bit-ccp-spb          to   rf-bit-ccp-spb         .
           move      bit-tra-cur          to   rf-bit-tra-cur         .
           move      bit-cau-tra          to   rf-bit-cau-tra         .
           move      bit-asp-ben          to   rf-bit-asp-ben         .
           move      bit-num-col          to   rf-bit-num-col         .
           move      bit-pes-tot          to   rf-bit-pes-tot         .
           move      bit-dat-itr          to   rf-bit-dat-itr         .
           move      bit-ora-itr          to   rf-bit-ora-itr         .
           move      bit-cod-vet          to   rf-bit-cod-vet         .
           move      bit-cod-vt2          to   rf-bit-cod-vt2         .
           move      bit-cod-vt3          to   rf-bit-cod-vt3         .
           move      bit-cod-ant          to   rf-bit-cod-ant         .
           move      bit-prt-mgd          to   rf-bit-prt-mgd         .
           move      bit-nrg-mgd          to   rf-bit-nrg-mgd         .
           move      bit-dri-mgd          to   rf-bit-dri-mgd         .
           move      bit-nri-mgd          to   rf-bit-nri-mgd         .
           move      bit-nps-sdb          to   rf-bit-nps-sdb         .
           move      bit-ctr-sdb          to   rf-bit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       exe-cnv-bit-450.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bit-460.
           move      bit-iva-cod (w-c01)  to   rf-bit-iva-cod (w-c01) .
           move      bit-iva-ibl (w-c01)  to   rf-bit-iva-ibl (w-c01) .
           move      bit-iva-imp (w-c01)  to   rf-bit-iva-imp (w-c01) .
           go to     exe-cnv-bit-450.
       exe-cnv-bit-460.
           move      bit-iva-tdo          to   rf-bit-iva-tdo         .
           move      zero                 to   w-c01                  .
       exe-cnv-bit-470.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bit-480.
           move      bit-ctp-cod (w-c01)  to   rf-bit-ctp-cod (w-c01) .
           move      bit-ctp-imp (w-c01)  to   rf-bit-ctp-imp (w-c01) .
           go to     exe-cnv-bit-470.
       exe-cnv-bit-480.
           move      bit-ctr-stp          to   rf-bit-ctr-stp         .
           move      bit-flg-blx (1)      to   rf-bit-flg-blx (1)     .
           move      bit-flg-blx (2)      to   rf-bit-flg-blx (2)     .
           move      bit-flg-blx (3)      to   rf-bit-flg-blx (3)     .
           move      bit-flg-blx (4)      to   rf-bit-flg-blx (4)     .
           move      bit-flg-blx (5)      to   rf-bit-flg-blx (5)     .
           move      bit-flg-blx (6)      to   rf-bit-flg-blx (6)     .
           move      bit-flg-blx (7)      to   rf-bit-flg-blx (7)     .
           move      bit-flg-nbx (1)      to   rf-bit-flg-nbx (1)     .
           move      bit-flg-nbx (2)      to   rf-bit-flg-nbx (2)     .
           move      bit-flg-nbx (3)      to   rf-bit-flg-nbx (3)     .
           move      bit-flg-pul          to   rf-bit-flg-pul         .
           move      bit-alx-exp          to   rf-bit-alx-exp         .
       exe-cnv-bit-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-bit-ide-dat         .
           move      s-ute                to   rf-bit-ide-ute         .
           move      s-fas                to   rf-bit-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-bit-sgl-vpf         .
           move      c-dec                to   rf-bit-dec-vpf         .
           move      c-tdc                to   rf-bit-tdc-vpf         .
           move      c-cdc                to   rf-bit-cdc-vpf         .
      *                  *---------------------------------------------*
      *                  * Quota a forfait                             *
      *                  *---------------------------------------------*
           move      rf-bit-pag-qaf       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-pag-qaf         .
      *                  *---------------------------------------------*
      *                  * Acconto                                     *
      *                  *---------------------------------------------*
           move      rf-bit-pag-act       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-pag-act         .
      *                  *---------------------------------------------*
      *                  * Ammontare provvigioni a forfait             *
      *                  *---------------------------------------------*
           move      rf-bit-pvf-age       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-pvf-age         .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bit-510.
           add       1                    to   w-c01                  .
           if        w-c01                >    9
                     go to exe-cnv-bit-520.
      *
           move      rf-bit-tot-rig
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-tot-rig
                                              (w-c01)                 .
      *
           go to     exe-cnv-bit-510.
       exe-cnv-bit-520.
      *                  *---------------------------------------------*
      *                  * Importo sconto in chiusura                  *
      *                  *---------------------------------------------*
           move      rf-bit-tot-scc       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-tot-scc         .
      *                  *---------------------------------------------*
      *                  * Importo sconto pagamento                    *
      *                  *---------------------------------------------*
           move      rf-bit-tot-scp       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-tot-scp         .
      *                  *---------------------------------------------*
      *                  * Importo di addebito della spesa             *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bit-530.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bit-540.
      *
           move      rf-bit-spe-imp
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-spe-imp
                                              (w-c01)                 .
      *
           go to     exe-cnv-bit-530.
       exe-cnv-bit-540.
      *                  *---------------------------------------------*
      *                  * Importo spese incasso                       *
      *                  *---------------------------------------------*
           move      rf-bit-tot-sic       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-tot-sic         .
      *                  *---------------------------------------------*
      *                  * Importo spese incasso aggiuntive            *
      *                  *---------------------------------------------*
           move      rf-bit-tot-sia       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-tot-sia         .
      *                  *---------------------------------------------*
      *                  * Importo spese bollo                         *
      *                  *---------------------------------------------*
           move      rf-bit-tot-spb       to   w-cal-imp-eur-imi      .
           move      09                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-tot-spb         .
      *                  *---------------------------------------------*
      *                  * Castelletto iva                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bit-550.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bit-560.
      *
           move      rf-bit-iva-ibl
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-iva-ibl
                                              (w-c01)                 .
      *
           move      rf-bit-iva-imp
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-iva-imp
                                              (w-c01)                 .
           go to     exe-cnv-bit-550.
       exe-cnv-bit-560.
      *                  *---------------------------------------------*
      *                  * Totale documento, in valuta                 *
      *                  *---------------------------------------------*
           move      rf-bit-iva-tdo       to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-iva-tdo         .
      *                  *---------------------------------------------*
      *                  * Castelletto contropartite                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-bit-570.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bit-700.
      *
           move      rf-bit-ctp-imp
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      11                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-bit-ctp-imp
                                              (w-c01)                 .
      *
           go to     exe-cnv-bit-570.
       exe-cnv-bit-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bit-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bit-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bit-250.
       exe-cnv-bit-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     bit                                              .
       exe-cnv-bit-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bit-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-bit-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ccc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ccc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ccc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ccc-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-ccc-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-ccc-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-ccc-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    ccc                                       .
       exe-cnv-ccc-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   ccc-k01                .
           start     ccc    key not less
                            ccc-k01
                            invalid key
                            go to exe-cnv-ccc-800.
       exe-cnv-ccc-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      ccc    next
                            with no lock
                            at end
                            go to exe-cnv-ccc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ccc-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-ccc-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
       exe-cnv-ccc-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ccc                 .
           move      ccc-ide-dat          to   rf-ccc-ide-dat         .
           move      ccc-ide-ute          to   rf-ccc-ide-ute         .
           move      ccc-ide-fas          to   rf-ccc-ide-fas         .
           move      ccc-cod-cli          to   rf-ccc-cod-cli         .
           move      ccc-emi-slc          to   rf-ccc-emi-slc         .
           move      ccc-tdt-slc          to   rf-ccc-tdt-slc         .
           move      ccc-tso-l01          to   rf-ccc-tso-l01         .
           move      ccc-tso-l02          to   rf-ccc-tso-l02         .
           move      ccc-tso-l03          to   rf-ccc-tso-l03         .
           move      ccc-tas-api          to   rf-ccc-tas-api         .
           move      ccc-max-fid          to   rf-ccc-max-fid         .
           move      ccc-dat-fid          to   rf-ccc-dat-fid         .
           move      ccc-alx-exp          to   rf-ccc-alx-exp         .
       exe-cnv-ccc-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-ccc-ide-dat         .
           move      s-ute                to   rf-ccc-ide-ute         .
           move      s-fas                to   rf-ccc-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Fido massimo                                *
      *                  *---------------------------------------------*
           move      rf-ccc-max-fid       to   w-cal-imp-eur-imi      .
           move      13                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ccc-max-fid         .
       exe-cnv-ccc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ccc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ccc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ccc-250.
       exe-cnv-ccc-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     ccc                                              .
       exe-cnv-ccc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ccc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-ccc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [cec]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-cec-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "cec "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/gep/fls/ioc/obj/iofcec"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-cec-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-cec-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-cec-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-cec-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    cec                                       .
       exe-cnv-cec-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   cec-k01                .
           start     cec    key not less
                            cec-k01
                            invalid key
                            go to exe-cnv-cec-800.
       exe-cnv-cec-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      cec    next
                            with no lock
                            at end
                            go to exe-cnv-cec-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-cec-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-cec-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
       exe-cnv-cec-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-cec                 .
           move      cec-ann-ese          to   rf-cec-ann-ese         .
           move      cec-tip-rec          to   rf-cec-tip-rec         .
           move      cec-cod-cli          to   rf-cec-cod-cli         .
           move      zero                 to   w-c01                  .
       exe-cnv-cec-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    12
                     go to exe-cnv-cec-420.
           move      cec-prg-mns (w-c01)  to   rf-cec-prg-mns (w-c01) .
           go to     exe-cnv-cec-410.
       exe-cnv-cec-420.
           move      cec-ale-alf          to   rf-cec-ale-alf         .
           move      cec-ale-num          to   rf-cec-ale-num         .
       exe-cnv-cec-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-c01                  .
       exe-cnv-cec-510.
           add       1                    to   w-c01                  .
           if        w-c01                >    12
                     go to exe-cnv-cec-700.
      *
           move      rf-cec-prg-mns
                    (w-c01)               to   w-cal-imp-eur-imi      .
           move      13                   to   w-cal-imp-eur-int      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-cec-prg-mns
                                              (w-c01)                 .
      *
           go to     exe-cnv-cec-510.
       exe-cnv-cec-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-cec-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-cec-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-cec-250.
       exe-cnv-cec-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     cec                                              .
       exe-cnv-cec-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-cec-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-cec-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di conferma, se manuale                         *
      *    *-----------------------------------------------------------*
       ric-cnf-man-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione conversione in corso            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Conversione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione a spaces area per conferma      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico : uscita con Ok     *
      *              *-------------------------------------------------*
           if        rr-aut-man (1 : 1)   not  = "M"
                     move  "S"            to   f-xxx-sts
                     go to ric-cnf-man-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt per conferma             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Conferma conversione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione risposta                        *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-sts              .
       ric-cnf-man-200.
      *              *-------------------------------------------------*
      *              * Accettazione risposta Si/No                     *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ris-snx-lun    to   v-car                  .
           move      w-exp-ris-snx-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ris-snx-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        f-xxx-sts             =    "S"
                     move  01             to   v-num
           else if   f-xxx-sts            =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       ric-cnf-man-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   f-xxx-sts
           else if   v-num                =    02
                     move  "N"            to   f-xxx-sts
           else      move  spaces         to   f-xxx-sts              .
       ric-cnf-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S" and
                     f-xxx-sts            not  = "N"
                     go to ric-cnf-man-200.
       ric-cnf-man-999.
           exit.

      *    *===========================================================*
      *    * Scrittura rullino messaggi                                *
      *    *-----------------------------------------------------------*
       wrt-rou-msg-000.
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Conversione archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"        delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records letti   : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records scritti : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       wrt-rou-msg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione presa visione                                *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Messaggio di fine conversione archivio          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Fine conversione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"                  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se funzionamento automa-   *
      *              * tico o manuale                                  *
      *              *-------------------------------------------------*
           if        rr-aut-man (1 : 1)   =    "M"
                     go to acc-pre-vis-600
           else      go to acc-pre-vis-400.
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attesa di 1 secondo                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Se funzionamento manuale                        *
      *              *-------------------------------------------------*
       acc-pre-vis-610.
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione presa visione               *
      *                  *---------------------------------------------*
           move      spaces               to   rr-pre-vis             .
      *                  *---------------------------------------------*
      *                  * Prompt per presa visione                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      " - Digitare OK : [  ]              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vis-620.
      *                  *---------------------------------------------*
      *                  * Accettazione presa visione                  *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-pre-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   rr-pre-vis             .
           if        rr-pre-vis           not  = "OK"
                     go to acc-pre-vis-620.
       acc-pre-vis-630.
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records letti                           *
      *    *-----------------------------------------------------------*
       inc-rec-let-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrl              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrl            >    100
                     go to inc-rec-let-100
           else if   f-xxx-nrl            >    10
                     go to inc-rec-let-200
           else      go to inc-rec-let-500.
       inc-rec-let-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
       inc-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records scritti                         *
      *    *-----------------------------------------------------------*
       inc-rec-scr-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrs              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrs            >    100
                     go to inc-rec-scr-100
           else if   f-xxx-nrs            >    10
                     go to inc-rec-scr-200
           else      go to inc-rec-scr-500.
       inc-rec-scr-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
       inc-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records letti                      *
      *    *-----------------------------------------------------------*
       vis-rec-let-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records scritti                    *
      *    *-----------------------------------------------------------*
       vis-rec-scr-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo netto                               *
      *    *-----------------------------------------------------------*
       det-prz-net-000.
      *              *-------------------------------------------------*
      *              * Se valore in input a zero : uscita              *
      *              *-------------------------------------------------*
           if        w-det-prz-net-prz    =    zero
                     go to det-prz-net-999.
      *              *-------------------------------------------------*
      *              * Se percentuali di sconto tutte a zero, prezzo   *
      *              * netto pari al valore in input : uscita          *
      *              *-------------------------------------------------*
           if        w-det-prz-net-psc (1)
                                          =    zero and
                     w-det-prz-net-psc (2)
                                          =    zero and
                     w-det-prz-net-psc (3)
                                          =    zero and
                     w-det-prz-net-psc (4)
                                          =    zero and
                     w-det-prz-net-psc (5)
                                          =    zero
                     go to det-prz-net-999.
      *              *-------------------------------------------------*
      *              * Ciclo per abbattimento con sconti               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-prz-net-ctr      .
       det-prz-net-100.
           add       1                    to   w-det-prz-net-ctr      .
           if        w-det-prz-net-ctr    >    5
                     go to det-prz-net-999.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di calcolo                 *
      *                  *---------------------------------------------*
           move      w-det-prz-net-prz    to   w-cal-imp-sco-iml      .
           move      w-det-prz-net-psc
                    (w-det-prz-net-ctr)   to   w-cal-imp-sco-psc      .
           perform   cal-imp-sco-000      thru cal-imp-sco-999        .
           move      w-cal-imp-sco-imn    to   w-det-prz-net-prz      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-prz-net-100.
       det-prz-net-999.
           exit.

      *    *===========================================================*
      *    * Routine di calcolo importo scontato                       *
      *    *                                                           *
      *    * Input  : w-cal-imp-sco-iml = importo lordo da scontare    *
      *    *        : w-cal-imp-sco-psc = percentuale di sconto        *
      *    *                                                           *
      *    * Output : w-cal-imp-sco-imn = importo netto scontato       *
      *    *          w-cal-imp-sco-ams = ammontare dello sconto       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-imp-sco-000.
      *              *-------------------------------------------------*
      *              * Se percentuale di sconto a zero : uscita        *
      *              *-------------------------------------------------*
           if        w-cal-imp-sco-psc    =    zero
                     move  w-cal-imp-sco-iml
                                          to   w-cal-imp-sco-imn
                     move  zero           to   w-cal-imp-sco-ams
                     go to cal-imp-sco-999.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           subtract  w-cal-imp-sco-psc    from 100,0
                                        giving w-cal-imp-sco-w01      .
           multiply  w-cal-imp-sco-iml    by   w-cal-imp-sco-w01
                                        giving w-cal-imp-sco-w02      .
           divide    100                  into w-cal-imp-sco-w02
                                        giving w-cal-imp-sco-imn
                                               rounded                .
           subtract  w-cal-imp-sco-imn    from w-cal-imp-sco-iml
                                        giving w-cal-imp-sco-ams      .
       cal-imp-sco-999.
           exit.

      *    *===========================================================*
      *    * Routine di calcolo importo in EURO                        *
      *    *                                                           *
      *    * Input  : w-cal-imp-eur-imi = importo in input             *
      *    *        : w-cal-imp-eur-int = interi per il formato        *
      *    *        : w-cal-imp-eur-dec = decimali per il formato      *
      *    *                                                           *
      *    *          N.B.: gli interi e i decimali sono attualmente   *
      *    *                ignorati                                   *
      *    *                                                           *
      *    * Output : w-cal-imp-eur-ime = importo in EURO              *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-imp-eur-000.
      *              *-------------------------------------------------*
      *              * Se importo in input a zero : uscita             *
      *              *-------------------------------------------------*
           if        w-cal-imp-eur-imi    =    zero
                     move  zero           to   w-cal-imp-eur-ime
                     go to cal-imp-eur-999.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           move      w-cal-imp-eur-imi    to   w-cal-imp-eur-wcl      .
           divide    1936,27              into w-cal-imp-eur-wcl
                                          rounded                     .
           move      w-cal-imp-eur-wcl    to   w-cal-imp-eur-ime      .
      *              *-------------------------------------------------*
      *              * Aggiustamento decimali                          *
      *              *-------------------------------------------------*
           if        c-dec                =    1
                     multiply  10         by   w-cal-imp-eur-ime
           else if   c-dec                =    2
                     multiply  100        by   w-cal-imp-eur-ime
           else if   c-dec                =    3
                     multiply  1000       by   w-cal-imp-eur-ime      .
      *              *-------------------------------------------------*
      *              * Messaggio di errore per importo inferiore al    *
      *              * centesimo di Euro                               *
      *              *-------------------------------------------------*
       cal-imp-eur-999.
           exit.

