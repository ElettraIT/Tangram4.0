       Identification Division.
       Program-Id.                                 pcge724s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    iva                 *
      *                                   Fase:    cge724              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/10/01    *
      *                       Ultima revisione:    NdK del 06/12/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento                  *
      *                                                                *
      *                    Modello di Pagamento Unificato              *
      *                                                                *
      *                    Aggiornamento normativa 13/10/07 : F24-ICI  *
      *                                                                *
      *                    Tracciato CBI_F24_001 : rel. 6.06 29/10/07  *
      *                    (n.b.: vedi pcge7240)                       *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *    * Work-area per la ridefinizione della variabile di i.p.c.  *
      *    * in input  "inp-mst"                                       *
      *    *-----------------------------------------------------------*
       01  w-inp-mst.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        * - OP : Open modulo di stampa documento                *
      *        * - ST : Stampa documento                               *
      *        * - CL : Close modulo di stampa documento               *
      *        *-------------------------------------------------------*
           05  w-inp-mst-ope              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati in input relativi ai tipi operazione             *
      *        *-------------------------------------------------------*
           05  w-inp-mst-dat.
      *            *---------------------------------------------------*
      *            * Dati in input relativi al tipo operazione "OP"    *
      *            *---------------------------------------------------*
               10  w-inp-mst-xop.
      *                *-----------------------------------------------*
      *                * Tipo selezione stampante                      *
      *                * - F : Facoltativa                             *
      *                * - O : Obbligatoria                            *
      *                *-----------------------------------------------*
                   15  w-inp-mst-sel      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice azienda                                *
      *                *-----------------------------------------------*
                   15  w-inp-mst-azi      pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Codice terminale                              *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ter      pic  x(08)                  .
      *                *-----------------------------------------------*
      *                * Codice utente                                 *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ute      pic  x(08)                  .
      *                *-----------------------------------------------*
      *                * Sigla sistema applicativo                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-sap      pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Sigla area gestionale                         *
      *                *-----------------------------------------------*
                   15  w-inp-mst-arg      pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Sigla settore gestionale                      *
      *                *-----------------------------------------------*
                   15  w-inp-mst-set      pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Sigla fase gestionale                         *
      *                *-----------------------------------------------*
                   15  w-inp-mst-fas      pic  x(06)                  .
      *                *-----------------------------------------------*
      *                * Descrizione del programma                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-dep      pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Dati in input relativi al tipo operazione "ST"    *
      *            *---------------------------------------------------*
               10  w-inp-mst-xst redefines
                   w-inp-mst-xop.
      *                *-----------------------------------------------*
      *                * Identificazione del documento                 *
      *                *-----------------------------------------------*
                   15  w-inp-mst-prt      pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Numero copia in corso di stampa               *
      *                *-----------------------------------------------*
                   15  w-inp-ctr-cps      pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(68)                  .
      *            *---------------------------------------------------*
      *            * Dati in input relativi al tipo operazione "CL"    *
      *            *---------------------------------------------------*
               10  w-inp-mst-xcl redefines
                   w-inp-mst-xop.
      *                *-----------------------------------------------*
      *                * Nessun dato ulteriore                         *
      *                *-----------------------------------------------*
                   15  filler             pic  x(78)                  .

      *    *===========================================================*
      *    * Work-area per la ridefinizione della variabile di i.p.c.  *
      *    * in output "out-mst"                                       *
      *    *-----------------------------------------------------------*
       01  w-out-mst.
      *        *-------------------------------------------------------*
      *        * Dati in output relativi ai tipi operazione            *
      *        *-------------------------------------------------------*
           05  w-out-mst-dat.
      *            *---------------------------------------------------*
      *            * Dati in output relativi al tipo operazione "OP"   *
      *            *---------------------------------------------------*
               10  w-out-mst-xop.
      *                *-----------------------------------------------*
      *                * Esito della funzione Open                     *
      *                * - Spaces : Operazione correttamente eseguita  *
      *                *            con selezione stampante effettua-  *
      *                *            ta correttamente                   *
      *                * - N      : Operazione correttamente eseguita  *
      *                *            e senza selezione stampante facol- *
      *                *            tativa non effettuata              *
      *                * - X      : Operazione non eseguita in quanto  *
      *                *            selezione stampante obbligatoria   *
      *                *            non effettuata                     *
      *                * - E      : Errore grave avvenuto in esecuzio- *
      *                *            ne della funzione                  *
      *                *-----------------------------------------------*
                   15  w-out-mst-opn      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(79)                  .
      *            *---------------------------------------------------*
      *            * Dati in output relativi al tipo operazione "ST"   *
      *            *---------------------------------------------------*
               10  w-out-mst-xst redefines
                   w-out-mst-xop.
      *                *-----------------------------------------------*
      *                * Esito della funzione Stampa                   *
      *                * - Spaces : Operazione correttamente eseguita  *
      *                * - N      : Documento non trovato              *
      *                * - E      : Errore grave avvenuto in esecuzio- *
      *                *            ne della funzione                  *
      *                *-----------------------------------------------*
                   15  w-out-mst-stp      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(79)                  .
      *            *---------------------------------------------------*
      *            * Dati in output relativi al tipo operazione "CL"   *
      *            *---------------------------------------------------*
               10  w-out-mst-xcl redefines
                   w-out-mst-xop.
      *                *-----------------------------------------------*
      *                * Esito della funzione Close                    *
      *                * - Spaces : Operazione correttamente eseguita  *
      *                * - E      : Errore grave avvenuto in esecuzio- *
      *                *            ne della funzione                  *
      *                *-----------------------------------------------*
                   15  w-out-mst-cls      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(79)                  .

      *    *===========================================================*
      *    * Area di identificazione del programma chiamante           *
      *    *-----------------------------------------------------------*
       01  w-ide.
      *        *-------------------------------------------------------*
      *        * Tipo selezione stampante                              *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-sel              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice azienda                                        *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-azi              pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice terminale                                      *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-ter              pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice utente                                         *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-ute              pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-sap              pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-arg              pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-set              pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-fas              pic  x(06) value spaces     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-dep              pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Data e ora di sistema                                 *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-sdt              pic  9(15) value zero       .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status generali del programma   *
      *        *-------------------------------------------------------*
           05  w-cnt-sts.
      *            *---------------------------------------------------*
      *            * Flag di selezione stampante                       *
      *            * - Spaces : Non selezionata                        *
      *            * - S      : Selezionata                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-flg-sst      pic  x(01) value spaces     .
      *            *---------------------------------------------------*
      *            * Flag di files necessari per la stampa aperti      *
      *            * - Spaces : Non aperti                             *
      *            * - S      : Aperti                                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-flg-opn      pic  x(01) value spaces     .
      *            *---------------------------------------------------*
      *            * Flag di Begin eseguito                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-flg-bgn      pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
      *            *---------------------------------------------------*
      *            * Parametri di stampa specifici del programma       *
      *            *---------------------------------------------------*
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .
      *        *-------------------------------------------------------*
      *        * [mpu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmpu"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [raa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfraa"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)                  .
               10  w-let-arc-gxc-tip      pic  x(01)                  .
               10  w-let-arc-gxc-cmn      pic  9(05)                  .
               10  w-let-arc-gxc-fzn      pic  9(03)                  .
               10  w-let-arc-gxc-lct      pic  9(03)                  .
               10  w-let-arc-gxc-des      pic  x(30)                  .
               10  w-let-arc-gxc-prv      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cbp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cbp.
               10  w-let-arc-cbp-flg      pic  x(01)                  .
               10  w-let-arc-cbp-tip      pic  9(02)                  .
               10  w-let-arc-cbp-cod      pic  x(10)                  .
               10  w-let-arc-cbp-des      pic  x(40)                  .
               10  w-let-arc-cbp-csc      pic  9(07)                  .
               10  w-let-arc-cbp-csa      pic  9(07)                  .
               10  w-let-arc-cbp-abi      pic  9(05)                  .
               10  w-let-arc-cbp-cab      pic  9(05)                  .
               10  w-let-arc-cbp-ccb      pic  x(12)                  .
               10  w-let-arc-cbp-cco      pic  9(07)                  .
               10  w-let-arc-cbp-bba      pic  9(07)                  .
               10  w-let-arc-cbp-bbp      pic  9(07)                  .
               10  w-let-arc-cbp-psm      pic  9(07)                  .
               10  w-let-arc-cbp-psi      pic  9(07)                  .
               10  w-let-arc-cbp-pdi      pic  9(07)                  .
               10  w-let-arc-cbp-psc      pic  9(07)                  .
               10  w-let-arc-cbp-anv      pic  9(07)                  .
               10  w-let-arc-cbp-dfp      pic  9(07)                  .
               10  w-let-arc-cbp-dfa      pic  9(07)                  .
               10  w-let-arc-cbp-onb      pic  9(07)                  .
               10  w-let-arc-cbp-inb      pic  9(07)                  .
               10  w-let-arc-cbp-ccp      pic  x(12)                  .
               10  w-let-arc-cbp-bpa      pic  9(07)                  .
               10  w-let-arc-cbp-bpp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-cfi      pic  x(16)                  .
               10  w-let-arc-fnt-cmn      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axi.
               10  w-let-arc-axi-flg      pic  x(01)                  .
               10  w-let-arc-axi-cod      pic  9(05)                  .
               10  w-let-arc-axi-den      pic  x(40)                  .
               10  w-let-arc-axi-cib.
                   15  w-let-arc-axi-cic
                               occurs 05  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axs.
               10  w-let-arc-axs-flg      pic  x(01)                  .
               10  w-let-arc-axs-abi      pic  9(05)                  .
               10  w-let-arc-axs-cab      pic  9(05)                  .
               10  w-let-arc-axs-den      pic  x(40)                  .
               10  w-let-arc-axs-via      pic  x(40)                  .
               10  w-let-arc-axs-loc      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per dati anagrafici azienda                     *
      *    *-----------------------------------------------------------*
       01  w-azi.
           05  w-azi-rag-soc.
               10  w-azi-rag-cgn          pic  x(41)                  .
               10  w-azi-rag-nom          pic  x(29)                  .
           05  w-azi-ind-azi              pic  x(40)                  .
           05  w-azi-loc-azi.
               10  w-azi-loc-azi-cap      pic  x(05)                  .
               10  w-azi-loc-azi-f01      pic  x(01)                  .
               10  w-azi-loc-azi-loc      pic  x(34)                  .
           05  w-azi-prv-azi              pic  x(03)                  .
           05  w-azi-cod-fis              pic  x(16)                  .
           05  w-azi-cod-cmn              pic  9(05)                  .
           05  w-azi-dat-nsc              pic  9(07)                  .
           05  w-azi-loc-nsc              pic  x(40)                  .
           05  w-azi-prv-nsc              pic  x(02)                  .
           05  w-azi-sex-int              pic  x(01)                  .
           05  w-azi-prt-iw1              pic  9(02)                  .
           05  w-azi-prt-iw2              pic  9(02)                  .
           05  w-azi-ctr-001              pic  9(02)                  .
           05  w-azi-ctr-002              pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per Totali                                      *
      *    *-----------------------------------------------------------*
       01  w-tot.
      *        *-------------------------------------------------------*
      *        * Contatori e indici                                    *
      *        *-------------------------------------------------------*
      *            *---------------------------------------------------*
      *            * Contatore righe                                   *
      *            *---------------------------------------------------*
           05  w-tot-ctr-rig              pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatori Erario                                  *
      *            *---------------------------------------------------*
           05  w-tot-ctr-era              pic  9(05)                  .
           05  w-tot-max-era              pic  9(05) value 06         .
           05  w-tot-uea-era              pic  9(05) value 25         .
           05  w-tot-vpp-era              pic  9(05) value 18         .
      *            *---------------------------------------------------*
      *            * Contatori Inps                                    *
      *            *---------------------------------------------------*
           05  w-tot-ctr-ips              pic  9(05)                  .
           05  w-tot-max-ips              pic  9(05) value 04         .
           05  w-tot-vpp-ips              pic  9(05) value 27         .
      *            *---------------------------------------------------*
      *            * Contatori Regione                                 *
      *            *---------------------------------------------------*
           05  w-tot-ctr-rgn              pic  9(05)                  .
           05  w-tot-max-rgn              pic  9(05) value 04         .
           05  w-tot-vpp-rgn              pic  9(05) value 34         .
      *            *---------------------------------------------------*
      *            * Contatori Enti locali                             *
      *            *---------------------------------------------------*
           05  w-tot-ctr-enl              pic  9(05)                  .
           05  w-tot-max-enl              pic  9(05) value 04         .
           05  w-tot-vpp-enl              pic  9(05) value 41         .
      *            *---------------------------------------------------*
      *            * Contatori ICI                                     *
      *            *---------------------------------------------------*
           05  w-tot-ctr-inl              pic  9(05)                  .
           05  w-tot-max-inl              pic  9(05) value 03         .
           05  w-tot-vpp-inl              pic  9(05) value 48         .
      *            *---------------------------------------------------*
      *            * Contatori Enti Previdenziali                      *
      *            *---------------------------------------------------*
           05  w-tot-ctr-enp              pic  9(05)                  .
           05  w-tot-max-enp              pic  9(05) value 02         .
           05  w-tot-vpp-enp              pic  9(05) value 53         .
      *        *-------------------------------------------------------*
      *        * Totale generale                                       *
      *        *-------------------------------------------------------*
           05  w-tot-gen-tot              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Totali sezioni                                        *
      *        *-------------------------------------------------------*
           05  w-tot-deb-era              pic s9(13)                  .
           05  w-tot-cre-era              pic s9(13)                  .
           05  w-tot-deb-ips              pic s9(13)                  .
           05  w-tot-cre-ips              pic s9(13)                  .
           05  w-tot-deb-rgn              pic s9(13)                  .
           05  w-tot-cre-rgn              pic s9(13)                  .
           05  w-tot-deb-enl              pic s9(13)                  .
           05  w-tot-cre-enl              pic s9(13)                  .
           05  w-tot-deb-inl              pic s9(13)                  .
           05  w-tot-cre-inl              pic s9(13)                  .
           05  w-tot-deb-enp              pic s9(13)                  .
           05  w-tot-cre-enp              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per stampa totali sezioni                      *
      *        *-------------------------------------------------------*
           05  w-tot-deb-vpp              pic  9(02)                  .
           05  w-tot-deb-stp              pic s9(13)                  .
           05  w-tot-cre-stp              pic s9(13)                  .
           05  w-tot-dia-enl              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per posizionamenti fissi valori sezioni        *
      *        *-------------------------------------------------------*
           05  w-tot-deb-pos              pic  9(02) value 45         .
           05  w-tot-cre-pos              pic  9(02) value 57         .
           05  w-tot-tot-pos              pic  9(02) value 69         .

      *    *===========================================================*
      *    * Work area per ridefinizione dati sezione                  *
      *    *-----------------------------------------------------------*
       01  w-rds.
      *        *-------------------------------------------------------*
      *        * Area di ridefinizione                                 *
      *        *-------------------------------------------------------*
           05  w-rds-ele.
               10  w-rds-ele-sez.
                   15  filler occurs 512  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Dati sezioni                                      *
      *            *---------------------------------------------------*
      *                *-----------------------------------------------*
      *                * Presentazione                                 *
      *                *-----------------------------------------------*
               10  w-rds-sez-pre redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-cbp      pic  x(10)                  .
                   15  w-rds-cod-cbp-des  pic  x(40)                  .
                   15  w-rds-fir-maa      pic  x(40)                  .
                   15  w-rds-dat-pre      pic  9(07)                  .
                   15  w-rds-cod-int      pic  9(07)                  .
                   15  w-rds-cod-int-rag  pic  x(40)                  .
                   15  filler occurs 368  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Erario                                        *
      *                *-----------------------------------------------*
               10  w-rds-sez-era redefines
                   w-rds-ele-sez.
                   15  w-rds-trb-era      pic  x(07)                  .
                   15  w-rds-trb-era-des  pic  x(40)                  .
                   15  w-rds-rtz-era      pic  x(07)                  .
                   15  w-rds-ann-era      pic  9(04)                  .
                   15  w-rds-uff-era      pic  x(07)                  .
                   15  w-rds-uff-era-des  pic  x(40)                  .
                   15  w-rds-att-era      pic  x(11)                  .
                   15  filler occurs 396  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * INPS                                          *
      *                *-----------------------------------------------*
               10  w-rds-sez-ips redefines
                   w-rds-ele-sez.
                   15  w-rds-sed-ips      pic  x(07)                  .
                   15  w-rds-sed-ips-des  pic  x(40)                  .
                   15  w-rds-trb-ips      pic  x(07)                  .
                   15  w-rds-trb-ips-des  pic  x(40)                  .
                   15  w-rds-mat-ips      pic  x(20)                  .
                   15  w-rds-mda-ips      pic  9(02)                  .
                   15  w-rds-ada-ips      pic  9(04)                  .
                   15  w-rds-maa-ips      pic  9(02)                  .
                   15  w-rds-aaa-ips      pic  9(04)                  .
                   15  filler occurs 386  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Regione                                       *
      *                *-----------------------------------------------*
               10  w-rds-sez-rgn redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-rgn      pic  x(07)                  .
                   15  w-rds-cod-rgn-des  pic  x(40)                  .
                   15  w-rds-trb-rgn      pic  x(07)                  .
                   15  w-rds-trb-rgn-des  pic  x(40)                  .
                   15  w-rds-rtz-rgn      pic  x(07)                  .
                   15  w-rds-ann-rgn      pic  9(04)                  .
                   15  filler occurs 407  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Enti locali                                   *
      *                *-----------------------------------------------*
               10  w-rds-sez-enl redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-enl      pic  x(07)                  .
                   15  w-rds-cod-enl-des  pic  x(40)                  .
                   15  w-rds-trb-enl      pic  x(07)                  .
                   15  w-rds-trb-rgn-des  pic  x(40)                  .
                   15  w-rds-rtz-enl      pic  x(07)                  .
                   15  w-rds-ann-enl      pic  9(04)                  .
      *                    *-------------------------------------------*
      *                    * Aggiunti per ICI/IMU                      *
      *                    *-------------------------------------------*
                   15  w-rds-pi1-enl      pic  x(01)                  .
                   15  w-rds-pi2-enl      pic  x(01)                  .
                   15  w-rds-pi3-enl      pic  x(01)                  .
                   15  w-rds-pi4-enl      pic  x(01)                  .
                   15  w-rds-nim-enl      pic  9(02)                  .
                   15  w-rds-dia-enl      pic  9(13)                  .
      *                    *-------------------------------------------*
      *                    * Aggiunto per identificativo               *
      *                    *-------------------------------------------*
                   15  w-rds-ide-enl      pic  x(20)                  .
                   15  filler occurs 368  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * INAIL                                         *
      *                *-----------------------------------------------*
               10  w-rds-sez-inl redefines
                   w-rds-ele-sez.
                   15  w-rds-sed-inl      pic  x(07)                  .
                   15  w-rds-sed-inl-des  pic  x(40)                  .
                   15  w-rds-pos-inl      pic  x(20)                  .
                   15  w-rds-ccp-inl      pic  x(07)                  .
                   15  w-rds-rif-inl      pic  x(20)                  .
                   15  w-rds-cau-inl      pic  x(07)                  .
                   15  filler occurs 411  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Enti previdenziali                            *
      *                *-----------------------------------------------*
               10  w-rds-sez-enp redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-enp      pic  x(07)                  .
                   15  w-rds-cod-enp-des  pic  x(40)                  .
                   15  w-rds-sed-enp      pic  x(07)                  .
                   15  w-rds-sed-enp-des  pic  x(40)                  .
                   15  w-rds-cau-enp      pic  x(07)                  .
                   15  w-rds-cau-enp-des  pic  x(40)                  .
                   15  w-rds-pos-enp      pic  x(20)                  .
                   15  w-rds-mda-enp      pic  9(02)                  .
                   15  w-rds-ada-enp      pic  9(04)                  .
                   15  w-rds-maa-enp      pic  9(02)                  .
                   15  w-rds-aaa-enp      pic  9(04)                  .
                   15  filler occurs 339  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per stampa importo in valuta base               *
      *    *-----------------------------------------------------------*
       01  w-stp-imp-vlb.
           05  w-stp-imp-vlb-imp          pic  9(13)                  .
           05  w-stp-imp-vlb-imp-r redefines
               w-stp-imp-vlb-imp.
               10  w-stp-imp-vlb-int      pic  9(11)                  .
               10  w-stp-imp-vlb-dec      pic  9(02)                  .
           05  w-stp-imp-vlb-lin          pic  9(03)                  .
           05  w-stp-imp-vlb-pos          pic  9(03)                  .
           05  w-stp-imp-vlb-sgn          pic  x(01)                  .
           05  w-stp-imp-vlb-snz          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per stampa data presentazione                   *
      *    *-----------------------------------------------------------*
       01  w-stp-dat-pre.
           05  w-stp-dat-pre-dat          pic  9(07)                  .
           05  w-stp-dat-pre-ded          pic  x(08)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazione codice provincia da stringa   *
      *        * localita'                                             *
      *        *-------------------------------------------------------*
           05  w-det-cod-prv.
               10  w-det-cod-prv-cod      pic  x(03)                  .
               10  w-det-cod-prv-loc      pic  x(40)                  .
               10  w-det-cod-prv-pos      pic  9(03)                  .
               10  w-det-cod-prv-c01      pic  9(03)                  .
               10  w-det-cod-prv-cx1      pic  9(03)                  .
               10  w-det-cod-prv-cx2      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione codice ufficio e codice atto  *
      *        *-------------------------------------------------------*
           05  w-det-era-uea.
               10  w-det-era-uea-uff      pic  x(03)                  .
               10  w-det-era-uea-att      pic  x(11)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile di i.p.c. dello stesso  *
      *              * livello "inp-mst"                               *
      *              *-------------------------------------------------*
           perform   rea-inp-mst-000      thru rea-inp-mst-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-inp-mst-ope        =    "OP"
                     go to main-200
           else if   w-inp-mst-ope        =    "ST"
                     go to main-400
           else if   w-inp-mst-ope        =    "CL"
                     go to main-600
           else      go to main-999.
       main-200.
      *              *-------------------------------------------------*
      *              * Se funzione OP : Open                           *
      *              *-------------------------------------------------*
           perform   exe-fun-opn-000      thru exe-fun-opn-999        .
           go to     main-999.
       main-400.
      *              *-------------------------------------------------*
      *              * Se funzione ST : Stampa                         *
      *              *-------------------------------------------------*
           perform   exe-fun-stp-000      thru exe-fun-stp-999        .
           go to     main-999.
       main-600.
      *              *-------------------------------------------------*
      *              * Se funzione CL : Close                          *
      *              *-------------------------------------------------*
           perform   exe-fun-cls-000      thru exe-fun-cls-999        .
           go to     main-999.
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Lettura della variabile di i.p.c. dello stesso livello di *
      *    * nome "inp-mst" per parametri su funzione da eseguire      *
      *    *-----------------------------------------------------------*
       rea-inp-mst-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile                         *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "inp-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to rea-inp-mst-600.
       rea-inp-mst-300.
      *              *-------------------------------------------------*
      *              * Se variabile non trovata o non corretta         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione area di ridefinizione della *
      *                  * variabile                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-inp-mst              .
      *                  *---------------------------------------------*
      *                  * Cancellazione della variabile in uscita     *
      *                  *---------------------------------------------*
           perform   del-out-mst-000      thru del-out-mst-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-inp-mst-999.
       rea-inp-mst-600.
      *              *-------------------------------------------------*
      *              * Se variabile trovata                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su formato della variabile             *
      *                  *---------------------------------------------*
           if        s-tip                not  = "A"
                     go to rea-inp-mst-300.
           if        s-car                not  = 80
                     go to rea-inp-mst-300.
      *                  *---------------------------------------------*
      *                  * Se formato variabile corretto               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spostamento del valore della variabile  *
      *                      * in area di ridefinizione                *
      *                      *-----------------------------------------*
           move      s-alf                to   w-inp-mst              .
       rea-inp-mst-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione della variabile di i.p.c. dello stesso li-  *
      *    * vello di nome "out-mst"                                   *
      *    *-----------------------------------------------------------*
       del-out-mst-000.
      *              *-------------------------------------------------*
      *              * Cancellazione della variabile                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "out-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-out-mst-999.
           exit.

      *    *===========================================================*
      *    * Scrittura della variabile di i.p.c. dello stesso livello  *
      *    * di nome "out-mst"                                         *
      *    *-----------------------------------------------------------*
       wrt-out-mst-000.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile                       *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "out-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      w-out-mst            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       wrt-out-mst-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione OP : Open                             *
      *    *-----------------------------------------------------------*
       exe-fun-opn-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori passati dal chiamante    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo selezione stampante                    *
      *                  *---------------------------------------------*
           move      w-inp-mst-sel        to   w-ide-pgc-sel          .
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           move      w-inp-mst-azi        to   w-ide-pgc-azi          .
      *                  *---------------------------------------------*
      *                  * Codice terminale                            *
      *                  *---------------------------------------------*
           move      w-inp-mst-ter        to   w-ide-pgc-ter          .
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           move      w-inp-mst-ute        to   w-ide-pgc-ute          .
      *                  *---------------------------------------------*
      *                  * Sigla sistema applicativo                   *
      *                  *---------------------------------------------*
           move      w-inp-mst-sap        to   w-ide-pgc-sap          .
      *                  *---------------------------------------------*
      *                  * Sigla area gestionale                       *
      *                  *---------------------------------------------*
           move      w-inp-mst-arg        to   w-ide-pgc-arg          .
      *                  *---------------------------------------------*
      *                  * Sigla settore gestionale                    *
      *                  *---------------------------------------------*
           move      w-inp-mst-set        to   w-ide-pgc-set          .
      *                  *---------------------------------------------*
      *                  * Sigla fase gestionale                       *
      *                  *---------------------------------------------*
           move      w-inp-mst-fas        to   w-ide-pgc-fas          .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      w-inp-mst-dep        to   w-ide-pgc-dep          .
       exe-fun-opn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione data e ora di sistema           *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-sdt                to   w-ide-pgc-sdt          .
       exe-fun-opn-400.
      *              *-------------------------------------------------*
      *              * Selezione della stampante                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per selezione della  *
      *                  * stampante specifici per il programma        *
      *                  *---------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Selezione stampante                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri per modulo "mpslct"           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice azienda                      *
      *                          *-------------------------------------*
           move      w-ide-pgc-azi        to   r-env-cod-azi          .
      *                          *-------------------------------------*
      *                          * Codice terminale                    *
      *                          *-------------------------------------*
           move      w-ide-pgc-ter        to   r-env-cod-ter          .
      *                          *-------------------------------------*
      *                          * Codice utente                       *
      *                          *-------------------------------------*
           move      w-ide-pgc-ute        to   r-env-cod-ute          .
      *                          *-------------------------------------*
      *                          * System date and time                *
      *                          *-------------------------------------*
           move      w-ide-pgc-sdt        to   r-env-dat-tim          .
      *                          *-------------------------------------*
      *                          * Sistema applicativo                 *
      *                          *-------------------------------------*
           move      w-ide-pgc-sap        to   r-ide-sis-app          .
      *                          *-------------------------------------*
      *                          * Area gestionale                     *
      *                          *-------------------------------------*
           move      w-ide-pgc-arg        to   r-ide-are-ges          .
      *                          *-------------------------------------*
      *                          * Settore gestionale                  *
      *                          *-------------------------------------*
           move      w-ide-pgc-set        to   r-ide-set-ges          .
      *                          *-------------------------------------*
      *                          * Fase gestionale                     *
      *                          *-------------------------------------*
           move      w-ide-pgc-fas        to   r-ide-fas-ges          .
      *                          *-------------------------------------*
      *                          * Flags di tipo selezione             *
      *                          *-------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                          *-------------------------------------*
      *                          * Codice stampante                    *
      *                          *-------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                          *-------------------------------------*
      *                          * Tipo di stampa                      *
      *                          *-------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                          *-------------------------------------*
      *                          * Codice modulo                       *
      *                          *-------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                          *-------------------------------------*
      *                          * Tipo modulo                         *
      *                          *-------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                          *-------------------------------------*
      *                          * Ampiezza linea stampa in caratteri  *
      *                          *-------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                          *-------------------------------------*
      *                          * Top margin in linee                 *
      *                          *-------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                          *-------------------------------------*
      *                          * Numero linee di stampa minimo       *
      *                          *-------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                          *-------------------------------------*
      *                          * Bottom margin in linee              *
      *                          *-------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                          *-------------------------------------*
      *                          * Ampiezza caratteri                  *
      *                          *-------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                          *-------------------------------------*
      *                          * Altezza interlinea                  *
      *                          *-------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                          *-------------------------------------*
      *                          * Area riservata espansioni future    *
      *                          *-------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                          *-------------------------------------*
      *                          * Area riservata funzioni speciali    *
      *                          *-------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *                      *-----------------------------------------*
      *                      * Richiamo modulo "mpslct"                *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *                  *---------------------------------------------*
      *                  * Test su esito selezione stampante           *
      *                  *---------------------------------------------*
           if        r-rsc                =    spaces
                     go to exe-fun-opn-600.
       exe-fun-opn-425.
      *                  *---------------------------------------------*
      *                  * Se selezione stampante non eseguita         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo selezione stampante        *
      *                      *-----------------------------------------*
           if        w-ide-pgc-sel        =    "O"
                     go to exe-fun-opn-475.
       exe-fun-opn-450.
      *                          *-------------------------------------*
      *                          * Se tipo selezione stampante facol-  *
      *                          * tativo                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Apertura files necessari alla   *
      *                              * stampa                          *
      *                              *---------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
      *                              *---------------------------------*
      *                              * Flag di files aperti            *
      *                              *---------------------------------*
           move      "S"                  to   w-cnt-sts-flg-opn      .
      *                              *---------------------------------*
      *                              * Flag di selezione stampante non *
      *                              * effettuata                      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-sst      .
      *                              *---------------------------------*
      *                              * Flag di Begin non eseguito      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
      *                              *---------------------------------*
      *                              * Uscita con status a "N"         *
      *                              *---------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-opn          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     exe-fun-opn-999.
       exe-fun-opn-475.
      *                          *-------------------------------------*
      *                          * Se tipo selezione stampante obbli-  *
      *                          * gatorio                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Flag di files non aperti        *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-opn      .
      *                              *---------------------------------*
      *                              * Flag di selezione stampante non *
      *                              * effettuata                      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-sst      .
      *                              *---------------------------------*
      *                              * Flag di Begin non eseguito      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
      *                              *---------------------------------*
      *                              * Uscita con status a "X"         *
      *                              *---------------------------------*
           move      spaces               to   w-out-mst              .
           move      "X"                  to   w-out-mst-opn          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     exe-fun-opn-999.
       exe-fun-opn-600.
      *                  *---------------------------------------------*
      *                  * Se selezione stampante eseguita             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura parametri di selezione stampa   *
      *                      * da segreteria                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Inizializzazione area parametri di  *
      *                          * stampa                              *
      *                          *-------------------------------------*
           move      spaces               to   p-sel                  .
      *                          *-------------------------------------*
      *                          * Inizializzazione numero progressivo *
      *                          * segmento                            *
      *                          *-------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       exe-fun-opn-650.
      *                          *-------------------------------------*
      *                          * Incremento numero progressivo seg-  *
      *                          * mento                               *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *                          *-------------------------------------*
      *                          * Richiamo del modulo di segreteria   *
      *                          * per l'estrazione del segmento di    *
      *                          * parametri stampa                    *
      *                          *-------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                          *-------------------------------------*
      *                          * Concatenazione del segmento in area *
      *                          * parametri di stampa selezionati     *
      *                          *-------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *                          *-------------------------------------*
      *                          * Riciclo su segmento successivo, a   *
      *                          * meno che non si sia sull'ultimo     *
      *                          *-------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to exe-fun-opn-650.
       exe-fun-opn-700.
      *                      *-----------------------------------------*
      *                      * Preparazioni per uscita                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Flag di selezione stampante effet-  *
      *                          * tuata                               *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-sts-flg-sst      .
      *                          *-------------------------------------*
      *                          * Flag di Begin non eseguito          *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
      *                          *-------------------------------------*
      *                          * Apertura files necessari alla stam- *
      *                          * pa                                  *
      *                          *-------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
      *                          *-------------------------------------*
      *                          * Flag di files aperti                *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-sts-flg-opn      .
      *                          *-------------------------------------*
      *                          * Uscita con status a Spaces          *
      *                          *-------------------------------------*
           move      spaces               to   w-out-mst              .
           move      spaces               to   w-out-mst-opn          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     exe-fun-opn-999.
       exe-fun-opn-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione ST : Stampa                           *
      *    *-----------------------------------------------------------*
       exe-fun-stp-000.
      *              *-------------------------------------------------*
      *              * Se files non aperti : uscita                    *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-opn    not  = "S"
                     go to exe-fun-stp-999.
      *              *-------------------------------------------------*
      *              * Se selezione stampante non effettuata : uscita  *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-sst    not  = "S"
                     go to exe-fun-stp-999.
      *              *-------------------------------------------------*
      *              * Se Begin non eseguito                           *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-bgn    =    "S"
                     go to exe-fun-stp-500.
      *                  *---------------------------------------------*
      *                  * Esecuzione Begin                            *
      *                  *---------------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Flag di Begin eseguito                      *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-sts-flg-bgn      .
       exe-fun-stp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Esecuzione routine di stampa                    *
      *              *-------------------------------------------------*
           perform   rou-stp-doc-000      thru rou-stp-doc-999        .
       exe-fun-stp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione CL : Close                            *
      *    *-----------------------------------------------------------*
       exe-fun-cls-000.
      *              *-------------------------------------------------*
      *              * Se files aperti                                 *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-opn    not  = "S"
                     go to exe-fun-cls-100.
      *                  *---------------------------------------------*
      *                  * Chiusura files necessari alla stampa        *
      *                  *---------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       exe-fun-cls-100.
      *              *-------------------------------------------------*
      *              * Se selezione stampante effettuata               *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-sst    not  = "S"
                     go to exe-fun-cls-900.
      *                  *---------------------------------------------*
      *                  * Esecuzione End                              *
      *                  *---------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa    "mprint"     *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-fun-cls-900.
      *              *-------------------------------------------------*
      *              * Flag di files non aperti                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-flg-opn      .
      *              *-------------------------------------------------*
      *              * Flag di selezione stampante non effettuata      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-flg-sst      .
      *              *-------------------------------------------------*
      *              * Flag di Begin non eseguito                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
       exe-fun-cls-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampante            *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      080                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag destinato al controllo del Tipo Stampa *
      *                  * per inibire la possibilita' della stampa a  *
      *                  * Video                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-stp-esp-fut
                                              (98 : 1)                .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri correttivi per il modulo EPS      *
      *                  *                                             *
      *                  * Flag di parametri EPS    fisso '@'          *
      *                  * Interlinea               formato 99.99      *
      *                  * Scala                    formato 9.9        *
      *                  * Margine superiore        formato 99         *
      *                  * Margine inferiore        formato 99         *
      *                  * Margine sinistro         formato 99         *
      *                  * Margine destro           formato 99         *
      *                  * Posizionamento asse 'x'  formato 99         *
      *                  * Posizionamento asse 'y'  formato 99         *
      *                  * Angolo di rotazione      formato 999        *
      *                  * Linee di stampa per font formato 999        *
      *                  *---------------------------------------------*
           move      "@ 12.00 1.0 10 15 10 10 10 10 000 080"
                                          to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Open files necessari alla stampa                          *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [mpu]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [raa]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [mpu]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [raa]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *-----------------------------------------------------------*
       rou-stp-doc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
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
       rou-stp-doc-010.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-ctr-rig          .
           move      zero                 to   w-tot-ctr-era          .
           move      zero                 to   w-tot-ctr-ips          .
           move      zero                 to   w-tot-ctr-rgn          .
           move      zero                 to   w-tot-ctr-enl          .
           move      zero                 to   w-tot-ctr-inl          .
           move      zero                 to   w-tot-ctr-enp          .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per codici vari          *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-era-uea-uff      .
           move      spaces               to   w-det-era-uea-att      .
       rou-stp-doc-050.
      *              *-------------------------------------------------*
      *              * Calcolo preliminare dei totali                  *
      *              *-------------------------------------------------*
           perform   rou-stp-doc-sub-000  thru rou-stp-doc-sub-999    .
       rou-stp-doc-100.
      *              *-------------------------------------------------*
      *              * Start su file [mpu]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-inp-mst-prt        to   rf-mpu-num-prt         .
           move      zero                 to   rf-mpu-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-stp-doc-200.
      *                  *---------------------------------------------*
      *                  * Se record non trovato uscita con status al  *
      *                  * valore "N"                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-stp          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mpu]                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-stp-doc-300.
      *                  *---------------------------------------------*
      *                  * Release                                     *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-900.
       rou-stp-doc-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a test su contatore       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-mpu-num-prt       =    w-inp-mst-prt
                     go to rou-stp-doc-400.
      *                  *---------------------------------------------*
      *                  * Release                                     *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-900.
       rou-stp-doc-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-tot-ctr-rig          .
       rou-stp-doc-420.
      *              *-------------------------------------------------*
      *              * Nuova pagina                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-rig        not  = 1
                     go to rou-stp-doc-500.
      *                  *---------------------------------------------*
      *                  * Avanzamento pagina                          *
      *                  *---------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  rou-stp-doc-999.
       rou-stp-doc-440.
       rou-stp-doc-500.
      *              *-------------------------------------------------*
      *              * Ridefinizione riga                              *
      *              *-------------------------------------------------*
           move      rf-mpu-ele-sez       to   w-rds-ele-sez          .
       rou-stp-doc-600.
      *              *-------------------------------------------------*
      *              * Stampa corpo documento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della sezione        *
      *                  *---------------------------------------------*
           if        rf-mpu-tip-sez       =    "P    "
                     perform rou-stp-doc-pre-000
                                          thru rou-stp-doc-pre-999
           else if   rf-mpu-tip-sez       =    "ERARI"
                     perform rou-stp-doc-era-000
                                          thru rou-stp-doc-era-999
           else if   rf-mpu-tip-sez       =    "INPS "
                     perform rou-stp-doc-ips-000
                                          thru rou-stp-doc-ips-999
           else if   rf-mpu-tip-sez       =    "REGIO"
                     perform rou-stp-doc-rgn-000
                                          thru rou-stp-doc-rgn-999
           else if   rf-mpu-tip-sez       =    "ENLOC"
                     perform rou-stp-doc-enl-000
                                          thru rou-stp-doc-enl-999
           else if   rf-mpu-tip-sez       =    "INAIL"
                     perform rou-stp-doc-inl-000
                                          thru rou-stp-doc-inl-999
           else if   rf-mpu-tip-sez       =    "ENPRE"
                     perform rou-stp-doc-enp-000
                                          thru rou-stp-doc-enp-999
           else      go to rou-stp-doc-800.
       rou-stp-doc-700.
      *                  *---------------------------------------------*
      *                  * Importi                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice sezione                  *
      *                      *-----------------------------------------*
           if        rf-mpu-tip-sez       =    "P    "
                     go to rou-stp-doc-800.
      *                      *-----------------------------------------*
      *                      * Importo a debito                        *
      *                      *-----------------------------------------*
           move      rf-mpu-imp-deb       to   w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      w-tot-deb-pos        to   w-stp-imp-vlb-pos      .
           move      "N"                  to   w-stp-imp-vlb-snz      .
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Importo a credito                       *
      *                      *-----------------------------------------*
           move      rf-mpu-imp-cre       to   w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      w-tot-cre-pos        to   w-stp-imp-vlb-pos      .
           move      "N"                  to   w-stp-imp-vlb-snz      .
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
       rou-stp-doc-750.
      *                  *---------------------------------------------*
      *                  * Crediti extra                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su credito ICI                     *
      *                      *-----------------------------------------*
           if        w-tot-dia-enl        =    zero
                     go to rou-stp-doc-775.
      *                      *-----------------------------------------*
      *                      * Vertical - positioning                  *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-enl        to   p-lin                  .
           add       w-tot-max-enl        to   p-lin                  .
           add       1                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      w-tot-dia-enl        to   w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      10                   to   w-stp-imp-vlb-pos      .
           move      "N"                  to   w-stp-imp-vlb-snz      .
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
       rou-stp-doc-775.
      *                  *---------------------------------------------*
      *                  * Codice ufficio e atto per sezione Erario    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo sezione                    *
      *                      *-----------------------------------------*
           if        rf-mpu-tip-sez       not  = "ERARI"
                     go to rou-stp-doc-800.
      *                      *-----------------------------------------*
      *                      * Subroutine di stampa                    *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-uea-000  thru rou-stp-doc-uea-999    .
       rou-stp-doc-800.
      *              *-------------------------------------------------*
      *              * Flag di stampa                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   rf-mpu-flg-stp         .
      *              *-------------------------------------------------*
      *              * Update [mpu]                                    *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *              *-------------------------------------------------*
      *              * Release                                         *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
       rou-stp-doc-850.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-200.
       rou-stp-doc-900.
      *              *-------------------------------------------------*
      *              * Stampa Totali documento                         *
      *              *-------------------------------------------------*
           perform   rou-stp-doc-tot-000  thru rou-stp-doc-tot-999    .
      *              *-------------------------------------------------*
      *              * Stampa dicitura per numero copia                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      68                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      52                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      21                   to   p-pos                  .
      *
           if        w-inp-ctr-cps        =    1
                     move  "1' COPIA PER LA BANCA/POSTE/AGENTE DELLA RIS
      -                    "COSSIONE"     to   p-alf
           else if   w-inp-ctr-cps        =    2
                     move  "2' COPIA PER LA BANCA/POSTE/AGENTE DELLA RIS
      -                    "COSSIONE"     to   p-alf
           else if   w-inp-ctr-cps        =    3
                     move  "COPIA PER IL SOGGETTO CHE EFFETTUA IL VERSAM
      -                    "ENTO"         to   p-alf
           else      move  spaces         to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-980.
      *              *-------------------------------------------------*
      *              * Page eject                                      *
      *              *-------------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Pre-scansione per calcolo sub-totali                      *
      *    *-----------------------------------------------------------*
       rou-stp-doc-sub-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-gen-tot          .
           move      zero                 to   w-tot-deb-era          .
           move      zero                 to   w-tot-cre-era          .
           move      zero                 to   w-tot-deb-ips          .
           move      zero                 to   w-tot-cre-ips          .
           move      zero                 to   w-tot-deb-rgn          .
           move      zero                 to   w-tot-cre-rgn          .
           move      zero                 to   w-tot-deb-enl          .
           move      zero                 to   w-tot-cre-enl          .
           move      zero                 to   w-tot-deb-inl          .
           move      zero                 to   w-tot-cre-inl          .
           move      zero                 to   w-tot-deb-enp          .
           move      zero                 to   w-tot-cre-enp          .
           move      zero                 to   w-tot-dia-enl          .
       rou-stp-doc-sub-100.
      *              *-------------------------------------------------*
      *              * Start su file [mpu]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-inp-mst-prt        to   rf-mpu-num-prt         .
           move      zero                 to   rf-mpu-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-stp-doc-sub-900.
       rou-stp-doc-sub-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mpu]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmpu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mpu                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-stp-doc-sub-900.
       rou-stp-doc-sub-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-mpu-num-prt       not  = w-inp-mst-prt
                     go to rou-stp-doc-sub-900.
       rou-stp-doc-sub-400.
      *              *-------------------------------------------------*
      *              * Incremento totali                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della sezione        *
      *                  *---------------------------------------------*
           if        rf-mpu-tip-sez       =    "ERARI"
                     add  rf-mpu-imp-deb  to   w-tot-deb-era
                     add  rf-mpu-imp-cre  to   w-tot-cre-era
           else if   rf-mpu-tip-sez       =    "INPS "
                     add  rf-mpu-imp-deb  to   w-tot-deb-ips
                     add  rf-mpu-imp-cre  to   w-tot-cre-ips
           else if   rf-mpu-tip-sez       =    "REGIO"
                     add  rf-mpu-imp-deb  to   w-tot-deb-rgn
                     add  rf-mpu-imp-cre  to   w-tot-cre-rgn
           else if   rf-mpu-tip-sez       =    "ENLOC"
                     add  rf-mpu-imp-deb  to   w-tot-deb-enl
                     add  rf-mpu-imp-cre  to   w-tot-cre-enl
           else if   rf-mpu-tip-sez       =    "INAIL"
                     add  rf-mpu-imp-deb  to   w-tot-deb-inl
                     add  rf-mpu-imp-cre  to   w-tot-cre-inl
           else if   rf-mpu-tip-sez       =    "ENPRE"
                     add  rf-mpu-imp-deb  to   w-tot-deb-enp
                     add  rf-mpu-imp-cre  to   w-tot-cre-enp
           else      go to rou-stp-doc-sub-800.
       rou-stp-doc-sub-600.
      *              *-------------------------------------------------*
      *              * Incremento totale generale                      *
      *              *-------------------------------------------------*
           add       rf-mpu-imp-deb       to   w-tot-gen-tot          .
           subtract  rf-mpu-imp-cre       from w-tot-gen-tot          .
       rou-stp-doc-sub-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-sub-200.
       rou-stp-doc-sub-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-sub-999.
       rou-stp-doc-sub-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Intestazione                                 *
      *    *-----------------------------------------------------------*
       rou-stp-doc-int-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           if        w-rds-cod-int        not  numeric
                     move  zero           to   w-rds-cod-int          .
       rou-stp-doc-int-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del codice intestatario  *
      *              *-------------------------------------------------*
           if        w-rds-cod-int        not  = zero
                     go to rou-stp-doc-int-500.
       rou-stp-doc-int-200.
      *              *-------------------------------------------------*
      *              * Se modello intestato all'azienda                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dati anagrafici azienda                     *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-azi-rag-soc          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [ada]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Lettura [ada] dati anagrafici Azienda       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Salvataggio dati azienda letti              *
      *                  *---------------------------------------------*
           move      rf-ada-via-azi       to   w-azi-ind-azi          .
           move      rf-ada-loc-azi       to   w-azi-loc-azi          .
           move      rf-ada-cod-fis       to   w-azi-cod-fis          .
           move      rf-ada-cod-cmn       to   w-azi-cod-cmn          .
       rou-stp-doc-int-300.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice Fiscale                          *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-cfi-000  thru rou-stp-doc-cfi-999    .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-rag-000  thru rou-stp-doc-rag-999    .
      *                      *-----------------------------------------*
      *                      * Localita' e indirizzo                   *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-loc-000  thru rou-stp-doc-loc-999    .
       rou-stp-doc-int-480.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-int-900.
       rou-stp-doc-int-500.
      *              *-------------------------------------------------*
      *              * Se modello intestato a fornitore                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [fnt]                          *
      *                  *---------------------------------------------*
           move      w-rds-cod-int        to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzaione file [raa]                   *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *                  *---------------------------------------------*
      *                  * Lettura file [raa]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-rds-cod-int        to   rf-raa-cod-fnt         .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
       rou-stp-doc-int-520.
      *                  *---------------------------------------------*
      *                  * Stampa in funzione del segnale di si/no     *
      *                  * persona fisica                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rf-raa-snx-prf       =    01
                     go to rou-stp-doc-int-600
           else      go to rou-stp-doc-int-700.
       rou-stp-doc-int-600.
      *                      *-----------------------------------------*
      *                      * Se Societa'                             *
      *                      *-----------------------------------------*
           move      w-let-arc-fnt-rag    to   w-azi-rag-soc          .
           move      w-let-arc-fnt-via    to   w-azi-ind-azi          .
           move      w-let-arc-fnt-loc    to   w-azi-loc-azi          .
           move      w-let-arc-fnt-cfi    to   w-azi-cod-fis          .
           move      w-let-arc-fnt-cmn    to   w-azi-cod-cmn          .
      *                      *-----------------------------------------*
      *                      * Codice Fiscale                          *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-cfi-000  thru rou-stp-doc-cfi-999    .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-rag-000  thru rou-stp-doc-rag-999    .
      *                      *-----------------------------------------*
      *                      * Localita' e indirizzo                   *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-loc-000  thru rou-stp-doc-loc-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-doc-int-900.
       rou-stp-doc-int-700.
      *                      *-----------------------------------------*
      *                      * Se Persona fisica                       *
      *                      *-----------------------------------------*
           move      rf-raa-cgn-fnt       to   w-azi-rag-cgn          .
           move      rf-raa-nom-fnt       to   w-azi-rag-nom          .
           move      rf-raa-dat-nsc       to   w-azi-dat-nsc          .
           move      rf-raa-loc-nsc       to   w-azi-loc-nsc          .
      *                          *-------------------------------------*
      *                          * Individuazione codice provincia     *
      *                          *-------------------------------------*
           move      rf-raa-loc-nsc       to   w-det-cod-prv-loc      .
           perform   det-cod-prv-000      thru det-cod-prv-999        .
           move      w-det-cod-prv-cod    to   w-azi-prv-nsc          .
           if        w-det-cod-prv-cod    =    spaces or
                     w-det-cod-prv-pos    =    zero
                     go to rou-stp-doc-int-720.
      *                          *-------------------------------------*
      *                          * Si toglie il codice provincia dalla *
      *                          * localita'                           *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-loc-nsc
                                              (w-det-cod-prv-pos : 02).
       rou-stp-doc-int-720.
           if        rf-raa-sex-fnt       =    01
                     move  "M"            to   w-azi-sex-int
           else      move  "F"            to   w-azi-sex-int          .
      *
           move      rf-raa-via-fnt       to   w-azi-ind-azi          .
           move      rf-raa-loc-fnt       to   w-azi-loc-azi          .
           move      w-let-arc-fnt-cfi    to   w-azi-cod-fis          .
           move      w-let-arc-fnt-cmn    to   w-azi-cod-cmn          .
      *                      *-----------------------------------------*
      *                      * Codice Fiscale                          *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-cfi-000  thru rou-stp-doc-cfi-999    .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-rag-000  thru rou-stp-doc-rag-999    .
      *                      *-----------------------------------------*
      *                      * Data di nascita, sesso e localita'      *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-nsc-000  thru rou-stp-doc-nsc-999    .
      *                      *-----------------------------------------*
      *                      * Localita' e indirizzo                   *
      *                      *-----------------------------------------*
           perform   rou-stp-doc-loc-000  thru rou-stp-doc-loc-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-stp-doc-int-900.
       rou-stp-doc-int-800.
      *              *-------------------------------------------------*
      *              * Codice Fiscale del coobbligato                  *
      *              *                                                 *
      *              * Campo attualmente non gestito                   *
      *              *-------------------------------------------------*
       rou-stp-doc-int-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-int-999.
       rou-stp-doc-int-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Stampa codice fiscale                        *
      *    *-----------------------------------------------------------*
       rou-stp-doc-cfi-000.
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      08                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-azi-prt-iw1          .
       rou-stp-doc-cfi-200.
           add       1                    to   w-azi-prt-iw1          .
           if        w-azi-prt-iw1        >    16
                     go to rou-stp-doc-cfi-900.
      *
           multiply  2                    by   w-azi-prt-iw1
                                        giving w-azi-prt-iw2          .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      15                   to   p-pos                  .
           add       w-azi-prt-iw2        to   p-pos                  .
           move      w-azi-cod-fis
                    (w-azi-prt-iw1 : 01)  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           go to     rou-stp-doc-cfi-200.
       rou-stp-doc-cfi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-cfi-999.
       rou-stp-doc-cfi-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Stampa ragione sociale                       *
      *    *-----------------------------------------------------------*
       rou-stp-doc-rag-000.
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      10                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * In uppercase                                    *
      *              *-------------------------------------------------*
           move      w-azi-rag-soc        to   w-all-str-alf          .
           move      70                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-azi-rag-soc          .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      70                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      17                   to   p-pos                  .
           move      w-azi-rag-soc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-rag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-rag-999.
       rou-stp-doc-rag-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Data di nascita, sesso e localita'           *
      *    *-----------------------------------------------------------*
       rou-stp-doc-nsc-000.
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      12                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-nsc-100.
      *              *-------------------------------------------------*
      *              * Preparazione data                               *
      *              *-------------------------------------------------*
           move      w-azi-dat-nsc        to   w-stp-dat-pre-dat      .
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           perform   stp-dat-edt-000      thru stp-dat-edt-999        .
      *              *-------------------------------------------------*
      *              * Stampa data                                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-azi-prt-iw1          .
       rou-stp-doc-nsc-200.
           add       1                    to   w-azi-prt-iw1          .
           if        w-azi-prt-iw1        >    08
                     go to rou-stp-doc-nsc-300.
      *
           multiply  2                    by   w-azi-prt-iw1
                                        giving w-azi-prt-iw2          .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      15                   to   p-pos                  .
           add       w-azi-prt-iw2        to   p-pos                  .
           move      w-stp-dat-pre-ded
                    (w-azi-prt-iw1 : 01)  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           go to     rou-stp-doc-nsc-200.
       rou-stp-doc-nsc-300.
      *              *-------------------------------------------------*
      *              * Sesso                                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      35                   to   p-pos                  .
           move      w-azi-sex-int        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-nsc-500.
      *              *-------------------------------------------------*
      *              * Localita' di nascita senza codice provincia     *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      45                   to   p-pos                  .
           move      w-azi-loc-nsc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-nsc-600.
      *              *-------------------------------------------------*
      *              * Provincia di nascita                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-azi-prv-nsc        =    spaces
                     go to rou-stp-doc-nsc-900.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      76                   to   p-pos                  .
           move      w-azi-prv-nsc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      78                   to   p-pos                  .
           move      w-azi-prv-nsc
                    (02 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-nsc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-nsc-999.
       rou-stp-doc-nsc-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Stampa localita' e indirizzo                 *
      *    *-----------------------------------------------------------*
       rou-stp-doc-loc-000.
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      14                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-loc-100.
      *              *-------------------------------------------------*
      *              * Localita' azienda                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura del codice provincia                *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-azi-cod-cmn        to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * In uppercase                                *
      *                  *---------------------------------------------*
           move      w-azi-loc-azi-loc    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-azi-loc-azi-loc      .
      *                  *---------------------------------------------*
      *                  * Stampa localita' senza il CAP               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      17                   to   p-pos                  .
           move      w-azi-loc-azi-loc    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Stampa provincia                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      47                   to   p-pos                  .
           move      w-let-arc-gxc-prv
                    (01 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      49                   to   p-pos                  .
           move      w-let-arc-gxc-prv
                    (02 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * In uppercase                                *
      *                  *---------------------------------------------*
           move      w-azi-ind-azi        to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-azi-ind-azi          .
      *                  *---------------------------------------------*
      *                  * Indirizzo azienda                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      52                   to   p-pos                  .
           move      w-azi-ind-azi        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-loc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-loc-999.
       rou-stp-doc-loc-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Presentazione                                *
      *    *-----------------------------------------------------------*
       rou-stp-doc-pre-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       rou-stp-doc-pre-100.
      *              *-------------------------------------------------*
      *              * Stampa preliminare intestazione                 *
      *              *                                                 *
      *              * N.B.: si presume che esista solo una presenta-  *
      *              *       zione per il modello                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Intestazione Modello                        *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-int-000  thru rou-stp-doc-int-999    .
       rou-stp-doc-pre-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cbp]                      *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-rds-cod-cbp        to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [axi]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-cbp-abi    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [axs]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-cbp-abi    to   w-let-arc-axs-abi      .
           move      w-let-arc-cbp-cab    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      03                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Denominazione istituto bancario             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      48                   to   p-pos                  .
           move      w-let-arc-axi-den    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      05                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Denominazione sportello                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      48                   to   p-pos                  .
           move      w-let-arc-axs-den    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-pre-300.
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      59                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Firma                                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      07                   to   p-pos                  .
           move      w-rds-fir-maa        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-pre-310.
      *                  *---------------------------------------------*
      *                  * Estremi del versamento                      *
      *                  *---------------------------------------------*
           if        w-tot-gen-tot        =    zero
                     go to rou-stp-doc-pre-900.
       rou-stp-doc-pre-315.
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      64                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data presentazione                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test preliminare                        *
      *                      *-----------------------------------------*
           if        w-rds-dat-pre        =    zero
                     go to rou-stp-doc-pre-340.
           if        w-rds-dat-pre        not  numeric
                     move  w-ide-pgc-sdt  to   s-sdt
                     move  s-dat          to   w-stp-dat-pre-dat
           else      move  w-rds-dat-pre  to   w-stp-dat-pre-dat      .
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           perform   stp-dat-edt-000      thru stp-dat-edt-999        .
      *                      *-----------------------------------------*
      *                      * Stampa data                             *
      *                      *-----------------------------------------*
           move      zero                 to   w-azi-prt-iw1          .
       rou-stp-doc-pre-320.
           add       1                    to   w-azi-prt-iw1          .
           if        w-azi-prt-iw1        >    08
                     go to rou-stp-doc-pre-340.
      *
           multiply  2                    by   w-azi-prt-iw1
                                        giving w-azi-prt-iw2          .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      03                   to   p-pos                  .
           add       w-azi-prt-iw2        to   p-pos                  .
           move      w-stp-dat-pre-ded
                    (w-azi-prt-iw1 : 01)  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           go to     rou-stp-doc-pre-320.
       rou-stp-doc-pre-340.
      *                  *---------------------------------------------*
      *                  * Codice ABI                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      24                   to   p-pos                  .
           move      w-let-arc-cbp-abi    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice CAB                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      36                   to   p-pos                  .
           move      w-let-arc-cbp-cab    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      66                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * C/C                                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      12                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      15                   to   p-pos                  .
           move      w-let-arc-cbp-ccb    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice ABI                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      33                   to   p-pos                  .
           move      w-let-arc-cbp-abi    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice CAB                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      43                   to   p-pos                  .
           move      w-let-arc-cbp-cab    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-pre-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-pre-999.
       rou-stp-doc-pre-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Erario                                       *
      *    *-----------------------------------------------------------*
       rou-stp-doc-era-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Trattamento eventuali codice ufficio e codice   *
      *              * atto                                            *
      *              *-------------------------------------------------*
           if        w-rds-uff-era        not  = spaces
                     move  w-rds-uff-era  to   w-det-era-uea-uff      .
           if        w-rds-att-era        not  = spaces
                     move  w-rds-att-era  to   w-det-era-uea-att      .
       rou-stp-doc-era-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore di sezione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-ctr-era          .
      *                  *---------------------------------------------*
      *                  * Test se superato massimo elementi           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-era        >    w-tot-max-era
                     go to rou-stp-doc-era-900.
       rou-stp-doc-era-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-era        to   p-lin                  .
           add       w-tot-ctr-era        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice tributo                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      24                   to   p-pos                  .
           move      w-rds-trb-era        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Rateazione                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      31                   to   p-pos                  .
           move      w-rds-rtz-era
                    (01 : 02)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      34                   to   p-pos                  .
           move      w-rds-rtz-era
                    (03 : 02)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno di riferimento                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      39                   to   p-pos                  .
           move      w-rds-ann-era        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-era-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-era-999.
       rou-stp-doc-era-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Stampa sezione Erario - ufficio e atto       *
      *    *-----------------------------------------------------------*
       rou-stp-doc-uea-000.
      *              *-------------------------------------------------*
      *              * Test se stampa da eseguire                      *
      *              *-------------------------------------------------*
           if        w-det-era-uea-uff    =    spaces and
                     w-det-era-uea-att    =    spaces
                     go to rou-stp-doc-uea-900.
       rou-stp-doc-uea-100.
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-uea-era        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Codice ufficio                                  *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      04                   to   p-pos                  .
           move      w-det-era-uea-uff
                    (01 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      06                   to   p-pos                  .
           move      w-det-era-uea-uff
                    (02 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      08                   to   p-pos                  .
           move      w-det-era-uea-uff
                    (03 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-uea-200.
      *              *-------------------------------------------------*
      *              * Codice atto                                     *
      *              *-------------------------------------------------*
           if        w-det-era-uea-att    =    spaces
                     go to rou-stp-doc-uea-900.
           move      zero                 to   w-azi-prt-iw1          .
       rou-stp-doc-uea-220.
           add       1                    to   w-azi-prt-iw1          .
           if        w-azi-prt-iw1        >    11
                     go to rou-stp-doc-uea-900.
      *
           multiply  2                    by   w-azi-prt-iw1
                                        giving w-azi-prt-iw2          .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      10                   to   p-pos                  .
           add       w-azi-prt-iw2        to   p-pos                  .
           move      w-det-era-uea-att
                    (w-azi-prt-iw1 : 01)  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           go to     rou-stp-doc-uea-220.
       rou-stp-doc-uea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-uea-999.
       rou-stp-doc-uea-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Inps                                         *
      *    *-----------------------------------------------------------*
       rou-stp-doc-ips-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       rou-stp-doc-ips-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore di sezione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-ctr-ips          .
      *                  *---------------------------------------------*
      *                  * Test se superato massimo elementi           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-ips        >    w-tot-max-ips
                     go to rou-stp-doc-ips-900.
       rou-stp-doc-ips-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-ips        to   p-lin                  .
           add       w-tot-ctr-ips        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice sede                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      03                   to   p-pos                  .
           move      w-rds-sed-ips        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Causale tributo                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      08                   to   p-pos                  .
           move      w-rds-trb-ips        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Matricola Inps                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      17                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      13                   to   p-pos                  .
           move      w-rds-mat-ips        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese iniziale                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      31                   to   p-pos                  .
           move      w-rds-mda-ips        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno iniziale                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      33                   to   p-pos                  .
           move      w-rds-ada-ips        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese finale                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      38                   to   p-pos                  .
           move      w-rds-maa-ips        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno finale                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      40                   to   p-pos                  .
           move      w-rds-aaa-ips        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-ips-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-ips-999.
       rou-stp-doc-ips-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Regione                                      *
      *    *-----------------------------------------------------------*
       rou-stp-doc-rgn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       rou-stp-doc-rgn-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore di sezione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-ctr-rgn          .
      *                  *---------------------------------------------*
      *                  * Test se superato massimo elementi           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-rgn        >    w-tot-max-rgn
                     go to rou-stp-doc-rgn-900.
       rou-stp-doc-rgn-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-rgn        to   p-lin                  .
           add       w-tot-ctr-rgn        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice regione                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      04                   to   p-pos                  .
           move      w-rds-cod-rgn
                    (01 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      06                   to   p-pos                  .
           move      w-rds-cod-rgn
                    (02 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice tributo                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      24                   to   p-pos                  .
           move      w-rds-trb-rgn        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Rateazione                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      31                   to   p-pos                  .
           move      w-rds-rtz-rgn
                    (01 : 02)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      34                   to   p-pos                  .
           move      w-rds-rtz-rgn
                    (03 : 02)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno di riferimento                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      39                   to   p-pos                  .
           move      w-rds-ann-rgn        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-rgn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-rgn-999.
       rou-stp-doc-rgn-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : ICI e Enti locali                            *
      *    *-----------------------------------------------------------*
       rou-stp-doc-enl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       rou-stp-doc-enl-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore di sezione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-ctr-enl          .
      *                  *---------------------------------------------*
      *                  * Test se superato massimo elementi           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-enl        >    w-tot-max-enl
                     go to rou-stp-doc-enl-900.
       rou-stp-doc-enl-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale Identificativo                    *
      *                  *---------------------------------------------*
           if        w-rds-ide-enl        =    spaces
                     go to rou-stp-doc-enl-300.
           if        w-tot-ctr-enl        >    1
                     go to rou-stp-doc-enl-300.
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-enl        to   p-lin                  .
           add       w-tot-ctr-enl        to   p-lin                  .
           subtract  2                    from p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      27                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      34                   to   p-pos                  .
           move      "Identificativo operazione :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Identificativo                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      62                   to   p-pos                  .
           move      w-rds-ide-enl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-enl-300.
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-enl        to   p-lin                  .
           add       w-tot-ctr-enl        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice ente                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      03                   to   p-pos                  .
           move      w-rds-cod-enl
                    (01 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      05                   to   p-pos                  .
           move      w-rds-cod-enl
                    (02 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      06                   to   p-pos                  .
           move      w-rds-cod-enl
                    (03 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      08                   to   p-pos                  .
           move      w-rds-cod-enl
                    (04 : 01)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Parametri ICI                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      11                   to   p-pos                  .
           move      w-rds-pi1-enl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      13                   to   p-pos                  .
           move      w-rds-pi2-enl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      15                   to   p-pos                  .
           move      w-rds-pi3-enl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      17                   to   p-pos                  .
           move      w-rds-pi4-enl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Immobili ICI                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      19                   to   p-pos                  .
           move      w-rds-nim-enl        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice tributo                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      24                   to   p-pos                  .
           move      w-rds-trb-enl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Rateazione                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      31                   to   p-pos                  .
           move      w-rds-rtz-enl
                    (01 : 02)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      34                   to   p-pos                  .
           move      w-rds-rtz-enl
                    (03 : 02)             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno di riferimento                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      39                   to   p-pos                  .
           move      w-rds-ann-enl        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Cumulo eventuale detrazione ICI             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test preliminari                        *
      *                      *-----------------------------------------*
           if        w-rds-dia-enl        not  numeric
                     go to rou-stp-doc-enl-900.
           if        w-rds-dia-enl        =    zero
                     go to rou-stp-doc-enl-900.
      *                      *-----------------------------------------*
      *                      * Cumulo                                  *
      *                      *-----------------------------------------*
           add       w-rds-dia-enl        to   w-tot-dia-enl          .
      *                      *-----------------------------------------*
      *                      * Incremento credito                      *
      *                      *-----------------------------------------*
           add       w-rds-dia-enl        to   w-tot-cre-enl          .
      *                      *-----------------------------------------*
      *                      * Decremento totale                       *
      *                      *-----------------------------------------*
           subtract  w-rds-dia-enl        from w-tot-gen-tot          .
       rou-stp-doc-enl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-enl-999.
       rou-stp-doc-enl-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Inail                                        *
      *    *-----------------------------------------------------------*
       rou-stp-doc-inl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       rou-stp-doc-inl-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore di sezione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-ctr-inl          .
      *                  *---------------------------------------------*
      *                  * Test se superato massimo elementi           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-inl        >    w-tot-max-inl
                     go to rou-stp-doc-inl-900.
       rou-stp-doc-inl-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-inl        to   p-lin                  .
           add       w-tot-ctr-inl        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice sede                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      14                   to   p-pos                  .
           move      w-rds-sed-inl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Posizione                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      20                   to   p-pos                  .
           move      w-rds-pos-inl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * c.c.                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      29                   to   p-pos                  .
           move      w-rds-ccp-inl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riferimento                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      33                   to   p-pos                  .
           move      w-rds-rif-inl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Causale                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      40                   to   p-pos                  .
           move      w-rds-cau-inl        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-inl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-inl-999.
       rou-stp-doc-inl-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine : Enti previdenziali                           *
      *    *-----------------------------------------------------------*
       rou-stp-doc-enp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       rou-stp-doc-enp-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore di sezione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-ctr-enp          .
      *                  *---------------------------------------------*
      *                  * Test se superato massimo elementi           *
      *                  *---------------------------------------------*
           if        w-tot-ctr-enp        >    w-tot-max-enp
                     go to rou-stp-doc-enp-900.
       rou-stp-doc-enp-200.
      *              *-------------------------------------------------*
      *              * Stampa elementi riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-vpp-enp        to   p-lin                  .
           add       w-tot-ctr-enp        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice ente                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      03                   to   p-pos                  .
           move      w-rds-cod-enp        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice sede                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      10                   to   p-pos                  .
           move      w-rds-sed-enp        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Causale                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      15                   to   p-pos                  .
           move      w-rds-cau-enp        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Posizione                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      22                   to   p-pos                  .
           move      w-rds-pos-enp        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese iniziale                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      31                   to   p-pos                  .
           move      w-rds-mda-enp        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno iniziale                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      33                   to   p-pos                  .
           move      w-rds-ada-enp        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese finale                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      38                   to   p-pos                  .
           move      w-rds-maa-enp        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Anno finale                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      40                   to   p-pos                  .
           move      w-rds-aaa-enp        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-enp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-enp-999.
       rou-stp-doc-enp-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Stampa sub-totali                                         *
      *    *-----------------------------------------------------------*
       rou-stp-doc-tot-000.
      *              *-------------------------------------------------*
      *              * Totali Erario                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-deb-era        =    zero and
                     w-tot-cre-era        =    zero
                     go to rou-stp-doc-tot-200.
      *                  *---------------------------------------------*
      *                  * Preparazione valori                         *
      *                  *---------------------------------------------*
           move      w-tot-vpp-era        to   w-tot-deb-vpp          .
           add       w-tot-max-era        to   w-tot-deb-vpp          .
           add       1                    to   w-tot-deb-vpp          .
      *
           move      w-tot-deb-era        to   w-tot-deb-stp          .
           move      w-tot-cre-era        to   w-tot-cre-stp          .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-val-000  thru rou-stp-doc-val-999    .
       rou-stp-doc-tot-200.
      *              *-------------------------------------------------*
      *              * Totali Inps                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-deb-ips        =    zero and
                     w-tot-cre-ips        =    zero
                     go to rou-stp-doc-tot-300.
      *                  *---------------------------------------------*
      *                  * Preparazione valori                         *
      *                  *---------------------------------------------*
           move      w-tot-vpp-ips        to   w-tot-deb-vpp          .
           add       w-tot-max-ips        to   w-tot-deb-vpp          .
           add       1                    to   w-tot-deb-vpp          .
      *
           move      w-tot-deb-ips        to   w-tot-deb-stp          .
           move      w-tot-cre-ips        to   w-tot-cre-stp          .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-val-000  thru rou-stp-doc-val-999    .
       rou-stp-doc-tot-300.
      *              *-------------------------------------------------*
      *              * Totali Regione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-deb-rgn        =    zero and
                     w-tot-cre-rgn        =    zero
                     go to rou-stp-doc-tot-400.
      *                  *---------------------------------------------*
      *                  * Preparazione valori                         *
      *                  *---------------------------------------------*
           move      w-tot-vpp-rgn        to   w-tot-deb-vpp          .
           add       w-tot-max-rgn        to   w-tot-deb-vpp          .
           add       1                    to   w-tot-deb-vpp          .
      *
           move      w-tot-deb-rgn        to   w-tot-deb-stp          .
           move      w-tot-cre-rgn        to   w-tot-cre-stp          .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-val-000  thru rou-stp-doc-val-999    .
       rou-stp-doc-tot-400.
      *              *-------------------------------------------------*
      *              * Totali Enti locali                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-deb-enl        =    zero and
                     w-tot-cre-enl        =    zero
                     go to rou-stp-doc-tot-500.
      *                  *---------------------------------------------*
      *                  * Preparazione valori                         *
      *                  *---------------------------------------------*
           move      w-tot-vpp-enl        to   w-tot-deb-vpp          .
           add       w-tot-max-enl        to   w-tot-deb-vpp          .
           add       1                    to   w-tot-deb-vpp          .
      *
           move      w-tot-deb-enl        to   w-tot-deb-stp          .
           move      w-tot-cre-enl        to   w-tot-cre-stp          .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-val-000  thru rou-stp-doc-val-999    .
       rou-stp-doc-tot-500.
      *              *-------------------------------------------------*
      *              * Totali Inail                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-deb-inl        =    zero and
                     w-tot-cre-inl        =    zero
                     go to rou-stp-doc-tot-600.
      *                  *---------------------------------------------*
      *                  * Preparazione valori                         *
      *                  *---------------------------------------------*
           move      w-tot-vpp-inl        to   w-tot-deb-vpp          .
           add       w-tot-max-inl        to   w-tot-deb-vpp          .
           add       1                    to   w-tot-deb-vpp          .
      *
           move      w-tot-deb-inl        to   w-tot-deb-stp          .
           move      w-tot-cre-inl        to   w-tot-cre-stp          .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-val-000  thru rou-stp-doc-val-999    .
       rou-stp-doc-tot-600.
      *              *-------------------------------------------------*
      *              * Totali Enti previdenziali                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-tot-deb-enp        =    zero and
                     w-tot-cre-enp        =    zero
                     go to rou-stp-doc-tot-800.
      *                  *---------------------------------------------*
      *                  * Preparazione valori                         *
      *                  *---------------------------------------------*
           move      w-tot-vpp-enp        to   w-tot-deb-vpp          .
           add       w-tot-max-enp        to   w-tot-deb-vpp          .
           add       1                    to   w-tot-deb-vpp          .
      *
           move      w-tot-deb-enp        to   w-tot-deb-stp          .
           move      w-tot-cre-enp        to   w-tot-cre-stp          .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           perform   rou-stp-doc-val-000  thru rou-stp-doc-val-999    .
       rou-stp-doc-tot-800.
      *              *-------------------------------------------------*
      *              * Saldo finale                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Posizionamento                              *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      58                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo                                       *
      *                  *---------------------------------------------*
           move      w-tot-gen-tot        to   w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      w-tot-tot-pos        to   w-stp-imp-vlb-pos      .
           move      "S"                  to   w-stp-imp-vlb-snz      .
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
       rou-stp-doc-tot-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-tot-999.
       rou-stp-doc-tot-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Stampa sub-totali                                         *
      *    *                                                           *
      *    * Stampa valori                                             *
      *    *-----------------------------------------------------------*
       rou-stp-doc-val-000.
      *              *-------------------------------------------------*
      *              * Stampa elementi preparati                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Posizionamento                              *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-tot-deb-vpp        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale debito                               *
      *                  *---------------------------------------------*
           move      w-tot-deb-stp        to   w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      w-tot-deb-pos        to   w-stp-imp-vlb-pos      .
           move      "N"                  to   w-stp-imp-vlb-snz      .
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Totale credito                              *
      *                  *---------------------------------------------*
           move      w-tot-cre-stp        to   w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      w-tot-cre-pos        to   w-stp-imp-vlb-pos      .
           move      "N"                  to   w-stp-imp-vlb-snz      .
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Segno                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           if        w-tot-cre-stp        >    w-tot-deb-stp
                     move  "-"            to   w-stp-imp-vlb-sgn
           else      move  "+"            to   w-stp-imp-vlb-sgn      .
           if        w-tot-cre-stp        =    w-tot-deb-stp
                     move  spaces         to   w-stp-imp-vlb-sgn      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      068                  to   p-pos                  .
           move      w-stp-imp-vlb-sgn    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo                                       *
      *                  *---------------------------------------------*
           move      w-tot-deb-stp        to   w-stp-imp-vlb-imp      .
           subtract  w-tot-cre-stp        from w-stp-imp-vlb-imp      .
           move      p-lnr                to   w-stp-imp-vlb-lin      .
           move      w-tot-tot-pos        to   w-stp-imp-vlb-pos      .
      *
           if        w-tot-deb-stp        >    zero and
                     w-stp-imp-vlb-imp    =    zero
                     move  "S"            to   w-stp-imp-vlb-snz
           else      move  "N"            to   w-stp-imp-vlb-snz      .
      *
           perform   stp-imp-vlb-000      thru stp-imp-vlb-999        .
       rou-stp-doc-val-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-val-999.
       rou-stp-doc-val-999.
           exit.

      *    *===========================================================*
      *    * Stampa importo in valuta base                             *
      *    *-----------------------------------------------------------*
       stp-imp-vlb-000.
      *              *-------------------------------------------------*
      *              * Test se zero                                    *
      *              *-------------------------------------------------*
           if        w-stp-imp-vlb-imp    >    zero
                     go to stp-imp-vlb-100.
       stp-imp-vlb-050.
      *              *-------------------------------------------------*
      *              * Se zero                                         *
      *              *-------------------------------------------------*
           if        w-stp-imp-vlb-snz    =    "N"
                     go to stp-imp-vlb-900.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      w-stp-imp-vlb-lin    to   p-lin                  .
           move      w-stp-imp-vlb-pos    to   p-pos                  .
           move      "        000"        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     stp-imp-vlb-900.
       stp-imp-vlb-100.
      *              *-------------------------------------------------*
      *              * Test se interi a zero                           *
      *              *-------------------------------------------------*
           if        w-stp-imp-vlb-int    =    zero
                     move  "        0"    to   w-all-str-alf
                     go to stp-imp-vlb-150.
      *              *-------------------------------------------------*
      *              * Editing degli interi                            *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-stp-imp-vlb-int    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Allineamento a destra degli interi              *
      *              *-------------------------------------------------*
           move      09                   to   w-all-str-lun          .
           move      p-edt                to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
       stp-imp-vlb-150.
      *              *-------------------------------------------------*
      *              * Stampa degli interi                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      w-stp-imp-vlb-lin    to   p-lin                  .
           move      w-stp-imp-vlb-pos    to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-imp-vlb-200.
      *              *-------------------------------------------------*
      *              * Editing dei decimali                            *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-imp-vlb-dec    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Allineamento a destra dei decimali              *
      *              *-------------------------------------------------*
           move      02                   to   w-all-str-lun          .
           move      p-edt                to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *              *-------------------------------------------------*
      *              * Stampa dei decimali                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      w-stp-imp-vlb-lin    to   p-lin                  .
           move      w-stp-imp-vlb-pos    to   p-pos                  .
           add       09                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-imp-vlb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-imp-vlb-999.
       stp-imp-vlb-999.
           exit.

      *    *===========================================================*
      *    * Stampa data editata 'G G M M A A A A'                     *
      *    *-----------------------------------------------------------*
       stp-dat-edt-000.
      *              *-------------------------------------------------*
      *              * Editing giorno                                  *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-pre-dat    to   s-dat                  .
           move      s-gio                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (1)      .
      *              *-------------------------------------------------*
      *              * Editing mese                                    *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-pre-dat    to   s-dat                  .
           move      s-mes                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Editing anno                                    *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-pre-dat    to   s-dat                  .
           move      s-saa                to   p-num                  .
           add       1900                 to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (3)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio data                               *
      *              *-------------------------------------------------*
           move      08                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Valore editato                                  *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-stp-dat-pre-ded      .
       stp-dat-edt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-dat-edt-999.
       stp-dat-edt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione codice provincia da stringa localita'      *
      *    *-----------------------------------------------------------*
       det-cod-prv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-cod-prv-cod      .
           move      zero                 to   w-det-cod-prv-pos      .
       det-cod-prv-100.
      *              *-------------------------------------------------*
      *              * Determinazione lunghezza stringa localita'      *
      *              *-------------------------------------------------*
           move      w-det-cod-prv-loc    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
       det-cod-prv-200.
      *              *-------------------------------------------------*
      *              * Determinazione contatori di comodo              *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-det-cod-prv-c01      .
           if        w-det-cod-prv-c01    not  > 05
                     go to det-cod-prv-900.
           subtract  2                    from w-det-cod-prv-c01
                                        giving w-det-cod-prv-cx2      .
           subtract  1                    from w-det-cod-prv-c01
                                        giving w-det-cod-prv-cx1      .
       det-cod-prv-300.
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che esista lo spazio prima del codice pro-  *
      *                  * vincia                                      *
      *                  *---------------------------------------------*
           if        w-det-cod-prv-loc
                    (w-det-cod-prv-cx2 : 01)
                                          not  = spaces
                     go to det-cod-prv-900.
      *                  *---------------------------------------------*
      *                  * Che il codice provincia non contenga spazi  *
      *                  *---------------------------------------------*
           if        w-det-cod-prv-loc
                    (w-det-cod-prv-cx1 : 01)
                                          =    spaces
                     go to det-cod-prv-900.
           if        w-det-cod-prv-loc
                    (w-det-cod-prv-c01 : 01)
                                          =    spaces
                     go to det-cod-prv-900.
       det-cod-prv-400.
      *              *-------------------------------------------------*
      *              * Estrazione del codice provincia                 *
      *              *-------------------------------------------------*
           move      w-det-cod-prv-loc
                    (w-det-cod-prv-cx1 : 02)
                                          to   w-det-cod-prv-cod      .
      *              *-------------------------------------------------*
      *              * Posizione del codice provincia                  *
      *              *-------------------------------------------------*
           move      w-det-cod-prv-cx1    to   w-det-cod-prv-pos      .
       det-cod-prv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cod-prv-999.
       det-cod-prv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il tipo elemento             *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-tip    =    "C"
                     go to let-arc-gxc-100
           else if   w-let-arc-gxc-tip    =    "F"
                     go to let-arc-gxc-200
           else if   w-let-arc-gxc-tip    =    "L"
                     go to let-arc-gxc-300.
       let-arc-gxc-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Comune                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Comune                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Frazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-fzn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Frazione                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Localita'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-lct    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Localita'                           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      w-let-arc-gxc-lct    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Se codice elemento a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Se codice elemento non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non trovato                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-600.
      *              *-------------------------------------------------*
      *              * Se codice elemento esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in work area                *
      *                  *---------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cbp]                         *
      *    *-----------------------------------------------------------*
       let-arc-cbp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-cbp-cod    =    spaces
                     go to let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP    "         to   f-key                  .
           move      w-let-arc-cbp-tip    to   rf-cbp-tip-cbp         .
           move      w-let-arc-cbp-cod    to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cbp-400.
       let-arc-cbp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cbp-des-cbp       to   w-let-arc-cbp-des      .
           move      rf-cbp-stc-csc       to   w-let-arc-cbp-csc      .
           move      rf-cbp-stc-csa       to   w-let-arc-cbp-csa      .
           move      rf-cbp-abi-ban       to   w-let-arc-cbp-abi      .
           move      rf-cbp-cab-ban       to   w-let-arc-cbp-cab      .
           move      rf-cbp-sgl-ccb       to   w-let-arc-cbp-ccb      .
           move      rf-cbp-stc-cco       to   w-let-arc-cbp-cco      .
           move      rf-cbp-stc-bba       to   w-let-arc-cbp-bba      .
           move      rf-cbp-stc-bbp       to   w-let-arc-cbp-bbp      .
           move      rf-cbp-stc-psm       to   w-let-arc-cbp-psm      .
           move      rf-cbp-stc-psi       to   w-let-arc-cbp-psi      .
           move      rf-cbp-stc-pdi       to   w-let-arc-cbp-pdi      .
           move      rf-cbp-stc-psc       to   w-let-arc-cbp-psc      .
           move      rf-cbp-stc-anv       to   w-let-arc-cbp-anv      .
           move      rf-cbp-stc-dfp       to   w-let-arc-cbp-dfp      .
           move      rf-cbp-stc-dfa       to   w-let-arc-cbp-dfa      .
           move      rf-cbp-stc-onb       to   w-let-arc-cbp-onb      .
           move      rf-cbp-stc-inb       to   w-let-arc-cbp-inb      .
           move      rf-cbp-sgl-ccp       to   w-let-arc-cbp-ccp      .
           move      rf-cbp-stc-bpa       to   w-let-arc-cbp-bpa      .
           move      rf-cbp-stc-bpp       to   w-let-arc-cbp-bpp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cbp-999.
       let-arc-cbp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cbp-flg      .
           move      all   "."            to   w-let-arc-cbp-des      .
           go to     let-arc-cbp-600.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-600.
           move      zero                 to   w-let-arc-cbp-csc      .
           move      zero                 to   w-let-arc-cbp-csa      .
           move      zero                 to   w-let-arc-cbp-abi      .
           move      zero                 to   w-let-arc-cbp-cab      .
           move      spaces               to   w-let-arc-cbp-ccb      .
           move      zero                 to   w-let-arc-cbp-cco      .
           move      zero                 to   w-let-arc-cbp-bba      .
           move      zero                 to   w-let-arc-cbp-bbp      .
           move      zero                 to   w-let-arc-cbp-psm      .
           move      zero                 to   w-let-arc-cbp-psi      .
           move      zero                 to   w-let-arc-cbp-pdi      .
           move      zero                 to   w-let-arc-cbp-psc      .
           move      zero                 to   w-let-arc-cbp-anv      .
           move      zero                 to   w-let-arc-cbp-dfp      .
           move      zero                 to   w-let-arc-cbp-dfa      .
           move      zero                 to   w-let-arc-cbp-onb      .
           move      zero                 to   w-let-arc-cbp-inb      .
           move      spaces               to   w-let-arc-cbp-ccp      .
           move      zero                 to   w-let-arc-cbp-bpa      .
           move      zero                 to   w-let-arc-cbp-bpp      .
       let-arc-cbp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODfnt    "         to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-cod-fis       to   w-let-arc-fnt-cfi      .
           move      rf-fnt-cod-cmn       to   w-let-arc-fnt-cmn      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-600.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-600.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      spaces               to   w-let-arc-fnt-cfi      .
           move      zero                 to   w-let-arc-fnt-cmn      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axi]                         *
      *    *-----------------------------------------------------------*
       let-arc-axi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axi-cod    =    zero
                     go to let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      w-let-arc-axi-cod    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axi-400.
       let-arc-axi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axi-den-abi       to   w-let-arc-axi-den      .
           move      rf-axi-cir-itb       to   w-let-arc-axi-cib      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axi-999.
       let-arc-axi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axi-flg      .
           move      all   "."            to   w-let-arc-axi-den      .
           go to     let-arc-axi-600.
       let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-den      .
       let-arc-axi-600.
           move      spaces               to   w-let-arc-axi-cib      .
       let-arc-axi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axs]                         *
      *    *-----------------------------------------------------------*
       let-arc-axs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-abi    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Test se codice agenzia a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-cab    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB"             to   f-key                  .
           move      w-let-arc-axs-abi    to   rf-axs-cod-abi         .
           move      w-let-arc-axs-cab    to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axs-400.
       let-arc-axs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axs-den-spt       to   w-let-arc-axs-den      .
           move      rf-axs-via-spt       to   w-let-arc-axs-via      .
           move      rf-axs-loc-spt       to   w-let-arc-axs-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axs-999.
       let-arc-axs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axs-flg      .
           move      all   "."            to   w-let-arc-axs-den      .
           go to     let-arc-axs-600.
       let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-den      .
       let-arc-axs-600.
           move      spaces               to   w-let-arc-axs-via      .
           move      spaces               to   w-let-arc-axs-loc      .
       let-arc-axs-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

