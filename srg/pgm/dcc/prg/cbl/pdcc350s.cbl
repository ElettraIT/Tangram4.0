       Identification Division.
       Program-Id.                                 pdcc350s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    lst                 *
      *                                   Fase:    dcc350              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/13    *
      *                       Ultima revisione:    NdK del 02/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento                  *
      *                                                                *
      *                    Documento : Listino prezzi netti concordati *
      *                                clienti                         *
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
      *                * Codice cliente                                *
      *                *-----------------------------------------------*
                   15  w-inp-mst-cli      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Data validita'                                *
      *                *-----------------------------------------------*
                   15  w-inp-mst-vld      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Si/No Eject a fine documento                  *
      *                *                                               *
      *                *  - S : Si                                     *
      *                *  - N : No                                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ejc      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(63)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .

      *    *===========================================================*
      *    * Work-area per listino                                     *
      *    *-----------------------------------------------------------*
       01  w-lst.
      *        *-------------------------------------------------------*
      *        * Comodo per bufferizzazione intestazione cliente       *
      *        *-------------------------------------------------------*
           05  w-lst-inl-rs1              pic  x(40)                  .
           05  w-lst-inl-rs2              pic  x(40)                  .
           05  w-lst-inl-via              pic  x(40)                  .
           05  w-lst-inl-loc              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatore numero pagina                               *
      *        *-------------------------------------------------------*
           05  w-lst-num-pag              pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rsu      pic  x(40)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-rs1      pic  x(40)                  .
               10  w-let-arc-dcc-rs2      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-age      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-rsu      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-piv      pic  9(11)                  .
               10  w-let-arc-cli-stc      pic  9(07)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdx.
               10  w-let-arc-pdx-flg      pic  x(01)                  .
               10  w-let-arc-pdx-rec      pic  9(02)                  .
               10  w-let-arc-pdx-arc      pic  9(07)                  .
               10  w-let-arc-pdx-lng      pic  x(03)                  .
               10  w-let-arc-pdx-pro      pic  9(07)                  .
               10  w-let-arc-pdx-fmt      pic  x(14)                  .
               10  w-let-arc-pdx-ctr      pic  9(02)                  .
               10  w-let-arc-pdx-des.
                   15  w-let-arc-pdx-rig  occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per contatori ed indici                         *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatori generici                                    *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per ciclo di report-program                     *
      *    *-----------------------------------------------------------*
       01  w-prn.
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                               *
      *        *-------------------------------------------------------*
           05  w-prn-flg-uno              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di intestazione documento di riferimento         *
      *        *-------------------------------------------------------*
           05  w-prn-fdi-ddr              pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per bufferizzazione eventuale descrizione per   *
      *    * il cliente letta da [pdx]                                 *
      *    *-----------------------------------------------------------*
       01  w-buf-des-pdx.
      *        *-------------------------------------------------------*
      *        * Contatore di righe lette                              *
      *        *-------------------------------------------------------*
           05  w-buf-des-pdx-ctr          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-buf-des-pdx-c01          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per test su linee residue                      *
      *        *-------------------------------------------------------*
           05  w-buf-des-pdx-res          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per descrizione letta                          *
      *        *-------------------------------------------------------*
           05  w-buf-des-pdx-des.
               10  w-buf-des-pdx-der occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per coordinate di posizionamento campi piede    *
      *    *-----------------------------------------------------------*
       01  w-lpp.
      *        *-------------------------------------------------------*
      *        * Linea e posizione per literal 'Segue'                 *
      *        *-------------------------------------------------------*
           05  w-lpp-lit-seg-lin          pic  9(03)                  .
           05  w-lpp-lit-seg-pos          pic  9(03)                  .

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
      *                  * Se ciclo ripetitivo di stampa, a lettura    *
      *                  * parametri da segreteria                     *
      *                  *---------------------------------------------*
           if        w-inp-mst-sel        =    "R"
                     go to exe-fun-opn-600.
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
      *                          *-------------------------------------*
      *                          * Se ciclo ripetitivo di stampa, si   *
      *                          * deve forzare il nome per il file,   *
      *                          * passato dal chiamante               *
      *                          *                                     *
      *                          * N.B.: il nome file e' contentuto    *
      *                          *       nelle variabili               *
      *                          *      'p-sel-fut-nam' e              *
      *                          *      'p-sel-fut-sel'                *
      *                          *-------------------------------------*
           if        w-inp-mst-sel        =    "r" or
                     w-inp-mst-sel        =    "R"
                     move  w-inp-mst-dep  to   p-sel-fut-sel          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     exe-fun-opn-999.
       exe-fun-opn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-fun-opn-999.
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
           move      96                   to   w-cnt-stp-amp-lin      .
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
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
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
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Open files necessari alla stampa                          *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [lst]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [lst]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
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
       rou-stp-doc-100.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
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
       rou-stp-doc-200.
      *              *-------------------------------------------------*
      *              * Eventuale stampa intestazione pagina            *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Stampa testata documento                        *
      *              *-------------------------------------------------*
           perform   stp-tes-doc-000      thru stp-tes-doc-999        .
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-300.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-300.
      *              *-------------------------------------------------*
      *              * Stampa corpo documento                          *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-000      thru stp-cor-doc-999        .
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-400.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-400.
      *              *-------------------------------------------------*
      *              * Stampa piede documento                          *
      *              *-------------------------------------------------*
           perform   stp-pie-doc-000      thru stp-pie-doc-999        .
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-900.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-900.
      *              *-------------------------------------------------*
      *              * Page eject                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-inp-mst-ejc        =    "N"
                     go to rou-stp-doc-950.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *-----------------------------------------------------------*
       stp-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione dati di testata              *
      *                  *---------------------------------------------*
           perform   stp-tes-doc-dat-000  thru stp-tes-doc-dat-999    .
       stp-tes-doc-010.
      *              *-------------------------------------------------*
      *              * Vertical position forzata                       *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      15                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Spettabile                                      *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      057                  to   p-pos                  .
           move      "Spettabile"         to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinee multiple                             *
      *              *-------------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      3                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-030.
      *              *-------------------------------------------------*
      *              * Ragione sociale 1 cliente                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      057                  to   p-pos                  .
           move      w-lst-inl-rs1        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Ragione sociale 2 cliente                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      057                  to   p-pos                  .
           move      w-lst-inl-rs2        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      057                  to   p-pos                  .
           move      w-lst-inl-via        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Localita'                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      057                  to   p-pos                  .
           move      w-lst-inl-loc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-040.
      *              *-------------------------------------------------*
      *              * Interlinee multiple                             *
      *              *-------------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      3                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Oggetto                                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "OGGETTO: nuovo listino                            
      -              "                                              "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinee multiple                             *
      *              *-------------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      3                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Testo 1                                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Di seguito nuovo listino di vendita a voi riservat
      -              "o con decorrenza dal           , data consegna"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Data di validita'                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      "S"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-inp-mst-vld        to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Testo 2                                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "merce                                             
      -              "                                              "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-200.
      *              *-------------------------------------------------*
      *              * Fincatura per dettaglio                         *
      *              *-------------------------------------------------*
           perform   stp-fnc-det-000      thru stp-fnc-det-999        .
       stp-tes-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
                     go to stp-tes-doc-999.
       stp-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di lettura dati di testata                     *
      *    *-----------------------------------------------------------*
       stp-tes-doc-dat-000.
      *              *-------------------------------------------------*
      *              * Lettura record [dcc] principale                 *
      *              *-------------------------------------------------*
           move      w-inp-mst-cli        to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rs1    to   w-lst-inl-rs1          .
           move      w-let-arc-dcc-rs2    to   w-lst-inl-rs2          .
           move      w-let-arc-dcc-via    to   w-lst-inl-via          .
           move      w-let-arc-dcc-loc    to   w-lst-inl-loc          .
       stp-doc-dat-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
                     go to stp-tes-doc-dat-999.
       stp-tes-doc-dat-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *-----------------------------------------------------------*
       stp-cor-doc-000.
      *              *-------------------------------------------------*
      *              * Start su file [lst]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRO    "         to   f-key                  .
           move      02                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      w-inp-mst-cli        to   rf-lst-cod-cli         .
           move      spaces               to   rf-lst-sgl-vlt         .
           move      zero                 to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-900.
       stp-cor-doc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [lst]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-900.
       stp-cor-doc-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-lst-tip-rec       not  = 02     or
                     rf-lst-cod-lst       not  = spaces or
                     rf-lst-cod-cli       not  = w-inp-mst-cli
                     go to stp-cor-doc-900.
       stp-cor-doc-400.
      *              *-------------------------------------------------*
      *              * Test se righe residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    1
                     go to stp-cor-doc-500.
      *                  *---------------------------------------------*
      *                  * Reintestazione                              *
      *                  *---------------------------------------------*
           perform   rei-mod-tes-000      thru rei-mod-tes-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Stampa fincatura per dettaglio              *
      *                  *---------------------------------------------*
           perform   stp-fnc-det-000      thru stp-fnc-det-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-cor-doc-999.
       stp-cor-doc-500.
      *              *-------------------------------------------------*
      *              * Dati prodotto                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcp]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Lettura per codice                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-lst-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       stp-cor-doc-520.
      *              *-------------------------------------------------*
      *              * Descrizione per i documenti                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione preliminare descrizione     *
      *                  * interna                                     *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pdx-des      .
      *                  *---------------------------------------------*
      *                  * Test su flag di presenza della descrizione  *
      *                  * estesa : se non presente, la descrizione    *
      *                  * per i documenti e' uguale a quella interna  *
      *                  *---------------------------------------------*
           if        rf-dcp-des-pdx       =    0
                     go to stp-cor-doc-600.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [pdx] per descrizione per  *
      *                  * i documenti                                 *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-pdx-rec      .
           move      zero                 to   w-let-arc-pdx-arc      .
           move      "I  "                to   w-let-arc-pdx-lng      .
           move      rf-dcp-num-pro       to   w-let-arc-pdx-pro      .
           move      spaces               to   w-let-arc-pdx-fmt      .
           perform   let-arc-pdx-000      thru let-arc-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-pdx-flg    not  = spaces
                     go to stp-cor-doc-600.
      *                  *---------------------------------------------*
      *                  * Descrizione letta                           *
      *                  *---------------------------------------------*
           move      w-let-arc-pdx-des    to   w-buf-des-pdx-des      .
           move      w-let-arc-pdx-ctr    to   w-buf-des-pdx-ctr      .
       stp-cor-doc-600.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      rf-dcp-alf-pro       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto - riga 1               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-buf-des-pdx-der (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura per la vendita             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      059                  to   p-pos                  .
           move      rf-dcp-umi-ven       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Prezzo listino                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           add       rf-dcp-dec-prz       to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "BG"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      064                  to   p-pos                  .
           move      rf-lst-prz-lst       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Editing sconto 1                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-lst-per-sco (1)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Editing sconto 2                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-lst-per-sco (2)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (3)      .
      *                  *---------------------------------------------*
      *                  * Editing sconto 3                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-lst-per-sco (3)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (5)      .
      *                  *---------------------------------------------*
      *                  * Editing sconto 4                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-lst-per-sco (4)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (7)      .
      *                  *---------------------------------------------*
      *                  * Editing sconto 5                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-lst-per-sco (5)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (9)      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      19                   to   w-all-str-lun          .
           move      09                   to   w-all-str-num          .
      *
           if        rf-lst-per-sco (2)   =    zero
                     move  spaces         to   w-all-str-cat (2)
           else      move  "+"            to   w-all-str-cat (2)      .
      *
           if        rf-lst-per-sco (3)   =    zero
                     move  spaces         to   w-all-str-cat (4)
           else      move  "+"            to   w-all-str-cat (4)      .
      *
           if        rf-lst-per-sco (4)   =    zero
                     move  spaces         to   w-all-str-cat (6)
           else      move  "+"            to   w-all-str-cat (6)      .
      *
           if        rf-lst-per-sco (5)   =    zero
                     move  spaces         to   w-all-str-cat (8)
           else      move  "+"            to   w-all-str-cat (8)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      19                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      078                  to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-700.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto - righe successive     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da stampare                     *
      *                      *-----------------------------------------*
           if        w-buf-des-pdx-ctr    =    1
                     go to stp-cor-doc-800.
      *                      *-----------------------------------------*
      *                      * Ciclo per 'n' righe                     *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-des-pdx-c01      .
       stp-cor-doc-720.
           add       1                    to   w-buf-des-pdx-c01      .
           if        w-buf-des-pdx-c01    >    w-buf-des-pdx-ctr
                     go to stp-cor-doc-800.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-buf-des-pdx-der
                    (w-buf-des-pdx-c01)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     stp-cor-doc-720.
       stp-cor-doc-800.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il cliente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione [pdk]                   *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [pdk] per codice       *
      *                      * prodotto per il cliente                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TIPREC"             to   f-key                  .
           move      "C"                  to   rf-pdk-tip-rec         .
           move      rf-dcp-num-pro       to   rf-pdk-num-pro         .
           move      w-inp-mst-cli        to   rf-pdk-cod-arc         .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                          *-------------------------------------*
      *                          * Se non trovato                      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-890.
      *                      *-----------------------------------------*
      *                      * Composizione riga aggiuntiva            *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "- Vs. codice :"     to   w-all-str-cat (1)      .
           move      rf-pdk-alf-pro       to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
       stp-cor-doc-820.
      *                      *-----------------------------------------*
      *                      * Test se righe residue sufficienti       *
      *                      *-----------------------------------------*
           if        p-res                >    1
                     go to stp-cor-doc-840.
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-cor-doc-999.
      *                      *-----------------------------------------*
      *                      * Fincatura per dettaglio                 *
      *                      *-----------------------------------------*
           perform   int-fin-det-cli-000  thru int-fin-det-cli-999    .
       stp-cor-doc-840.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-890.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-200.
       stp-cor-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-999.
       stp-cor-doc-999.
           exit.

      *    *===========================================================*
      *    * Fincatura dettaglio                                       *
      *    *                                                           *
      *    * Stampa per il Cliente                                     *
      *    *-----------------------------------------------------------*
       int-fin-det-cli-000.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale subordinato            *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "             Codice e descrizione prodotto        
      -              "        Udm  Prezzo netto       % sconto      "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------------------------------------------------
      -              "------  ---  ------------  -------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-fin-det-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-fin-det-cli-999.
       int-fin-det-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa piede documento                         *
      *    *-----------------------------------------------------------*
       stp-pie-doc-000.
       stp-pie-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-pie-doc-999.
       stp-pie-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di reintestazione per superamento numero righe    *
      *    * previste dal modulo di stampa                             *
      *    *-----------------------------------------------------------*
       rei-mod-tes-000.
      *              *-------------------------------------------------*
      *              * Se posizionamento per il literal 'segue' e' a   *
      *              * zero : non si stampa il literal                 *
      *              *-------------------------------------------------*
           if        w-lpp-lit-seg-lin    =    zero or
                     w-lpp-lit-seg-pos    =    zero
                     go to rei-mod-tes-100.
      *              *-------------------------------------------------*
      *              * Literal 'segue'                                 *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      w-lpp-lit-seg-lin    to   p-lin                  .
           move      w-lpp-lit-seg-pos    to   p-pos                  .
           move      "*** Segue ***"      to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rei-mod-tes-100.
      *              *-------------------------------------------------*
      *              * Incremento numero pagina                        *
      *              *-------------------------------------------------*
           add       1                    to   w-lst-num-pag          .
       rei-mod-tes-200.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  rei-mod-tes-999.
       rei-mod-tes-400.
      *              *-------------------------------------------------*
      *              * Eventuale intestazione pagina                   *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Routine di stampa dati di testata               *
      *              *-------------------------------------------------*
           perform   stp-tes-doc-000      thru stp-tes-doc-999        .
       rei-mod-tes-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina stampa                                *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Eventuale impaginazione                         *
      *              *-------------------------------------------------*
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Fincatura per dettaglio                                   *
      *    *-----------------------------------------------------------*
       stp-fnc-det-000.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale subordinato            *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "             Codice e descrizione prodotto        
      -              "        Udm  Prezzo netto       % sconto      "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------------------------------------------------
      -              "------  ---  ------------  -------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-det-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici a zero o spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-key       to   w-let-arc-dcc-rsu      .
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-rs1-doc       to   w-let-arc-dcc-rs1      .
           move      rf-dcc-rs2-doc       to   w-let-arc-dcc-rs2      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
           move      rf-dcc-cod-age       to   w-let-arc-dcc-age      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-520.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-520.
           move      spaces               to   w-let-arc-dcc-rsu      .
           move      spaces               to   w-let-arc-dcc-rs1      .
           move      spaces               to   w-let-arc-dcc-rs2      .
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      zero                 to   w-let-arc-dcc-age      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-rag-key       to   w-let-arc-cli-rsu      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-prt-iva       to   w-let-arc-cli-piv      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-stc      .
           move      rf-cli-cod-iva       to   w-let-arc-cli-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-520.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-520.
           move      spaces               to   w-let-arc-cli-rsu      .
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-piv      .
           move      zero                 to   w-let-arc-cli-stc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [pdx] per scansione delle righe   *
      *    *-----------------------------------------------------------*
       let-arc-pdx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdx-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-arc-pdx-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdx-des      .
       let-arc-pdx-100.
      *              *-------------------------------------------------*
      *              * Start su file [pdx]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      w-let-arc-pdx-rec    to   rf-pdx-tip-rec         .
           move      w-let-arc-pdx-arc    to   rf-pdx-cod-arc         .
           move      w-let-arc-pdx-lng    to   rf-pdx-cod-lng         .
           move      w-let-arc-pdx-pro    to   rf-pdx-cod-num         .
           move      w-let-arc-pdx-fmt    to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-pdx-800.
       let-arc-pdx-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdx]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-pdx-800.
       let-arc-pdx-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-pdx-tip-rec       not  = w-let-arc-pdx-rec or
                     rf-pdx-cod-arc       not  = w-let-arc-pdx-arc or
                     rf-pdx-cod-lng       not  = w-let-arc-pdx-lng or
                     rf-pdx-cod-num       not  = w-let-arc-pdx-pro or
                     rf-pdx-for-mat       not  = w-let-arc-pdx-fmt
                     go to let-arc-pdx-800.
       let-arc-pdx-400.
      *              *-------------------------------------------------*
      *              * Test su contenuto descrizione                   *
      *              *-------------------------------------------------*
           if        rf-pdx-des-pro       =    spaces
                     go to let-arc-pdx-200.
       let-arc-pdx-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-let-arc-pdx-ctr      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-pdx-rig
                                              (w-let-arc-pdx-ctr)     .
       let-arc-pdx-600.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura                              *
      *              *-------------------------------------------------*
           go to     let-arc-pdx-200.
       let-arc-pdx-800.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi letti                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdx-ctr    =    zero
                     go to let-arc-pdx-820
           else      go to let-arc-pdx-900.
       let-arc-pdx-820.
      *                  *---------------------------------------------*
      *                  * Se nessun elemento rilevato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di nessun elemento rilevato        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-pdx-flg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     let-arc-pdx-900.
       let-arc-pdx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdx-999.
       let-arc-pdx-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

