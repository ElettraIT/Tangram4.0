       Identification Division.
       Program-Id.                                 pazi200s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    azi                 *
      *                                Settore:    arc                 *
      *                                   Fase:    azi200              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/11/02    *
      *                       Ultima revisione:    NdK del 04/03/14    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento                  *
      *                                                                *
      *                    Documento : Scheda veicolo                  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Funzioni del modulo di stampa documento                        *
      *                                                                *
      *       - OP : Open modulo di stampa documento                   *
      *       - ST : Stampa documento                                  *
      *       - CL : Close modulo di stampa documento                  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Intercomunicazione                                             *
      *                                                                *
      *       Il programma comunica con il chiamante per mezzo di va-  *
      *       riabili di i.p.c. di pipeline dello stesso livello del   *
      *       programma chiamante.                                     *
      *                                                                *
      *       In input sara' letta la variabile : "inp-mst"            *
      *       In output sara' data la variabile : "out-mst"            *
      *                                                                *
      *       Entrambe sono definite come variabili di tipo alfanume-  *
      *       rico di 80 caratteri, ridefinite in working-storage per  *
      *       comodita' di manipolazione.                              *
      *                                                                *
      *       Il formato della variabile in input "inp-mst", una volta *
      *       spostata in working-storage, e' definito come segue nel- *
      *       la working-storage section per il record w-inp-mst.      *
      *                                                                *
      *       Il formato della variabile in output "out-mst", una vol- *
      *       ta spostata in working-storage, e' definito come segue   *
      *       nella working-storage section per il record w-out-mst.   *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Funzione OP : Open modulo di stampa documento                  *
      *                                                                *
      *                                                                *
      *       Input  : w-inp-mst-ope = Tipo operazione  : OP           *
      *                                                                *
      *                w-inp-mst-sel = Tipo selezione stampante        *
      *                                 - F : Facoltativa              *
      *                                 - O : Obbligatoria             *
      *                                                                *
      *                w-inp-mst-azi = Codice azienda                  *
      *                                                                *
      *                w-inp-mst-ter = Codice terminale                *
      *                                                                *
      *                w-inp-mst-ute = Codice utente                   *
      *                                                                *
      *                w-inp-mst-sap = Sigla sistema applicativo       *
      *                                                                *
      *                w-inp-mst-arg = Sigla area gestionale           *
      *                                                                *
      *                w-inp-mst-set = Sigla settore gestionale        *
      *                                                                *
      *                w-inp-mst-fas = Sigla fase gestionale           *
      *                                                                *
      *                w-inp-mst-dep = Descrizione del programma       *
      *                                                                *
      *                                                                *
      *       Output : w-out-mst-opn = Esito                           *
      *                                 - Spaces : Operazione corret-  *
      *                                            tamente eseguita e  *
      *                                            selezione stampante *
      *                                            effettuata          *
      *                                 - N      : Operazione corret-  *
      *                                            tamente eseguita e  *
      *                                            selezione stampante *
      *                                            facoltativa non ef- *
      *                                            fettuata            *
      *                                 - X      : Operazione non ese- *
      *                                            guita in quanto se- *
      *                                            lezione stampante   *
      *                                            obbligatoria non    *
      *                                            effettuata          *
      *                                 - E      : Errore grave in e-  *
      *                                            secuzione funzione  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Funzione CL : Close modulo di stampa documento                 *
      *                                                                *
      *                                                                *
      *       Input  : w-inp-mst-ope = Tipo operazione  : CL           *
      *                                                                *
      *                                                                *
      *       Output : w-out-mst-cls = Esito                           *
      *                                 - Spaces : Operazione corret-  *
      *                                            tamente eseguita    *
      *                                 - E      : Errore grave in e-  *
      *                                            secuzione funzione  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Funzione ST : Stampa documento                                 *
      *                                                                *
      *                                                                *
      *       Input  : w-inp-mst-ope = Tipo operazione  : ST           *
      *                                                                *
      *                w-inp-mst-vei = Identificazione del veicolo     *
      *                                                                *
      *                                                                *
      *       Output : w-out-mst-stp = Esito                           *
      *                                 - Spaces : Operazione corret-  *
      *                                            tamente eseguita    *
      *                                 - N      : Documento da stam-  *
      *                                            pare non trovato    *
      *                                 - E      : Errore grave in e-  *
      *                                            secuzione funzione  *
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
      *                * Identificazione del veicolo                   *
      *                *-----------------------------------------------*
                   15  w-inp-mst-vei      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(71)                  .
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
      *        * [gva]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfgva"                          .
      *        *-------------------------------------------------------*
      *        * [gvs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfgvs"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per contatori ed indici                         *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatori generici                                    *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine int-lin-res-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-int-lin-res.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-int-lin-res-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di interlinee                                  *
      *        *-------------------------------------------------------*
           05  w-int-lin-res-lin          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero di linee per il test sulle residue             *
      *        *-------------------------------------------------------*
           05  w-int-lin-res-res          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per routine rou-esi-gvs-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-rou-esi-gvs.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-rou-esi-gvs-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore righe                                       *
      *        *-------------------------------------------------------*
           05  w-rou-esi-gvs-ctr          pic  9(05)                  .

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
           move      096                  to   w-cnt-stp-amp-lin      .
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
           move      12,00                to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      06,00                to   w-cnt-stp-alt-int      .
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
      *              * [gva]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofgva"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gva                 .
      *              *-------------------------------------------------*
      *              * [gvs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofgvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gvs                 .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [gva]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofgva"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gva                 .
      *              *-------------------------------------------------*
      *              * [gvs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofgvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gvs                 .
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
      *              * Lettura record testata da stampare              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVEI    "         to   f-key                  .
           move      w-inp-mst-vei        to   rf-gva-cod-vei         .
           move      "pgm/azi/fls/ioc/obj/iofgva"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gva                 .
           if        f-sts                =    e-not-err
                     go to rou-stp-doc-100.
      *                  *---------------------------------------------*
      *                  * Se record non trovato uscita con status al  *
      *                  * valore "N"                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-stp          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-100.
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
                     go to rou-stp-doc-500.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-500.
      *              *-------------------------------------------------*
      *              * Stampa chiusura scheda                          *
      *              *-------------------------------------------------*
           perform   stp-chi-doc-000      thru stp-chi-doc-999        .
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-900.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-900.
      *              *-------------------------------------------------*
      *              * Page eject                                      *
      *              *-------------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *-----------------------------------------------------------*
       stp-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       stp-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Riga 1. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per codice veicolo                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Codice veicolo ......:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice veicolo                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-cod-vei       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per codice mnemonico                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      047                  to   p-pos                  .
           move      "Mnemonico .......:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      066                  to   p-pos                  .
           move      rf-gva-cod-mne       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-200.
      *              *-------------------------------------------------*
      *              * Riga 2. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per tipo veicolo                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Tipo di veicolo .....:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Tipo veicolo                                *
      *                  *                                             *
      *                  * - 01 : autoveicolo                          *
      *                  * - 02 : veicolo commerciale                  *
      *                  * - 03 : autocarro                            *
      *                  * - 04 : carrello elevatore                   *
      *                  * - 05 : motocarro                            *
      *                  * - 06 : ciclomotore                          *
      *                  * - 07 : motociclo                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
      *
           if        rf-gva-tip-vei       =    01
                     move  "Autoveicolo                             "
                                          to   p-alf
           else if   rf-gva-tip-vei       =    02
                     move  "Veicolo commerciale                     "
                                          to   p-alf
           else if   rf-gva-tip-vei       =    03
                     move  "Autocarro                               "
                                          to   p-alf
           else if   rf-gva-tip-vei       =    04
                     move  "Carrello elevatore                      "
                                          to   p-alf
           else if   rf-gva-tip-vei       =    05
                     move  "Motocarro                               "
                                          to   p-alf
           else if   rf-gva-tip-vei       =    06
                     move  "Ciclomotore                             "
                                          to   p-alf
           else if   rf-gva-tip-vei       =    07
                     move  "Motociclo                               "
                                          to   p-alf
           else      move  "?                                       "
                                          to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-300.
      *              *-------------------------------------------------*
      *              * Riga 3. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per targa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Targa ...............:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Targa                                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-sgl-trg (1)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per immatricolazione                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      047                  to   p-pos                  .
           move      "Immatricolazione :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data immatricolazione                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      066                  to   p-pos                  .
           move      rf-gva-dat-imm       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-400.
      *              *-------------------------------------------------*
      *              * Riga 4. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Marca                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Marca ...............:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Marca                                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-mrc-vei       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-500.
      *              *-------------------------------------------------*
      *              * Riga 5. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Modello                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Modello .............:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Modello                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-mod-vei       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-600.
      *              *-------------------------------------------------*
      *              * Riga 6. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per tipo motore                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Tipo di motore ......:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Tipo motore                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
      *
           if        rf-gva-tip-mot       =    01
                     move  "Benzina                                 "
                                          to   p-alf
           else if   rf-gva-tip-mot       =    02
                     move  "Diesel                                  "
                                          to   p-alf
           else if   rf-gva-tip-mot       =    03
                     move  "Elettrico                               "
                                          to   p-alf
           else      move  "?                                       "
                                          to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-700.
      *              *-------------------------------------------------*
      *              * Riga 7. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Cilindrata                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Cilindrata .....(cc).:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Cilindrata                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-cil-cmc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Portata                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      047                  to   p-pos                  .
           move      "Portata ...(q.li):"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Portata                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      066                  to   p-pos                  .
           move      rf-gva-por-qli       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-800.
      *              *-------------------------------------------------*
      *              * Riga 8. scheda                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Potenza fiscale                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Potenza fiscale (Kw) :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Potenza fiscale                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-ptz-fis       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-850.
      *              *-------------------------------------------------*
      *              * Riga Accessori                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        rf-gva-ele-acs       =    spaces
                     go to stp-tes-doc-900.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di stampa 1..10                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       stp-tes-doc-852.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to stp-tes-doc-900.
           if        rf-gva-rig-acc
                    (w-cix-ctr-001)       =    spaces
                     go to stp-tes-doc-852.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Accessori                       *
      *                  *---------------------------------------------*
           if        w-cix-ctr-001        >    1
                     go to stp-tes-doc-854.
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           move      "Accessori ...........:"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-854.
      *                  *---------------------------------------------*
      *                  * Accessori                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      026                  to   p-pos                  .
           move      rf-gva-rig-acc
                    (w-cix-ctr-001)       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-856.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-852.
       stp-tes-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
                     go to stp-tes-doc-999.
       stp-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *-----------------------------------------------------------*
       stp-cor-doc-000.
      *              *-------------------------------------------------*
      *              * Se non esistono righe per la scheda : uscita    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di esistenza righe                  *
      *                  *---------------------------------------------*
           perform   rou-esi-gvs-000      thru rou-esi-gvs-999        .
           if        w-rou-esi-gvs-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      7                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| ------------------------------------------------
      -              "------------------------------------------   |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Fincatura 1 per righe                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|                 Descrizione operazione          
      -              "             Km.         Data     Scadenza   |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Fincatura 2 per righe                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|                       Archivio                  
      -              "         Nr. docum.    Data doc              |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| ------------------------------------------------
      -              "------   -----------   --------   --------   |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-100.
      *              *-------------------------------------------------*
      *              * Start su righe documento [gvs]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODVEI    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-inp-mst-vei        to   rf-gvs-cod-vei         .
           move      zero                 to   rf-gvs-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofgvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gvs                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to stp-cor-doc-200.
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-stp          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     stp-cor-doc-999.
       stp-cor-doc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [gvs]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofgvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gvs                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-900.
       stp-cor-doc-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-gvs-cod-vei       not  = w-inp-mst-vei
                     go to stp-cor-doc-900.
       stp-cor-doc-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       stp-cor-doc-500.
      *              *-------------------------------------------------*
      *              * Stampa riga 1. documento                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione operazione                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
      *
           if        rf-gvs-tip-rig       =    "ACQ  "
                     move  "Acquisto                                "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "ASS  "
                     move  "Assicurazione                           "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "TPR  "
                     move  "Tassa di proprieta'                     "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "REV  "
                     move  "Revisione                               "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "TAG  "
                     move  "Tagliando in garanzia                   "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "MNO  "
                     move  "Manutenzione ordinaria                  "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "MNS  "
                     move  "Manutenzione straordinaria              "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "VEN  "
                     move  "Vendita                                 "
                                          to   p-alf
           else if   rf-gvs-tip-rig       =    "KLM  "
                     move  "Rilevazione chilometrica                "
                                          to   p-alf
           else      move  "?                                       "
                                          to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Km.                                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      060                  to   p-pos                  .
           move      rf-gvs-klm-prc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Allineamento a destra                   *
      *                      *-----------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      p-edt                to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      060                  to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data operazione                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      074                  to   p-pos                  .
           move      rf-gvs-dat-ope       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      085                  to   p-pos                  .
           move      rf-gvs-dat-scd       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-600.
      *              *-------------------------------------------------*
      *              * Stampa riga 2. documento                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rf-gvs-cod-arc       =    zero   and
                     rf-gvs-dat-doc       =    zero   and
                     rf-gvs-num-doc       =    spaces
                     go to stp-cor-doc-700.
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      004                  to   p-pos                  .
      *
           if        rf-gvs-cod-arc       =    zero
                     move  spaces         to   p-alf
           else if   rf-gvs-tip-arc       =    "C"
                     move  "[C]"          to   p-alf
           else if   rf-gvs-tip-arc       =    "F"
                     move  "[F]"          to   p-alf
           else      move  "[?]"          to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      rf-gvs-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-620.
      *                  *---------------------------------------------*
      *                  * Decrizione archivio                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione tipo archivio    *
      *                      *-----------------------------------------*
           if        rf-gvs-cod-arc       =    zero
                     go to  stp-cor-doc-700.
           if        rf-gvs-tip-arc       =    "C"
                     go to  stp-cor-doc-630
           else if   rf-gvs-tip-arc       =    "F"
                     go to  stp-cor-doc-640.
       stp-cor-doc-630.
      *                      *-----------------------------------------*
      *                      * Tipo archivio : Clienti                 *
      *                      *-----------------------------------------*
           move      rf-gvs-cod-arc       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           go to     stp-cor-doc-650.
       stp-cor-doc-640.
      *                      *-----------------------------------------*
      *                      * Tipo archivio : Fornitori               *
      *                      *-----------------------------------------*
           move      rf-gvs-cod-arc       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
           go to     stp-cor-doc-650.
       stp-cor-doc-650.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
      *
           if        rf-gvs-tip-arc       =    "C"
                     move  w-let-arc-cli-rag
                                          to   p-alf
           else      move  w-let-arc-fnt-rag
                                          to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-660.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      060                  to   p-pos                  .
           move      rf-gvs-num-doc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data operazione                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      074                  to   p-pos                  .
           move      rf-gvs-dat-doc       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-700.
      *              *-------------------------------------------------*
      *              * Note in riga per Stampa                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se stampa da eseguire                  *
      *                  *---------------------------------------------*
           if        rf-gvs-not-ope       =    spaces
                     go to stp-cor-doc-800.
           if        rf-gvs-rig-not (1)   =    spaces
                     go to stp-cor-doc-800.
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Riga 1.                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      005                  to   p-pos                  .
           move      rf-gvs-rig-not (1)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-720.
      *                  *---------------------------------------------*
      *                  * Riga 2.                                     *
      *                  *---------------------------------------------*
           if        rf-gvs-rig-not (2)   =    spaces
                     go to stp-cor-doc-800.
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      045                  to   p-pos                  .
           move      rf-gvs-rig-not (2)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-730.
      *                  *---------------------------------------------*
      *                  * Riga 3.                                     *
      *                  *---------------------------------------------*
           if        rf-gvs-rig-not (3)   =    spaces
                     go to stp-cor-doc-800.
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           perform   stp-rig-vuo-000      thru stp-rig-vuo-999        .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      005                  to   p-pos                  .
           move      rf-gvs-rig-not (3)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-800.
      *              *-------------------------------------------------*
      *              * Sottolineatura riga                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-cor-doc-999.
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| ------------------------------------------------
      -              "------   -----------   --------   --------   |"
                                          to   p-alf                  .
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
      *    * Routine di stampa riga vuota                              *
      *    *-----------------------------------------------------------*
       stp-rig-vuo-000.
      *              *-------------------------------------------------*
      *              * Riga vuota                                      *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      096                  to   p-pos                  .
           move      "|"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-rig-vuo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-rig-vuo-999.
       stp-rig-vuo-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa annotazioni documento                   *
      *    *-----------------------------------------------------------*
       stp-chi-doc-000.
      *              *-------------------------------------------------*
      *              * Chiusura commessa                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-chi-doc-999.
      *                  *---------------------------------------------*
      *                  * Riga vuota                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      096                  to   p-pos                  .
           move      "|"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea con Test su linee residue        *
      *                  *---------------------------------------------*
           move      1                    to   w-int-lin-res-res      .
           move      1                    to   w-int-lin-res-lin      .
           perform   int-lin-res-000      thru int-lin-res-999        .
           if        w-int-lin-res-flg    not  = spaces
                     go to stp-chi-doc-999.
      *                  *---------------------------------------------*
      *                  * Linea di '-' finale                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+-------------------------------------------------
      -              "---------------------------------------------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-chi-doc-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri intestazione standard    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "SCHEDA :"           to   w-all-str-cat (1)      .
           move      rf-gva-des-vei       to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-cnt-tit-des-tit      .
      *              *-------------------------------------------------*
      *              * Preparazione data e nr. pagina                  *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-cnt-tit-dat-stp      .
           move      p-pag                to   w-cnt-tit-num-pag      .
           add       1                    to   w-cnt-tit-num-pag      .
      *              *-------------------------------------------------*
      *              * Intestazione standard                           *
      *              *-------------------------------------------------*
           perform   int-pag-std-000      thru int-pag-std-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per interruzione forzata          *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to  int-pag-sta-999.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
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
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Linea di '-' aggiuntiva                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      96                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+-------------------------------------------------
      -              "---------------------------------------------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
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
           go to     let-arc-cli-999.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [fnt]                            *
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
           move      "CODFNT    "         to   f-key                  .
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
           go to     let-arc-fnt-999.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Interlinea generica con test su linee residue             *
      *    *-----------------------------------------------------------*
       int-lin-res-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione Flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-int-lin-res-flg      .
      *              *-------------------------------------------------*
      *              * Test su linee residue                           *
      *              *-------------------------------------------------*
           if        p-res                >    w-int-lin-res-res
                     go to int-lin-res-800.
      *                  *---------------------------------------------*
      *                  * Eventuale intestazione pagina               *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to int-lin-res-800.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     int-lin-res-900.
       int-lin-res-800.
      *              *-------------------------------------------------*
      *              * Uscita con interlinea                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      w-int-lin-res-lin    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     int-lin-res-999.
       int-lin-res-900.
      *              *-------------------------------------------------*
      *              * Uscita con Flag di errore                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-int-lin-res-flg      .
           go to     int-lin-res-999.
       int-lin-res-999.
           exit.

      *    *===========================================================*
      *    * Routine di esistenza righe corpo                          *
      *    *-----------------------------------------------------------*
       rou-esi-gvs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-rou-esi-gvs-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-rou-esi-gvs-ctr      .
       rou-esi-gvs-100.
      *              *-------------------------------------------------*
      *              * Start su righe [gvs]                            *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODVEI    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-inp-mst-vei        to   rf-gvs-cod-vei         .
           move      zero                 to   rf-gvs-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofgvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gvs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : a test su contatore *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-esi-gvs-900.
       rou-esi-gvs-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [gvs]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofgvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gvs                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end' : a test su contatore      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-esi-gvs-900.
       rou-esi-gvs-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo : a test su contatore          *
      *              *-------------------------------------------------*
           if        rf-gvs-cod-vei       not  = w-inp-mst-vei
                     go to rou-esi-gvs-900.
       rou-esi-gvs-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       rou-esi-gvs-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-rou-esi-gvs-ctr      .
       rou-esi-gvs-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     rou-esi-gvs-200.
       rou-esi-gvs-900.
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           if        w-rou-esi-gvs-ctr    =    zero
                     move  "#"            to   w-rou-esi-gvs-flg      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-esi-gvs-999.
       rou-esi-gvs-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

