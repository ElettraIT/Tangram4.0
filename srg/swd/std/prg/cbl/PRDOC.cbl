       Identification Division.
       Program-Id.                                 p___nnn0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    ___                 *
      *                                Settore:    ___                 *
      *                                   Fase:    ______              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del __/__/__    *
      *                       Ultima revisione:    NdK del __/__/__    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento                  *
      *                                                                *
      *                    Documento : _______________________________ *
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
      *                w-inp-mst-___ = Identificazione del documento   *
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
      *                * Identificazione del documento                 *
      *                *-----------------------------------------------*
                   15  w-inp-mst-___      pic  _(__)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(__)                  .
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
      *        * [___]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/___/fls/rec/rf___"                          .

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
      *              * Codice modulo ______                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "_"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      ___                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      _                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      __                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      _                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri ____                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea ____                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
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
           ____
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
           ____
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *-----------------------------------------------------------*
       rou-stp-doc-000.
           ____



______________ normalmente e' necessaria questa istruzione alla fine _______
      *              *-------------------------------------------------*
      *              * Page eject                                      *
      *              *-------------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-999.
           exit.
