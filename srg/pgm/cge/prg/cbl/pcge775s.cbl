       Identification Division.
       Program-Id.                                 pcge775s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    iva                 *
      *                                   Fase:    cge775              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/09/17    *
      *                       Ultima revisione:    NdK del 21/02/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento                  *
      *                                                                *
      *                    Modello di Comunicazione IVA Trimestrale    *
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
      *                * Identificazione del documento                 *
      *                *-----------------------------------------------*
                   15  w-inp-tip-stp      pic  x(01)                  .
                   15  w-inp-ann-dic      pic  9(03)                  .
                   15  w-inp-mes-dic      pic  9(02)                  .
                   15  w-inp-ctr-mod      pic  9(01)                  .
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

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ivc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfivc"                          .
      *        *-------------------------------------------------------*
      *        * [zti]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzti"                          .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ivc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ivc.
               10  w-let-arc-ivc-flg      pic  x(01)                  .
               10  w-let-arc-ivc-saa      pic  9(03)                  .
               10  w-let-arc-ivc-mes      pic  9(02)                  .
               10  w-let-arc-ivc-top      pic  9(04)                  .
               10  w-let-arc-ivc-val      pic  x(20)                  .
               10  w-let-arc-ivc-sta      pic  x(33)                  .

      *    *===========================================================*
      *    * Work-area per stampa                                      *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Work per stampa singolo elemento                      *
      *        *-------------------------------------------------------*
           05  w-stp-sng-ele.
      *            *---------------------------------------------------*
      *            * Secolo, anno                                      *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese                                              *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-mes      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo operazione                                   *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-top      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Linea di stampa                                   *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-lin      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione di stampa                               *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-pos      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Tipo elemento                                     *
      *            *                                                   *
      *            * - 'M' : Mese                                      *
      *            * - 'V' : Valore                                    *
      *            * - 'N' : Numero                                    *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-edt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Comodi per la stampa                              *
      *            *---------------------------------------------------*
               10  w-stp-sng-ele-num      pic s9(10)v9(02)            .
               10  w-stp-sng-ele-sta      pic  x(33)                  .
               10  w-stp-sng-ele-mod      pic  x(04)                  .

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
      *              * [ivc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofivc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivc                 .
      *              *-------------------------------------------------*
      *              * [zti]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzti"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zti                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [ivc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofivc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivc                 .
      *              *-------------------------------------------------*
      *              * [zti]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzti"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zti                 .
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
      *              * Deviazione in funzione della sezione            *
      *              *-------------------------------------------------*
           if        w-inp-tip-stp        =    "F"
                     perform  rou-stp-doc-frn-000
                                          thru rou-stp-doc-frn-999
           else      perform  rou-stp-doc-det-000
                                          thru rou-stp-doc-det-999    .
       rou-stp-doc-800.
      *              *-------------------------------------------------*
      *              * Page eject                                      *
      *              *-------------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine di stampa frontespizio                         *
      *    *-----------------------------------------------------------*
       rou-stp-doc-frn-000.
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
                     go to  rou-stp-doc-frn-999.
       rou-stp-doc-frn-100.
      *              *-------------------------------------------------*
      *              * Lettura : 2110 - Codice Fiscale                 *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2110                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      03                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      34                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-150.
      *              *-------------------------------------------------*
      *              * Lettura : 2120 - Anno di imposta                *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2120                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      10                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      22                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-200.
      *              *-------------------------------------------------*
      *              * Lettura : 2130 - Partita Iva                    *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2130                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      13                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      25                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      27                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-250.
      *              *-------------------------------------------------*
      *              * Lettura : 2170 - Codice Fiscale dichiarante     *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2170                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      19                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      33                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      24                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-275.
      *              *-------------------------------------------------*
      *              * Lettura : 2180 - Codice carica dichiarante      *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2180                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      19                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      78                   to   p-pos                  .
           move      w-let-arc-ivc-val    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-300.
      *              *-------------------------------------------------*
      *              * Lettura : 2210 - Codice Fiscale Intermediario   *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2210                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      26                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      32                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      28                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-350.
      *              *-------------------------------------------------*
      *              * Lettura : 2220 - Impegno alla Presentazione     *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2220                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      29                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      26                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-400.
      *              *-------------------------------------------------*
      *              * Lettura : 2230 - Data Impegno                   *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2230                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      33                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      21                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-frn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-frn-999.
       rou-stp-doc-frn-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine di stampa dettaglio                            *
      *    *-----------------------------------------------------------*
       rou-stp-doc-det-000.
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
                     go to  rou-stp-doc-det-999.
       rou-stp-doc-det-030.
      *              *-------------------------------------------------*
      *              * Lettura : 2110 - Codice Fiscale                 *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-let-arc-ivc-saa      .
           move      zero                 to   w-let-arc-ivc-mes      .
           move      2110                 to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      03                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      33                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-det-050.
      *              *-------------------------------------------------*
      *              * Numero modello                                  *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-inp-ctr-mod        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-sng-ele-mod      .
      *              *-------------------------------------------------*
      *              * Spaziatura valore                               *
      *              *-------------------------------------------------*
           move      04                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-stp-sng-ele-mod    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      07                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      61                   to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-det-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [zti]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOP    "         to   f-key                  .
           move      5000                 to   rf-zti-cod-top         .
           move      "pgm/cge/fls/ioc/obj/iofzti"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zti                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-stp-doc-det-900.
       rou-stp-doc-det-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [zti]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzti"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zti                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-stp-doc-det-900.
       rou-stp-doc-det-300.
      *              *-------------------------------------------------*
      *              * Max su [zti]                                    *
      *              *-------------------------------------------------*
           if        rf-zti-cod-top       >    5999
                     go to rou-stp-doc-det-900.
       rou-stp-doc-det-500.
      *              *-------------------------------------------------*
      *              * Linea, posizione ed editing in funzione del     *
      *              * codice tipo operazione                          *
      *              *                                                 *
      *              * - 5010 : Mese                                   *
      *              *                                                 *
      *              * - 5050 : Totale Operazioni Attive          VP2  *
      *              * - 5060 : Totale Operazioni Passive         VP3  *
      *              * - 5070 : Iva Esigibile                     VP4  *
      *              * - 5080 : Iva Detratta                      VP5  *
      *              * - 5090 : Iva Dovuta                        VP6  *
      *              * - 5100 : Iva Credito                       VP6  *
      *              * - 5110 : Debito Periodo Precedente non s.  VP7  *
      *              * - 5120 : Credito Periodo Precedente        VP8  *
      *              * - 5130 : Credito Anno Precedente           VP9  *
      *              * - 5140 : Versamenti Auto UE                VP10 *
      *              * - 5150 : Crediti imposta                   VP11 *
      *              * - 5160 : Interessi Dovuti                  VP12 *
      *              * - 5165 : Metodo                            VP13 *
      *              * - 5170 : Acconto                           VP13 *
      *              * - 5180 : Importo Da Versare                VP14 *
      *              * - 5190 : Importo A Credito                 VP14 *
      *              *-------------------------------------------------*
           if        rf-zti-cod-top       =    5010
                     move  12             to   w-stp-sng-ele-lin
                     move  18             to   w-stp-sng-ele-pos
                     move  "M"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5050
                     move  14             to   w-stp-sng-ele-lin
                     move  37             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5060
                     move  16             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5070
                     move  18             to   w-stp-sng-ele-lin
                     move  37             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5080
                     move  20             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5090
                     move  22             to   w-stp-sng-ele-lin
                     move  37             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5100
                     move  22             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5110
                     move  24             to   w-stp-sng-ele-lin
                     move  37             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5120
                     move  26             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5130
                     move  28             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5140
                     move  30             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5150
                     move  32             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5160
                     move  34             to   w-stp-sng-ele-lin
                     move  37             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5165
                     move  36             to   w-stp-sng-ele-lin
                     move  56             to   w-stp-sng-ele-pos
                     move  "N"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5170
                     move  36             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5180
                     move  38             to   w-stp-sng-ele-lin
                     move  37             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt
           else if   rf-zti-cod-top       =    5190
                     move  38             to   w-stp-sng-ele-lin
                     move  59             to   w-stp-sng-ele-pos
                     move  "V"            to   w-stp-sng-ele-edt      .
       rou-stp-doc-det-600.
      *              *-------------------------------------------------*
      *              * Emissione elemento                              *
      *              *-------------------------------------------------*
           move      w-inp-ann-dic        to   w-stp-sng-ele-saa      .
           move      w-inp-mes-dic        to   w-stp-sng-ele-mes      .
           move      rf-zti-cod-top       to   w-stp-sng-ele-top      .
           perform   rou-stp-doc-ele-000  thru rou-stp-doc-ele-999    .
       rou-stp-doc-det-800.
      *              *-------------------------------------------------*
      *              * Riciclo su [zti]                                *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-det-200.
       rou-stp-doc-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-det-999.
       rou-stp-doc-det-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine di emissione singolo elemento                  *
      *    *-----------------------------------------------------------*
       rou-stp-doc-ele-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ivc-sta      .
      *              *-------------------------------------------------*
      *              * Lettura valore                                  *
      *              *-------------------------------------------------*
           move      w-stp-sng-ele-saa    to   w-let-arc-ivc-saa      .
           move      w-stp-sng-ele-mes    to   w-let-arc-ivc-mes      .
           move      w-stp-sng-ele-top    to   w-let-arc-ivc-top      .
           perform   let-arc-ivc-000      thru let-arc-ivc-999        .
      *              *-------------------------------------------------*
      *              * Test su valore letto                            *
      *              *-------------------------------------------------*
           if        w-let-arc-ivc-val    =    spaces
                     go to rou-stp-doc-ele-900.
      *              *-------------------------------------------------*
      *              * In campo per la stampa                          *
      *              *-------------------------------------------------*
           move      w-let-arc-ivc-val    to   w-stp-sng-ele-sta      .
       rou-stp-doc-ele-100.
      *              *-------------------------------------------------*
      *              * Eventuale spaziatura                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-stp-sng-ele-edt    not  = "S"
                     go to rou-stp-doc-ele-200.
      *                  *---------------------------------------------*
      *                  * Spaziatura valore                           *
      *                  *---------------------------------------------*
           move      16                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-let-arc-ivc-val    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
           move      w-all-str-alf        to   w-stp-sng-ele-sta      .
      *                  *---------------------------------------------*
      *                  * A stampa                                    *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-ele-600.
       rou-stp-doc-ele-200.
      *              *-------------------------------------------------*
      *              * Eventuale valore numerico                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-stp-sng-ele-edt    not  = "V"
                     go to rou-stp-doc-ele-300.
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      20                   to   p-car                  .
           move      w-stp-sng-ele-sta    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-stp-sng-ele-num      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<BG"                to   p-edm                  .
           move      w-stp-sng-ele-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-sng-ele-sta      .
      *                  *---------------------------------------------*
      *                  * Allineamento a destra                       *
      *                  *---------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      w-stp-sng-ele-sta    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
           move      w-all-str-alf        to   w-stp-sng-ele-sta      .
      *                  *---------------------------------------------*
      *                  * A stampa                                    *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-ele-600.
       rou-stp-doc-ele-300.
      *              *-------------------------------------------------*
      *              * Eventuale Mese                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-stp-sng-ele-edt    not  = "M"
                     go to rou-stp-doc-ele-400.
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      20                   to   p-car                  .
           move      w-stp-sng-ele-sta    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-stp-sng-ele-num      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-sng-ele-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-sng-ele-sta      .
      *                  *---------------------------------------------*
      *                  * Spaziatura valore                           *
      *                  *---------------------------------------------*
           move      02                   to   w-all-str-lun          .
           move      01                   to   w-all-str-num          .
           move      w-stp-sng-ele-sta    to   w-all-str-alf          .
           perform   all-str-spz-000      thru all-str-spz-999        .
           move      w-all-str-alf        to   w-stp-sng-ele-sta      .
      *                  *---------------------------------------------*
      *                  * A stampa                                    *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-ele-600.
       rou-stp-doc-ele-400.
      *              *-------------------------------------------------*
      *              * Eventuale Numero di un carattere                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-stp-sng-ele-edt    not  = "N"
                     go to rou-stp-doc-ele-600.
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      01                   to   p-car                  .
           move      w-stp-sng-ele-sta    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-stp-sng-ele-num      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-sng-ele-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-sng-ele-sta      .
      *                  *---------------------------------------------*
      *                  * Vertical - positioning                      *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-stp-sng-ele-lin    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sng-ele-pos    to   p-pos                  .
           move      w-stp-sng-ele-sta    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-stp-doc-ele-900.
       rou-stp-doc-ele-600.
      *              *-------------------------------------------------*
      *              * Vertical - positioning                          *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      w-stp-sng-ele-lin    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sng-ele-pos    to   p-pos                  .
           move      w-stp-sng-ele-sta    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-ele-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-ele-999.
       rou-stp-doc-ele-999.
           exit.

      *    *===========================================================*
      *    * Lettura archivio [ivc]                                    *
      *    *-----------------------------------------------------------*
       let-arc-ivc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofivc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivc                 .
      *              *-------------------------------------------------*
      *              * Lettura [ivc]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ANNMESTOP "         to   f-key                  .
           move      w-let-arc-ivc-saa    to   rf-ivc-ann-ivc         .
           move      w-let-arc-ivc-mes    to   rf-ivc-mes-ivc         .
           move      w-let-arc-ivc-top    to   rf-ivc-cod-top         .
           move      "pgm/cge/fls/ioc/obj/iofivc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivc                 .
      *              *-------------------------------------------------*
      *              * Memorizzazione dati letti                       *
      *              *-------------------------------------------------*
           move      rf-ivc-val-imp       to   w-let-arc-ivc-val      .
       let-arc-ivc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ivc-999.
       let-arc-ivc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

