       Identification Division.
       Program-Id.                                 elebfo1s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    elebfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/07/06    *
      *                       Ultima revisione:    NdK del 01/04/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Stampa documento di entrata                 *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      *                    ___ MEMO: TRASFORMARE IN elebfop0           *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *                              Note                              *
      *                                                                *
      * - La stampante impostata viene gestita da 'xpg440' con la fase *
      *   'elebfo'                                                     *
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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Work-area per il controllo del modulo di stampa       *
      *        *-------------------------------------------------------*
           05  w-cnt-mst.
      *            *---------------------------------------------------*
      *            * Selezione stampa effettuata                       *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-cnt-mst-sel          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di documento da stampare                     *
      *            *---------------------------------------------------*
               10  w-cnt-mst-flg          pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pbfo300s       *
      *    *                                                           *
      *    * La link-area comprende :                                  *
      *    *                                                           *
      *    *  - 'w-inp-mst' : Work-area per la ridefinizione della     *
      *    *                  variabile di i.p.c. in input  "inp-mst"  *
      *    *                                                           *
      *    *  - 'w-out-mst' : Work-area per la ridefinizione della     *
      *    *                  variabile di i.p.c. in output "out-mst"  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/prg/cpy/pbfo300s.pgl"                   .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cpw"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-rsm              pic  x(03)                  .
           05  w-exe-cnv-rsm              pic  9(03)                  .
           05  w-exe-saa-bfo              pic  x(03)                  .
           05  w-exe-num-bfo              pic  x(06)                  .
           05  w-exe-prt-bfo              pic  x(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zrm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zrm.
               10  w-let-arc-zrm-flg      pic  x(01)                  .
               10  w-let-arc-zrm-cod      pic  9(05)                  .
               10  w-let-arc-zrm-des      pic  x(40)                  .
               10  w-let-arc-zrm-stp      pic  x(08)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-bft.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-nds      pic  9(11)                  .
               10  w-slc-num-bft-nds-r redefines
                   w-slc-num-bft-nds.
                   15  w-slc-num-bft-nsa  pic  9(03)                  .
                   15  w-slc-num-bft-ndp  pic  9(02)                  .
                   15  w-slc-num-bft-npg  pic  9(06)                  .
               10  w-slc-num-prt-num      pic  9(09)                  .
               10  w-slc-num-prt-num-r    redefines
                   w-slc-num-prt-num.
                   15  w-slc-num-prt-saa  pic  9(03)                  .
                   15  w-slc-num-prt-prg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-prt      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division                                             .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           perform   ext-prm-000          thru ext-prm-999            .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   opn-fls-000          thru opn-fls-999            .
       main-100.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura e preparazione html            *
      *              *-------------------------------------------------*
           perform   exe-cph-000          thru exe-cph-999            .
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   cls-fls-000          thru cls-fls-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *-----------------------------------------------------------*
       ext-prm-000.
      *              *-------------------------------------------------*
      *              * Data di sistema                                 *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * CORRETTIVO PROVVISORIO                          *
      *              *-------------------------------------------------*
           if        w-exe-dat-exe        <    999999
                     add  1000000         to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-cod-rsm          .
           move      zero                 to   w-exe-cnv-rsm          .
           move      spaces               to   w-exe-saa-bfo          .
           move      spaces               to   w-exe-num-bfo          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      02                   to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           move      o-pst                to   w-cgi-str-var          .
           perform   cgi-str-ext-000      thru cgi-str-ext-999        .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           move      w-cgi-str-num        to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-prt      .
      *                  *---------------------------------------------*
      *                  * Codice numerico responsabile                *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      03                   to   p-car                  .
           move      w-exe-cod-rsm        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-cnv-rsm          .
       ext-prm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-999.
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione ST : Stampa                           *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Apertura modulo di stampa documento             *
      *              *-------------------------------------------------*
           perform   opn-mod-stp-000      thru opn-mod-stp-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione stampa documento                     *
      *              *-------------------------------------------------*
           perform   exe-mod-stp-000      thru exe-mod-stp-999        .
      *              *-------------------------------------------------*
      *              * Chiusura modulo di stampa documento             *
      *              *-------------------------------------------------*
           perform   cls-mod-stp-000      thru cls-mod-stp-999        .
       exe-cph-600.
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Apertura modulo di stampa                                 *
      *    *-----------------------------------------------------------*
       opn-mod-stp-000.
      *              *-------------------------------------------------*
      *              * Preparazione variabile "inp-mst"                *
      *              *                                                 *
      *              * ELETTRA                                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-inp-mst              .
           move      "OP"                 to   w-inp-mst-ope          .
           move      "F"                  to   w-inp-mst-sel          .
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "ele"                to   w-inp-mst-azi          .
           move      "cgi"                to   w-inp-mst-ter          .
           move      "master "            to   w-inp-mst-ute          .
           move      "pgm"                to   w-inp-mst-sap          .
           move      "bfo"                to   w-inp-mst-arg          .
           move      "mov"                to   w-inp-mst-set          .
      *              *-------------------------------------------------*
      *              * Eventuale lettura codice stampante associato al *
      *              * responsabile di magazzino                       *
      *              *-------------------------------------------------*



______*    move      w-exe-cnv-rsm        to   w-let-arc-zrm-cod      .
______*    perform   let-arc-zrm-000      thru let-arc-zrm-999        .



______* LET-ARC-ZRM
      
      
      
      *              *-------------------------------------------------*
      *              * Fase di stampa in funzione del responsabile:    *
      *              * questo per consentire di stampare in due stam-  *
      *              * panti differenti                                *
      *              *                                                 *
      *              * (Giorgio - 31/10/24 - Resp. 5 e 11)             *
      *              *                                                 *
      *              * ELETTRA                                         *
      *              *-------------------------------------------------*
           if        w-exe-cnv-rsm        =    005 or
                     w-exe-cnv-rsm        =    011
                     move  "elebf2"       to   w-inp-mst-fas
           else      move  "elebfo"       to   w-inp-mst-fas          .
      *
           move      "pbfo300s"           to   w-inp-mst-dep          .
      *              *-------------------------------------------------*
      *              * Scrittura variabile "inp-mst"                   *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "inp-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      w-inp-mst            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di stampa documento         *
      *              *-------------------------------------------------*
           move      "ele/bfo/prg/obj/pbfo300s"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
      *              *-------------------------------------------------*
      *              * Controllo esito operazione di Open              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura variabile di ritorno "out-mst"      *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "out-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to opn-mod-stp-500.
           move      s-alf                to   w-out-mst              .
           if        w-out-mst-opn        not  = spaces
                     go to opn-mod-stp-500.
      *                  *---------------------------------------------*
      *                  * Se variabile di ritorno "out-mst" esistente *
      *                  * ed indicante selezione effettuata           *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-mst-sel          .
           go to     opn-mod-stp-999.
       opn-mod-stp-500.
      *                  *---------------------------------------------*
      *                  * Se variabile di ritorno "out-mst" non esi-  *
      *                  * stente o se selezione non effettuata        *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-mst-sel          .
       opn-mod-stp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione modulo di stampa documento                     *
      *    *-----------------------------------------------------------*
       exe-mod-stp-000.
      *              *-------------------------------------------------*
      *              * Controllo preliminare che il documento sia      *
      *              * stampabile                                      *
      *              *                                                 *
      *              * Attualmente NON eseguito (Giorgio 22/05/23)     *
      *              *-------------------------------------------------*
       exe-mod-stp-100.
      *              *-------------------------------------------------*
      *              * Preparazione variabile "inp-mst"                *
      *              *-------------------------------------------------*
           move      spaces               to   w-inp-mst              .
           move      "ST"                 to   w-inp-mst-ope          .
           move      w-slc-num-bft-prt    to   w-inp-mst-prt          .
           move      "S"                  to   w-inp-mst-ejc          .
           move      1                    to   w-inp-mst-tor          .
      *              *-------------------------------------------------*
      *              * Scrittura variabile "inp-mst"                   *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "inp-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      w-inp-mst            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di stampa documento         *
      *              *                                                 *
      *              * ELETTRA                                         *
      *              *-------------------------------------------------*
           move      "ele/bfo/prg/obj/pbfo300s"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
       exe-mod-stp-200.
      *              *-------------------------------------------------*
      *              * Controllo esito operazione di Stampa            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura variabile di ritorno "out-mst"      *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "out-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to exe-mod-stp-400.
           move      s-alf                to   w-out-mst              .
           if        w-out-mst-stp        =    spaces
                     go to exe-mod-stp-400.
           if        w-out-mst-stp        =    "N"
                     go to exe-mod-stp-600
           else if   w-out-mst-stp        =    "E"
                     go to exe-mod-stp-800
           else      go to exe-mod-stp-400.
       exe-mod-stp-400.
      *              *-------------------------------------------------*
      *              * Se stampa a buon fine                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento flag                          *
      *                  * ___                                         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      w-slc-num-bft-prt    to   w-slc-num-bft-nds      .
           move      w-slc-num-bft-nsa    to   w-slc-num-prt-saa      .
           move      w-slc-num-bft-npg    to   w-slc-num-prt-prg      .
      *
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-prt-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<h1> ESEGUITA STAMPA DOCUMENTO NR."
                                          to   w-all-str-cat (1)      .
           move      "</h1>"              to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-prm-msg          .
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='ver'>"                     .
           display   w-exe-prm-msg                                    .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     exe-mod-stp-999.
       exe-mod-stp-600.
      *              *-------------------------------------------------*
      *              * Se documento da stampare non trovato            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           display   "<h1 style='color:white;font-size:200%;'>"       .
           display   "Documento da stampare non trovato !"            .
           display   "</h1>"                                          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     exe-mod-stp-999.
       exe-mod-stp-800.
      *              *-------------------------------------------------*
      *              * Se errore grave in stampa documento             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           display   "<h1 style='color:white;font-size:200%;'>"       .
           display   "Errore grave in stampa documento !"             .
           display   "</h1>"                                          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     exe-mod-stp-999.
       exe-mod-stp-999.
           exit.

      *    *===========================================================*
      *    * Chiusura modulo di stampa                                 *
      *    *-----------------------------------------------------------*
       cls-mod-stp-000.
      *              *-------------------------------------------------*
      *              * Preparazione variabile "inp-mst"                *
      *              *-------------------------------------------------*
           move      spaces               to   w-inp-mst              .
           move      "CL"                 to   w-inp-mst-ope          .
      *              *-------------------------------------------------*
      *              * Scrittura variabile "inp-mst"                   *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "inp-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      w-inp-mst            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di stampa documento         *
      *              *-------------------------------------------------*
           move      "ele/bfo/prg/obj/pbfo300s"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
      *              *-------------------------------------------------*
      *              * Cancellazione del modulo di stampa documento    *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       cls-mod-stp-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome campo           *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "rsp_doc"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-rsm
           else if   w-all-str-cat (1)    =    "prt_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-bfo          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zrm]                         *
      *    *-----------------------------------------------------------*
       let-arc-zrm-000.
      *              *-------------------------------------------------*
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zrm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zrm-cod    =    zero
                     go to let-arc-zrm-500.
       let-arc-zrm-100.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODRSP"             to   f-key                  .
           move      w-let-arc-zrm-cod    to   rf-zrm-cod-rsp         .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zrm-400.
       let-arc-zrm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zrm-des-rsp       to   w-let-arc-zrm-des      .
______*    move      rf-zrm-cod-stp       to   w-let-arc-zrm-stp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zrm-900.
       let-arc-zrm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zrm-flg      .
           move      all   "."            to   w-let-arc-zrm-des      .
           move      spaces               to   w-let-arc-zrm-stp      .
           go to     let-arc-zrm-600.
       let-arc-zrm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zrm-des      .
           move      spaces               to   w-let-arc-zrm-stp      .
       let-arc-zrm-600.
       let-arc-zrm-900.
      *              *-------------------------------------------------*
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
       let-arc-zrm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines di trattamento variabile POST                 *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
