       Identification Division.
       Program-Id.                                 elemag01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    elemag              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/01/24    *
      *                       Ultima revisione:    NdK del 16/02/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione movimenti di riclassificazione     *
      *                                                                *
      *                    ELETTRA                                     *
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
      *    * Area di comunicazione per modulo                 "mhtml0" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/h"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *-----------------------------------------------------------*
       01  w-cgi-str.
      *        *-------------------------------------------------------*
      *        * Variabile POST da trattare                            *
      *        *-------------------------------------------------------*
           05  w-cgi-str-var.
               10  filler    occurs 1800  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Delimitatore                                          *
      *        *-------------------------------------------------------*
           05  w-cgi-str-del              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore delle stringhe da estrarre                     *
      *        *-------------------------------------------------------*
           05  w-cgi-str-fld  occurs 20   pic  x(90)                  .
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-cgi-str-ctr              pic  9(02)                  .
           05  w-cgi-str-num              pic  9(02)                  .
           05  w-cgi-str-max              pic  9(02) value 20         .
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  w-cgi-tip-ope              pic  x(02)                  .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Livello operativo                                     *
      *        *-------------------------------------------------------*
           05  w-exe-liv-ope              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-ubi              pic  x(07)                  .






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
           move      zero                 to   w-exe-liv-ope          .
           move      spaces               to   w-exe-cod-ubi          .
______*    move      spaces               to   w-exe-cod-ubi          .




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
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice ubicazione in uppercase              *
      *                  *---------------------------------------------*
           move      w-exe-cod-ubi        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-cod-ubi          .
      *                  *---------------------------------------------*
      *                  * Conversione in numerico codice prodotto     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      07                   to   p-car                  .
______*    move      w-exe-num-pro        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
______*    move      p-num                to   w-exe-cnv-pro          .
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
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       exe-cph-010.
      *              *-------------------------------------------------*
      *              * Test preliminare su livello operativo           *
      *              *-------------------------------------------------*
           if        w-exe-liv-ope        =    "0"
                     go to exe-cph-100
           else if   w-exe-liv-ope        =    "1"
                     go to exe-cph-300
           else if   w-exe-liv-ope        =    "2"
                     go to exe-cph-500
           else if   w-exe-liv-ope        =    "3"
                     go to exe-cph-700
           else      go to exe-cph-100.
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Livello '0': accettazione responsabile          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-cph-lv0-000      thru exe-cph-lv0-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
       exe-cph-300.
      *              *-------------------------------------------------*
      *              * Livello '1': accettazione protocollo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-cph-lv1-000      thru exe-cph-lv1-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Livello '2': accettazione prodotti              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-cph-lv2-000      thru exe-cph-lv2-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
       exe-cph-700.
      *              *-------------------------------------------------*
      *              * Livello '3': accettazione quantita' per ubica-  *
      *              *              zione                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-cph-lv3-000      thru exe-cph-lv3-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per livello '0'                                *
      *    *-----------------------------------------------------------*
       exe-cph-lv0-000.
      *              *-------------------------------------------------*
      *              * Emissione 'header'                              *
      *              *-------------------------------------------------*
           perform   exe-cph-hea-000      thru exe-cph-hea-999        .
       exe-cph-lv0-200.
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='ods_001' id='ods_001' method='post' ac
      -              "tion='/cgi-bin/eleodsB1'>"                      .






       exe-cph-lv0-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-lv0-999.
       exe-cph-lv0-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per livello '1'                                *
      *    *-----------------------------------------------------------*
       exe-cph-lv1-000.
      *              *-------------------------------------------------*
      *              * Emissione 'header'                              *
      *              *-------------------------------------------------*
           perform   exe-cph-hea-000      thru exe-cph-hea-999        .
       exe-cph-lv1-200.
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='ods_001' id='ods_001' method='post' ac
      -              "tion='/cgi-bin/eleodsB1'>"                      .




       exe-cph-lv1-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-lv1-999.
       exe-cph-lv1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per livello '2'                                *
      *    *-----------------------------------------------------------*
       exe-cph-lv2-000.
      *              *-------------------------------------------------*
      *              * Emissione 'header'                              *
      *              *-------------------------------------------------*
           perform   exe-cph-hea-000      thru exe-cph-hea-999        .
       exe-cph-lv2-200.
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='ods_001' id='ods_001' method='post' ac
      -              "tion='/cgi-bin/eleodsB1'>"                      .




       exe-cph-lv2-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-lv2-999.
       exe-cph-lv2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per livello '3'                                *
      *    *-----------------------------------------------------------*
       exe-cph-lv3-000.
      *              *-------------------------------------------------*
      *              * Emissione 'header'                              *
      *              *-------------------------------------------------*
           perform   exe-cph-hea-000      thru exe-cph-hea-999        .
       exe-cph-lv3-200.
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='ods_001' id='ods_001' method='post' ac
      -              "tion='/cgi-bin/eleodsB1'>"                      .




       exe-cph-lv3-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-lv3-999.
       exe-cph-lv3-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'estrazione di max 20 coppie campi/valori da *
      *    * variabile POST letta                                      *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-cgi-str-var     = Variabile POST letta         *
      *    *                                                           *
      *    *          w-cgi-str-del     = Delimitatore (&)             *
      *    *                                                           *
      *    * Output : w-cgi-str-fld (i) = coppie estratte              *
      *    *                                                           *
      *    *          w-cgi-str-num     = Numero coppie estratte       *
      *    *-----------------------------------------------------------*
       cgi-str-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "&"                  to   w-cgi-str-del          .
           move      zero                 to   w-cgi-str-num          .
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-200.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-300.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
           go to     cgi-str-ext-200.
       cgi-str-ext-300.
      *              *-------------------------------------------------*
      *              * Test preliminare se il campo e' vuoto           *
      *              *-------------------------------------------------*
           if        w-cgi-str-var        =    spaces
                     go to cgi-str-ext-900.
       cgi-str-ext-400.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-cgi-str-var
                                delimited by   w-cgi-str-del
                                          into w-cgi-str-fld (01)
                                               w-cgi-str-fld (02)
                                               w-cgi-str-fld (03)
                                               w-cgi-str-fld (04)
                                               w-cgi-str-fld (05)
                                               w-cgi-str-fld (06)
                                               w-cgi-str-fld (07)
                                               w-cgi-str-fld (08)
                                               w-cgi-str-fld (09)
                                               w-cgi-str-fld (10)
                                               w-cgi-str-fld (11)
                                               w-cgi-str-fld (12)
                                               w-cgi-str-fld (13)
                                               w-cgi-str-fld (14)
                                               w-cgi-str-fld (15)
                                               w-cgi-str-fld (16)
                                               w-cgi-str-fld (17)
                                               w-cgi-str-fld (18)
                                               w-cgi-str-fld (19)
                                               w-cgi-str-fld (20)     .
       cgi-str-ext-500.
      *              *-------------------------------------------------*
      *              * Ciclo di verifica numero stringhe estratte      *
      *              *-------------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-600.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-900.
           if        w-cgi-str-fld
                    (w-cgi-str-ctr)       =    spaces
                     go to cgi-str-ext-900.
           add       1                    to   w-cgi-str-num          .
       cgi-str-ext-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-600.
       cgi-str-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-999.
       cgi-str-ext-999.
           exit.

      *    *===========================================================*
      *    * Operazioni su parametri in input                          *
      *    *-----------------------------------------------------------*
       ope-prm-inp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cgi-tip-ope        =    "NO"
                     go to ope-prm-inp-100
           else if   w-cgi-tip-ope        =    "EX"
                     go to ope-prm-inp-500.
       ope-prm-inp-100.
      *              *=================================================*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di normalizzazione                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-120.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-120.
       ope-prm-inp-500.
      *              *=================================================*
      *              * Estrazione coppie                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di estrazione                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-520.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
      *                  *---------------------------------------------*
      *                  * Estrazione                                  *
      *                  *---------------------------------------------*
           move      w-cgi-str-fld
                    (w-cgi-str-ctr)       to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           perform   ext-prm-ass-000      thru ext-prm-ass-999        .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-520.
       ope-prm-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ope-prm-inp-999.
       ope-prm-inp-999.
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
           if        w-all-str-cat (1)    =    "liv_ope"
                     move  w-all-str-cat (2)
                                          to   w-exe-liv-ope
           else if   w-all-str-cat (1)    =    "cod_ubi"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-ubi          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Intestazione                                              *
      *    *                                                           *
      *    * Subroutine per 'header'                                   *
      *    *-----------------------------------------------------------*
       exe-cph-hea-000.
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           move      "HH"                 to   h-ope                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Emissione icona                                 *
      *              *-------------------------------------------------*
           move      "HF"                 to   h-ope                  .
           move      "../icons/"          to   h-prm                  .
           move      "favicon.ico"        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Commento di apertura                            *
      *              *-------------------------------------------------*
           move      "EC"                 to   h-ope                  .
           move      "* wip - Tutti i diritti riservati *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Commento di aggiornamento                       *
      *              *-------------------------------------------------*
           move      "EC"                 to   h-ope                  .
           move      "* .....Aggiornamento : 05/06/2023 *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Titolo documento                                *
      *              *-------------------------------------------------*
           move      "HT"                 to   h-ope                  .
           move      "ORDINI DI SPEDIZIONE {ELETTRA}"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI CSS                                   *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../css_ele/"        to   h-prm                  .
           move      "jquery-ui.min.css"  to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Css per il documento                            *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../css_ele/"        to   h-prm                  .
           move      "ele.css"            to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery min                                      *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "jquery.min.js"      to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI min                                   *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "jquery-ui.min.js"   to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery redirect                                 *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "jquery.redirect.js" to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Javascript per il documento                     *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "eleodsB1.js"        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Head - fine                                     *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "head"               to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Corpo                                           *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "body"               to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-hea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-hea-999.
       exe-cph-hea-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
