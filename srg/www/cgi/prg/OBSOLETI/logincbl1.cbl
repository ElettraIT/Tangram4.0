       Identification Division.
       Program-Id.                                 logincbl1          .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    logincbl1           *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/12/03    *
      *                       Ultima revisione:    NdK del 11/02/14    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Prova di login a Tangram via Browser        *
      *                                                                *
      *                    Autenticazione Azienda                      *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*

      *    *===========================================================*
      *    * Work-area di emissione HTML                               *
      *    *-----------------------------------------------------------*
           copy      "www/cgi/prg/cpy/logincblx.cpy"                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area comune ai moduli 'login'                             *
      *    *-----------------------------------------------------------*
           copy      "www/cgi/prg/cpy/log_exew.cpy"                   .

      ******************************************************************
       Procedure Division                using w-exe                  .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
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
           move      spaces               to   w-exe-flg-sts          .
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Emissione testata                               *
      *              *-------------------------------------------------*
           perform   emi-tes-000          thru emi-tes-999            .
      *              *-------------------------------------------------*
      *              * Emissione corpo documento                       *
      *              *-------------------------------------------------*
           perform   emi-cor-000          thru emi-cor-999            .
      *              *-------------------------------------------------*
      *              * Emissione piede documento                       *
      *              *-------------------------------------------------*
           perform   emi-pie-000          thru emi-pie-999            .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-tes-000.
       emi-tes-100.
      *              *-------------------------------------------------*
      *              * FORZATURA flag                                  *
      *              *-------------------------------------------------*
           move      "A"                  to   w-exe-flg-sts          .
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           move      "docum  "            to   w-htm-tip-ele          .
           move      "APERTURA"           to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Apertura documento                              *
      *              *-------------------------------------------------*
           move      "html  A"            to   w-htm-tip-ele          .
           move      "INIZIO"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Apertura testata                                *
      *              *-------------------------------------------------*
           move      "head  A"            to   w-htm-tip-ele          .
           move      "TESTATA"            to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Titolo                                          *
      *              *-------------------------------------------------*
           move      "title  "            to   w-htm-tip-ele          .
           move      "TITOLO"             to   w-htm-nom-ele          .
           move      "TANGRAM 5.0"        to   w-htm-opz-val (01)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Css                                             *
      *              *-------------------------------------------------*
           move      "link  3"            to   w-htm-tip-ele          .
           move      "DEFINIZIONE 'CSS'"  to   w-htm-nom-ele          .
           move      "rel"                to   w-htm-opz-nom (01)     .
           move      "stylesheet"         to   w-htm-opz-val (01)     .
           move      "href"               to   w-htm-opz-nom (02)     .
           move      "http://10.1.10.2/prove/tangram/css/login2.css"
                                          to   w-htm-opz-val (02)     .
           move      "type"               to   w-htm-opz-nom (03)     .
           move      "text/css"           to   w-htm-opz-val (03)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura testata                                *
      *              *-------------------------------------------------*
           move      "head  C"            to   w-htm-tip-ele          .
           move      "TESTATA"            to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Corpo                                           *
      *              *-------------------------------------------------*
           move      "body  4"            to   w-htm-tip-ele          .
           move      "CORPO"              to   w-htm-nom-ele          .
           move      "bgcolor"            to   w-htm-opz-nom (01)     .
           move      "#FFFFFF"            to   w-htm-opz-val (01)     .
           move      "text"               to   w-htm-opz-nom (02)     .
           move      "#220000"            to   w-htm-opz-val (02)     .
           move      "vlink"              to   w-htm-opz-nom (03)     .
           move      "#FFFFFF"            to   w-htm-opz-val (03)     .
           move      "OnLoad"             to   w-htm-opz-nom (04)     .
           move      "document.login.azi.focus();"
                                          to   w-htm-opz-val (04)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           move      "form  4"            to   w-htm-tip-ele          .
           move      "MODULO 'login'"     to   w-htm-nom-ele          .
           move      "name"               to   w-htm-opz-nom (01)     .
           move      "login"              to   w-htm-opz-val (01)     .
           move      "action"             to   w-htm-opz-nom (02)     .
           move      spaces               to   w-htm-opz-val (02)     .
           move      "method"             to   w-htm-opz-nom (03)     .
           move      "post"               to   w-htm-opz-val (03)     .
           move      "onSubmit"           to   w-htm-opz-nom (04)     .
      *
           move      spaces               to   w-htm-opz-val (04)     .
           string    "var elem = document.getElementById(""flg""); elem.
      -              "value = """
                                delimited by size
                     w-exe-flg-sts
                                delimited by spaces
                     """;"
                                delimited by size
                                          into w-htm-opz-val (04)     .
      *
           perform   emi-htm-000          thru emi-htm-999            .
       emi-tes-900.
      *              *-------------------------------------------------*
      *              * Fine testata                                    *
      *              *-------------------------------------------------*
           go to     emi-tes-999.
       emi-tes-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *-----------------------------------------------------------*
       emi-cor-000.
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Div login                                       *
      *              *-------------------------------------------------*
           move      "div   1"            to   w-htm-tip-ele          .
           move      "RIQUADRO 1"         to   w-htm-nom-ele          .
           move      "id"                 to   w-htm-opz-nom (01)     .
           move      "login"              to   w-htm-opz-val (01)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Tabella login                                   *
      *              *-------------------------------------------------*
           move      "table 5"            to   w-htm-tip-ele          .
           move      "TABELLA 1"          to   w-htm-nom-ele          .
           move      "border"             to   w-htm-opz-nom (01)     .
           move      "0"                  to   w-htm-opz-val (01)     .
           move      "cellspacing"        to   w-htm-opz-nom (02)     .
           move      "0"                  to   w-htm-opz-val (02)     .
           move      "cellpadding"        to   w-htm-opz-nom (03)     .
           move      "0"                  to   w-htm-opz-val (03)     .
           move      "align"              to   w-htm-opz-nom (04)     .
           move      "center"             to   w-htm-opz-val (04)     .
           move      "width"              to   w-htm-opz-nom (05)     .
           move      "33%"                to   w-htm-opz-val (05)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    A"            to   w-htm-tip-ele          .
           move      "RIGA 1"            to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Sub tabella login                               *
      *              *-------------------------------------------------*
           move      "table 5"            to   w-htm-tip-ele          .
           move      "TABELLA 2"          to   w-htm-nom-ele          .
           move      "border"             to   w-htm-opz-nom (01)     .
           move      "0"                  to   w-htm-opz-val (01)     .
           move      "cellspacing"        to   w-htm-opz-nom (02)     .
           move      "0"                  to   w-htm-opz-val (02)     .
           move      "cellpadding"        to   w-htm-opz-nom (03)     .
           move      "0"                  to   w-htm-opz-val (03)     .
           move      "align"              to   w-htm-opz-nom (04)     .
           move      "center"             to   w-htm-opz-val (04)     .
           move      "width"              to   w-htm-opz-nom (05)     .
           move      "33%"                to   w-htm-opz-val (05)     .
           perform   emi-htm-000          thru emi-htm-999            .



           display   "<TD WIDTH='100%'>"                              .
           display   "<table width=100% cellpadding=2 cellspacing=1 bord
      -              "er=0 class=mainline>"                           .
           display   "<tr>"                                           .
      *
           display   "<th colspan=2>AZIENDA</th>"                     .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    C"            to   w-htm-tip-ele          .
           move      "RIGA 1"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    A"            to   w-htm-tip-ele          .
           move      "RIGA 2"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .


           display   "<td class=row1 align=right>"                    .
           display   "<span class=gen>Codice AZIENDA</span>"          .
           display   "</td>"                                          .
           display   "<td class=row2>"                                .


      *              *-------------------------------------------------*
      *              * Codice Azienda                                  *
      *              *-------------------------------------------------*
           move      "input 5"            to   w-htm-tip-ele          .
           move      "ACCETTAZIONE 'azi'" to   w-htm-nom-ele          .
           move      "type"               to   w-htm-opz-nom (01)     .
           move      "text"               to   w-htm-opz-val (01)     .
           move      "name"               to   w-htm-opz-nom (02)     .
           move      "azi"                to   w-htm-opz-val (02)     .
           move      "value"              to   w-htm-opz-nom (03)     .
           move      spaces               to   w-htm-opz-val (03)     .
           move      "size"               to   w-htm-opz-nom (04)     .
           move      "10"                 to   w-htm-opz-val (04)     .
           move      "maxlength"          to   w-htm-opz-nom (05)     .
           move      "8"                  to   w-htm-opz-val (05)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura elemento riga                          *
      *              *-------------------------------------------------*
           move      "td    C"            to   w-htm-tip-ele          .
           move      "COLONNA 1"          to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    C"            to   w-htm-tip-ele          .
           move      "RIGA 1"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .




      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    A"            to   w-htm-tip-ele          .
           move      "RIGA 2"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Prompt Password Azienda                         *
      *              *-------------------------------------------------*
           display   "<td class=row1 align=right>"                    .
           display   "<span class=gen>Password AZIENDA</span>"        .
           display   "</td>"                                          .
           display   "<td class=row2>"                                .
           display   "<!-------------------------------->"            .
           display   "<!--CAMPO DI INPUT 'pwd'        -->"            .
           display   "<!-------------------------------->"            .
           display   "<INPUT"                                         .
           display   " TYPE      = 'password'"                        .
           display   " NAME      = 'pwd'"                             .
           display   " VALUE     = ''"                                .
           display   " SIZE      = '10'"                              .
           display   " MAXLENGTH = '8'"                               .
           display   ">"                                              .

      *              *-------------------------------------------------*
      *              * Chiusura colonna riga                           *
      *              *-------------------------------------------------*
           move      "td    C"            to   w-htm-tip-ele          .
           move      "COLONNA 2"          to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    C"            to   w-htm-tip-ele          .
           move      "RIGA 2"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .



      *              *-------------------------------------------------*
      *              * Campo nascosto 'sss'                            *
      *              *-------------------------------------------------*
           move      "input 3"            to   w-htm-tip-ele          .
           move      "CAMPO INVISIBILE 'sss'"
                                          to   w-htm-nom-ele          .
           move      "type"               to   w-htm-opz-nom (01)     .
           move      "hidden"             to   w-htm-opz-val (01)     .
           move      "name"               to   w-htm-opz-nom (02)     .
           move      "sss"                to   w-htm-opz-val (02)     .
           move      "value"              to   w-htm-opz-nom (03)     .
           move      w-exe-win-nam        to   w-htm-opz-val (03)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Campo nascosto 'sts'                            *
      *              *-------------------------------------------------*
           move      "input 3"            to   w-htm-tip-ele          .
           move      "CAMPO INVISIBILE 'sts'"
                                          to   w-htm-nom-ele          .
           move      "type"               to   w-htm-opz-nom (01)     .
           move      "hidden"             to   w-htm-opz-val (01)     .
           move      "name"               to   w-htm-opz-nom (02)     .
           move      "sts"                to   w-htm-opz-val (02)     .
           move      "id"                 to   w-htm-opz-nom (03)     .
           move      "flg"                to   w-htm-opz-val (03)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    A"            to   w-htm-tip-ele          .
           move      "RIGA 3"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Apertura colonna riga                           *
      *              *-------------------------------------------------*
           move      "td    3"            to   w-htm-tip-ele          .
           move      "COLONNA 1/3"        to   w-htm-nom-ele          .
           move      "class"              to   w-htm-opz-nom (01)     .
           move      "catBottom"          to   w-htm-opz-val (01)     .
           move      "align"              to   w-htm-opz-nom (02)     .
           move      "right"              to   w-htm-opz-val (02)     .
           move      "colspan"            to   w-htm-opz-nom (03)     .
           move      "2"                  to   w-htm-opz-val (03)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Bottone 'OK'                                    *
      *              *-------------------------------------------------*
           move      "input 3"            to   w-htm-tip-ele          .
           move      "BOTTONE 'OK'"       to   w-htm-nom-ele          .
           move      "class"              to   w-htm-opz-nom (01)     .
           move      "mainoption"         to   w-htm-opz-val (01)     .
           move      "type"               to   w-htm-opz-nom (02)     .
           move      "submit"             to   w-htm-opz-val (02)     .
           move      "value"              to   w-htm-opz-nom (03)     .
           move      "OK"                 to   w-htm-opz-val (03)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura elemento riga                          *
      *              *-------------------------------------------------*
           move      "td    C"            to   w-htm-tip-ele          .
           move      "COLONNA 1/3"        to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    C"            to   w-htm-tip-ele          .
           move      "RIGA 1"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Sub Tabella - FINE                              *
      *              *-------------------------------------------------*
           move      "table C"            to   w-htm-tip-ele          .
           move      "TABELLA 2"          to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura elemento riga                          *
      *              *-------------------------------------------------*
           move      "td    C"            to   w-htm-tip-ele          .
           move      "TABELLA 1"          to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "tr    C"            to   w-htm-tip-ele          .
           move      "RIGA 1"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Tabella - FINE                                  *
      *              *-------------------------------------------------*
           move      "table C"            to   w-htm-tip-ele          .
           move      "TABELLA 1"          to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Div login - FINE                                *
      *              *-------------------------------------------------*
           move      "div   C"            to   w-htm-tip-ele          .
           move      "RIQUADRO 1"         to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Div help                                        *
      *              *-------------------------------------------------*
           move      "div   1"            to   w-htm-tip-ele          .
           move      "RIQUADRO 'help'"    to   w-htm-nom-ele          .
           move      "id"                 to   w-htm-opz-nom (01)     .
           move      "help"               to   w-htm-opz-val (01)     .
           perform   emi-htm-000          thru emi-htm-999            .
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
           if        w-exe-flg-sts        =    "E"
                     move  "GRAVE ERRORE IN LETTURA DATI"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "P"
                     move  "MANCA LA PASSWORD"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "U"
                     move  "MANCA IL CODICE UTENTE"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "N"
                     move  "CODICE UTENTE NON TROVATO"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "D"
                     move  "PASSWORD INCORRETTA"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "S"
                     move  "OK"
                                          to   w-exe-str-dsp
           else      move  "??????"       to   w-exe-str-dsp          .
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-exe-dat-exe        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Assemblaggio con codice utente                  *
      *              *-------------------------------------------------*
           if        w-exe-cod-ute        =    spaces
                     move  "[?]"          to   w-exe-cod-ute          .
      *
           move      220                  to   w-all-str-lun          .
           move      10                   to   w-all-str-num          .
      *
           move      "<P1>Codice :"       to   w-all-str-cat (1)      .
           move      w-exe-cod-ute        to   w-all-str-cat (2)      .
           move      "-"                  to   w-all-str-cat (3)      .
           move      w-exe-str-dsp        to   w-all-str-cat (4)      .
           move      "-"                  to   w-all-str-cat (5)      .
           move      w-exe-win-nam        to   w-all-str-cat (6)      .
           move      "-"                  to   w-all-str-cat (7)      .
           move      v-edt                to   w-all-str-cat (8)      .
           move      "</P1>"              to   w-all-str-cat (9)      .
           move      "- cbl1"             to   w-all-str-cat (10)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Div help - FINE                                 *
      *              *-------------------------------------------------*
           move      "div   C"            to   w-htm-tip-ele          .
           move      "RIQUADRO 'help'"    to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Fine corpo                                      *
      *              *-------------------------------------------------*
           go to     emi-cor-999.
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione piede documento                                 *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Chiusura html                                   *
      *              *-------------------------------------------------*
           move      "form  C"            to   w-htm-tip-ele          .
           move      "MODULO 'login'"     to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *
           move      "body  C"            to   w-htm-tip-ele          .
           move      "CORPO"              to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
      *
           move      "html  C"            to   w-htm-tip-ele          .
           move      "INIZIO"             to   w-htm-nom-ele          .
           perform   emi-htm-000          thru emi-htm-999            .
       emi-pie-900.
      *              *-------------------------------------------------*
      *              * Fine piede                                      *
      *              *-------------------------------------------------*
           go to     emi-pie-999.
       emi-pie-999.
           exit.

      *    *===========================================================*
      *    * Subroutines di emissione HTML                             *
      *    *-----------------------------------------------------------*
           copy      "www/cgi/prg/cpy/logincbls.cpy"                  .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
