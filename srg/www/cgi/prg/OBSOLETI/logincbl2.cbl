       Identification Division.
       Program-Id.                                 logincbl2          .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    logincbl2           *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/12/03    *
      *                       Ultima revisione:    NdK del 11/02/14    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Prova di login a Tangram via Browser        *
      *                                                                *
      *                    Autenticazione Utente                       *
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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *    * Area di comunicazione per moduli                "maucmf"  *
      *    *                                                 "mppssf"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [sss]                                                 *
      *        *-------------------------------------------------------*
           copy      "www/cgi/fls/rec/rfsss"                          .

      *    *===========================================================*
      *    * Work per records di [auc] 'ute'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucute0.cpw"                   .

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
      *        * Flag di visualizzazione testata                       *
      *        *-------------------------------------------------------*
           05  w-exe-flg-tst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-ute              pic  x(08)                  .
           05  w-exe-pwd-ute              pic  x(08)                  .
           05  w-exe-win-nam              pic  x(32)                  .
           05  w-exe-flg-sts              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-001              pic  x(16)                  .
           05  w-exe-prm-002              pic  x(16)                  .
           05  w-exe-prm-003              pic  x(36)                  .
           05  w-exe-prm-004              pic  x(05)                  .

      ******************************************************************
       Procedure Division                using w-exe                  .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
______*    perform   ext-prm-000          thru ext-prm-999            .
      *              *-------------------------------------------------*
      *              * Modulo da eseguire                              *
      *              *-------------------------------------------------*
           perform   exe-mod-000          thru exe-mod-999            .
           if        w-exe-flg-sts        =    "S"
                     go to main-999.
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
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-prm-001          .
           move      spaces               to   w-exe-prm-002          .
           move      spaces               to   w-exe-prm-003          .
           move      spaces               to   w-exe-prm-004          .
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
      *              * Lettura della variabile di environment          *
      *              *                                                 *
      *              * PROBLEMA : 'o-shs' e' di 220 ma la variabile    *
      *              *            'POST' è attualmente di 99 caratteri *
      *              * ___                                             *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione 4 coppie parametro / valore          *
      *              *-------------------------------------------------*
           move      o-pst                to   w-all-str-alf          .
           move      "&"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-exe-prm-001          .
           move      w-all-str-cat (2)    to   w-exe-prm-002          .
           move      w-all-str-cat (3)    to   w-exe-prm-003          .
           move      w-all-str-cat (4)    to   w-exe-prm-004          .
      *              *-------------------------------------------------*
      *              * Estrazione codice utente            parametro 1 *
      *              *-------------------------------------------------*
           move      w-exe-prm-001        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-cod-ute          .
      *              *-------------------------------------------------*
      *              * Estrazione password                 parametro 2 *
      *              *-------------------------------------------------*
           move      w-exe-prm-002        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-pwd-ute          .
      *              *-------------------------------------------------*
      *              * Estrazione ID finestra              parametro 3 *
      *              *-------------------------------------------------*
           move      w-exe-prm-003        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-win-nam          .
      *              *-------------------------------------------------*
      *              * Estrazione status                   parametro 4 *
      *              *-------------------------------------------------*
           move      w-exe-prm-004        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-flg-sts          .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Modulo da eseguire                                        *
      *    *-----------------------------------------------------------*
       exe-mod-000.
      *              *-------------------------------------------------*
      *              * Test su flag                                    *
      *              *-------------------------------------------------*
           if        w-exe-flg-sts        not  = "S" or
                     w-exe-cod-ute        =    spaces
                     go to exe-mod-999.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           call      "www/cgi/prg/obj/logincbl1"
                                         using w-exe                  .
           cancel    "www/cgi/prg/obj/logincbl1"                      .
       exe-mod-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Open  per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *              *-------------------------------------------------*
      *              * [sss]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Close per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "maucmf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
           if        j-rsc                not  = spaces
                     go to cls-fls-800.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "maucmf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/maucmf"                         .
       cls-fls-800.
      *              *-------------------------------------------------*
      *              * [sss]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-tst          .
           move      spaces               to   w-exe-flg-sts          .
      *              *-------------------------------------------------*
      *              * Test se sessione gia' attiva                    *
      *              *-------------------------------------------------*
           if        w-exe-win-nam        =    spaces
                     go to exe-cph-020.
      *                  *---------------------------------------------*
      *                  * Lettura record [sss]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "SSSIDX    "         to   f-key                  .
           move      w-exe-win-nam        to   rf-sss-idx-md5         .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
       exe-cph-020.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-exe-cod-ute        =    spaces
                     move  "U"            to   w-exe-flg-sts
                     go to exe-cph-500
           else if   w-exe-pwd-ute        =    spaces
                     move  "P"            to   w-exe-flg-sts
                     go to exe-cph-500
           else      go to exe-cph-030.
       exe-cph-030.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "UTE"                to   j-tre                  .
           move      w-exe-cod-ute        to   j-kre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       exe-cph-050.
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        j-rsc                =    e-not-fnd
                     go to exe-cph-100
           else if   j-rsc                =    spaces
                     go to exe-cph-200
           else      go to exe-cph-300.
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "N"                  to   w-exe-flg-sts          .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-cph-500.
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-exe-flg-sts          .
      *                  *---------------------------------------------*
      *                  * Record in area di lavoro                    *
      *                  *---------------------------------------------*
           move      j-dat                to   w-ute                  .
      *                  *---------------------------------------------*
      *                  * Test su password                            *
      *                  *---------------------------------------------*
           if        w-exe-pwd-ute        not  = spaces        and
                     w-exe-pwd-ute        not  = w-ute-pwd-ute
                     move  "D"            to   w-exe-flg-sts
                     go to exe-cph-500.
           if        w-exe-pwd-ute        = spaces             and
                     w-exe-pwd-ute        not  = w-ute-pwd-ute
                     move  "D"            to   w-exe-flg-sts
                     go to exe-cph-500.
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-cph-500.
       exe-cph-300.
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "E"                  to   w-exe-flg-sts          .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-cph-500.
       exe-cph-500.
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
       emi-tst-000.
      *              *-------------------------------------------------*
      *              * Flag di testata documento                       *
      *              *-------------------------------------------------*
           if        w-exe-flg-tst        not  = spaces
                     go to emi-tst-999.
      *
           move      "#"                  to   w-exe-flg-tst          .
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
______*    display   "Content-type: text/html"
______*                                   with no advancing           .
           display   ""                                               .
           display   "<html>"                                         .
           display   "<head>"                                         .
           display   "<title>TANGRAM 5.0 - prove</title>"             .
           
       
           display   "<script language='JavaScript'>"                 .
           display   "let finestra = window.name;"                    .
           display   "console.log ('Finestra = ' + finestra);"        .
           display   "</script>"                                      .
       
           
           display   "</head>"                                        .
      *              *-------------------------------------------------*
      *              * Css                                             *
      *              *-------------------------------------------------*
           display   "<LINK REL=stylesheet"                           .
           display   " HREF='http://10.1.10.2/prove/tangram/css/login2.c
      -              "ss'"                                            .
           display   " TYPE=text/css'>"                               .
           display   "<STYLE TYPE=text/css>"                          .
           display   "@import url(http://10.1.10.2/prove/tangram/css/log
      -              "in2.css);"                                      .
           display   "</STYLE>"                                       .
      *              *-------------------------------------------------*
      *              * Colori sfondo                                   *
      *              *-------------------------------------------------*
           display   "<!-- CORPO  -->"                                .
           display   "<BODY"                                          .
           display   " BGCOLOR = #FFFFFF"                             .
           display   " TEXT    = #220000"                             .
           display   " VLINK   = #FFFFFF"                             .
           display   " OnLoad = 'document.login.usr.focus(); document.lo
      -              "gin.sss.value = window.name;'>"                 .
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<!-- MODULO -->"                                .
           display   "<FORM"                                          .
           display   " NAME   = 'login'"                              .
           display   " ACTION = ''"                                   .
           display   " METHOD = 'POST'"                               .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    " onSubmit = 'var elem = document.getElementById(""
      -              "flg""); elem.value = """
                                delimited by size
                     w-exe-flg-sts
                                delimited by spaces
______*              """; return OnSubmitForm()'/>"
______*              """; window.location.reload();'/>"
______*              """;alert(elem.value);'/>"
                     """;'/>"
                                delimited by size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .





      *              _______ PROVE ________
           go to     emi-tst-999.

           display   "<script type='text/javascript'>".
           display   "function OnSubmitForm()".
           display   "{if(document.login.sts.value == 'S')".
           display   "{document.login.action ='/cgi-bin/css_tangram1';} 
      -              "".
           display   "return true;}".
           display   "</script>".



       emi-tst-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *-----------------------------------------------------------*
       emi-cor-000.
      *              *-------------------------------------------------*
      *              * Emissione testata                               *
      *              *-------------------------------------------------*
           perform   emi-tst-000          thru emi-tst-999            .
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Tabella login                                   *
      *              *-------------------------------------------------*
           display   "<!-- DIV 'login' -->"                           .
           display   "<DIV ID=login>"                                 .
           display   "<!-- TABELLA -->"                               .
           display   "<TABLE"                                         .
           display   " BORDER       = '0'"                            .
           display   " CELLSPACING  = '0'"                            .
           display   " CELLPADDING  = '0'"                            .
           display   " ALIGN        = 'CENTER'"                       .
           display   " WIDTH        = '33%'"                          .
           display   ">"                                              .
           display   "<!-- TABELLA CONTENITORE : RIGA -->"            .
           display   "<TR>"                                           .
      *
           display   "<!-------------------------------->"            .
           display   "<!-- SUB-TABELLA                -->"            .
           display   "<!-------------------------------->"            .
           display   "<TABLE"                                         .
           display   " BORDER       = '0'"                            .
           display   " CELLSPACING  = '0'"                            .
           display   " CELLPADDING  = '10'"                           .
           display   " ALIGN        = 'CENTER'"                       .
           display   " WIDTH        = '33%'"                          .
           display   ">"                                              .
           display   "<TD WIDTH='100%'>"                              .
           display   "<table width=100% cellpadding=2 cellspacing=1 bord
      -              "er=0 class=mainline>"                           .
           display   "<tr>"                                           .
      *
           display   "<th colspan=2>TANGRAM - LOGIN</th>"             .
           display   "</tr>"                                          .
      *
           display   "<tr>"                                           .
           display   "<td class=row1 align=right>"                    .
           display   "<span class=gen>Codice </span>"                 .
           display   "</td>"                                          .
      *
           display   "<td class=row2>"                                .
      *
           display   "<!-------------------------------->"            .
           display   "<!--CAMPO DI INPUT 'usr'        -->"            .
           display   "<!-------------------------------->"            .
           display   "<input "                                        .
           display   " type='text' "                                  .
           display   " id='usr' "                                     .
           display   " name='usr' "                                   .
           display   " value='' "                                     .
           display   " size='12' "                                    .
           display   " maxlenght='8' "                                .
           display   "/>"                                             .
      *
           display   "</td>"                                          .
           display   "</tr>"                                          .
      *
           display   "<tr>"                                           .
           display   "<td class=row1 align=right>"                    .
           display   "<span class=gen>Password </span>"               .
           display   "</td>"                                          .
      *
           display   "<td class=row2>"                                .
           display   "<!-------------------------------->"            .
           display   "<!--CAMPO DI INPUT 'pwd'        -->"            .
           display   "<!-------------------------------->"            .
           display   "<input "                                        .
           display   " type='password' "                              .
           display   " id='pwd' "                                     .
           display   " name='pwd' "                                   .
           display   " value='' "                                     .
           display   " size='12' "                                    .
           display   " maxlenght='8' "                                .
           display   "/>"                                             .
      *
           display   "<!-------------------------------->"            .
           display   "<!--CAMPO DI INPUT 'sss'        -->"            .
           display   "<!-------------------------------->"            .
           display   "<INPUT TYPE='hidden' NAME='sss'>"               .
      *
           display   "<!-------------------------------->"            .
           display   "<!--CAMPO DI INPUT 'sts'        -->"            .
           display   "<!-------------------------------->"            .
           display   "<INPUT TYPE='hidden' NAME='sts' ID='flg'>"      .
      *
           display   "</td>"                                          .
      *
           display   "</tr>"                                          .
           display   "<tr>"                                           .
           display   "<td class=catBottom align=right colspan=2>"     .
           display   "<!-------------------------------->"            .
           display   "<!-- TASTO 'OK'                 -->"            .
           display   "<!-------------------------------->"            .
           display   "<input class=mainoption type=submit value=login>".
           display   "</td>"                                          .
           display   "</tr>"                                          .
      *
           display   "<!-- SUB-TABELLA - fine -->"                    .
           display   "</TABLE>"                                       .
           display   "</td>"                                          .
           display   "</tr>"                                          .
      *
           display   "<!-- TABELLA - fine -->"                        .
           display   "</TABLE>"                                       .
           display   "<!-- DIV 'login' - fine -->"                    .
           display   "</DIV>"                                         .
      *
           display   "<!-- DIV 'help' -->"                            .
           display   "<DIV ID=help>"                                  .
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
                     move  "INSERIRE IL CODICE UTENTE"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "N"
                     move  "CODICE UTENTE NON TROVATO"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "D"
                     move  "PASSWORD ERRATA"
                                          to   w-exe-str-dsp
           else if   w-exe-flg-sts        =    "S"
                     move  "Autenticato"
                                          to   w-exe-str-dsp
           else      move  "??????"       to   w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *
           go to     emi-cor-890.
           
           
           
           
           
           
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
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
           move      09                   to   w-all-str-num          .
      *
           move      "<P1>Codice :"       to   w-all-str-cat (1)      .
           move      w-exe-cod-ute        to   w-all-str-cat (2)      .
           move      "-"                  to   w-all-str-cat (3)      .
           move      w-exe-str-dsp        to   w-all-str-cat (4)      .
           move      "-"                  to   w-all-str-cat (5)      .
           move      w-exe-win-nam        to   w-all-str-cat (6)      .
           move      "-"                  to   w-all-str-cat (7)      .
______*    move      rf-sss-cod-ute       to   w-all-str-cat (8)      .
______*    move      v-edt                to   w-all-str-cat (8)      .
           move      w-exe-flg-sts        to   w-all-str-cat (8)      .
           move      "</P1>"              to   w-all-str-cat (9)      .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       emi-cor-890.
           display   "<!-- DIV 'help' - fine -->"                     .
           display   "</DIV>"                                         .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Fine corpo                                      *
      *              *-------------------------------------------------*
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione piede documento                                 *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           if        w-exe-win-nam        =    spaces
                     go to emi-pie-800.
           if        w-exe-cod-ute        =    spaces or
                     w-exe-cod-ute        =    "[?]"
                     go to emi-pie-800.
           if        w-exe-flg-sts        not  = "S"
                     go to emi-pie-800.
           perform   cmp-rec-sss-000      thru cmp-rec-sss-999        .


           go to     emi-pie-800.



      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
       emi-pie-800.
      *              *-------------------------------------------------*
      *              * Chiusura html                                   *
      *              *-------------------------------------------------*
           display   "</FORM>"                                        .

           
       
           display   "<script language='JavaScript'>"                 .
           
           display   "window.addEventListener('beforeunload', function (
      -              "e) {"                                           .
           display   "e.preventDefault();"                            .
           display   "e.returnValue = '';"                            .
           display   "});"                                            .
           
           display   "</script>"                                      .
       
           display   "</BODY>"                                        .
           display   "</HTML>"                                        .
       emi-pie-999.
           exit.

      *    *===========================================================*
      *    * Composizione record testata [sss]                         *
      *    *-----------------------------------------------------------*
       cmp-rec-sss-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "www/cgi/fls/ioc/obj/iofsss"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sss                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-exe-win-nam        to   rf-sss-idx-md5         .
           move      w-exe-cod-ute        to   rf-sss-cod-ute         .
           move      w-exe-dat-exe        to   rf-sss-dat-iss         .
       cmp-rec-sss-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
