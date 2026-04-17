       Identification Division.
       Program-Id.                                 msqzou             .
      *================================================================*
      *                                                                *
      * Gestione file line sequential in output per sequenzializzazio- *
      * ni su files '.sqz'                                             *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Open output line sequential file                   *
      *                                                                *
      *             Input  : s-ope = "OP"                              *
      *                                                                *
      *                      s-nam = file name                         *
      *                                                                *
      *                      s-pat = file pathname                     *
      *                                                                *
      *             Output : s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Filter      Comunicazione tipo di output filter                *
      * Type                                                           *
      *             Input  : s-ope = "FT"                              *
      *                                                                *
      *                      s-num = tipo filtro                       *
      *                              - 0 : nessun filtro               *
      *                              - 1 : filtro standard             *
      *                              - 2 : filtro programma            *
      *                                                                *
      *                      s-pat = filtro                            *
      *                              - se 0 : Spaces                   *
      *                              - se 1 : Nome filtro standard     *
      *                              - se 2 : Pathname oggetto         *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put 0       Inizio put next record to line sequential file     *
      *                                                                *
      *             Input  : s-ope = "P0"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put 5       Blocco di 80 caratteri per put next                *
      *                                                                *
      *             Input  : s-ope = "P5"                              *
      *                                                                *
      *                      s-alf = blocco di 80 caratteri            *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put 9       Fine put next record to line sequential file       *
      *                                                                *
      *             Input  : s-ope = "P9"                              *
      *                                                                *
      *             Output : s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Close line sequential file                         *
      *                                                                *
      *             Input  : s-ope = "CL"                              *
      *                                                                *
      *             Output : s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
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
      *    * Work area per il controllo                                *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flag determinazione tipo runtime utilizzato           *
      *        * - Spaces : non determinato                            *
      *        * - #      : determinato                                *
      *        *-------------------------------------------------------*
           05  w-cnt-det-run              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Tipo di runtime utilizzato                            *
      *        * - 00 : Acucobol 85                                    *
      *        * - 01 : Austec RM-Master Cobol 74                      *
      *        * - 02 : Ryan McFarland RM-Cobol 85                     *
      *        *-------------------------------------------------------*
           05  w-cnt-cod-run              pic  9(02) value 00         .
      *        *-------------------------------------------------------*
      *        * Tipo di Open attualmente in corso                     *
      *        * - Spaces : nessuna                                    *
      *        * - O      : open output                                *
      *        *-------------------------------------------------------*
           05  w-cnt-tip-opn              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * File name originale da trattare                       *
      *        *-------------------------------------------------------*
           05  w-cnt-fil-nam              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname originale da trattare                   *
      *        *-------------------------------------------------------*
           05  w-cnt-fil-pat              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per pathname directory di base 'asc'           *
      *        *-------------------------------------------------------*
           05  w-cnt-fil-asc              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per sigla azienda in uso                       *
      *        *-------------------------------------------------------*
           05  w-cnt-fil-azi              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname per il file sequenziale, con formato:   *
      *        *                                                       *
      *        *   xxxxyyyy.sqz                                        *
      *        *                                                       *
      *        * - xxxx : Sigla azienda                                *
      *        * - yyyy : File name originale                          *
      *        * - sqz  : Suffisso costante                            *
      *        *-------------------------------------------------------*
           05  w-cnt-fil-sqz              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di filtro richiesto                              *
      *        * - 0 : nessun filtro                                   *
      *        * - 1 : filtro standard                                 *
      *        * - 2 : filtro programma                                *
      *        *-------------------------------------------------------*
           05  w-cnt-tip-flt              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome filtro o pathname programma oggetto di filtro    *
      *        *-------------------------------------------------------*
           05  w-cnt-nop-flt              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Indice corrispondente al numero blocco da 80 caratte- *
      *        * ri in corso di trattamento                            *
      *        *-------------------------------------------------------*
           05  w-cnt-inx-blo              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per campo precedente                      *
      *        *-------------------------------------------------------*
           05  w-cnt-inx-sav              pic  9(04)                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msqzff" *
      *    *-----------------------------------------------------------*
       01  x.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        * - OP : Open                                           *
      *        * - FF : Filtering                                      *
      *        * - CL : Close                                          *
      *        *-------------------------------------------------------*
           05  x-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di filtro richiesto                              *
      *        * - 0 : Nessun filtro                                   *
      *        * - 1 : Filtro standard                                 *
      *        * - 2 : Filtro programma                                *
      *        *-------------------------------------------------------*
           05  x-tpf                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome filtro o pathname programma oggetto di filtro    *
      *        *-------------------------------------------------------*
           05  x-nmf                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Status di uscita                                      *
      *        *-------------------------------------------------------*
           05  x-sts                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Area record da filtrare e filtrato                    *
      *        *-------------------------------------------------------*
           05  x-rec.
               10  x-chr      occurs 5120 pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using s                      .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        s-ope                =    "OP"
                     perform opn-opn-000  thru opn-opn-999
           else if   s-ope                =    "FT"
                     perform flt-typ-000  thru flt-typ-999
           else if   s-ope                =    "P0"
                     perform put-zer-000  thru put-zer-999
           else if   s-ope                =    "P5"
                     perform put-cin-000  thru put-cin-999
           else if   s-ope                =    "P9"
                     perform put-nov-000  thru put-nov-999
           else if   s-ope                =    "CL"
                     perform cls-cls-000  thru cls-cls-999            .
       main-999.
           exit      program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-opn-000.
      *              *-------------------------------------------------*
      *              * Salvataggio file name originale                 *
      *              *-------------------------------------------------*
           move      s-nam                to   w-cnt-fil-nam          .
      *              *-------------------------------------------------*
      *              * Salvataggio file pathname originale             *
      *              *-------------------------------------------------*
           move      s-pat                to   w-cnt-fil-pat          .
      *              *-------------------------------------------------*
      *              * Preparazione pathname per file sequenziale      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria del pathname del- *
      *                  * la directory di base 'asc'                  *
      *                  *---------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "asc "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-cnt-fil-asc          .
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria della sigla del-  *
      *                  * l'azienda in uso                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   w-cnt-fil-azi          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-fil-sqz          .
      *                  *---------------------------------------------*
      *                  * Composizione                                *
      *                  *---------------------------------------------*
           string    w-cnt-fil-asc
                                delimited by   spaces
                     "/tmp/"
                                delimited by   size
                     w-cnt-fil-azi
                                delimited by   spaces
                     w-cnt-fil-nam
                                delimited by   spaces
                     ".sqz"
                                delimited by   size
                                          into w-cnt-fil-sqz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo di filtro richiesto        *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-tip-flt          .
      *              *-------------------------------------------------*
      *              * Normalizzazione nome filtro o pathname program- *
      *              * ma oggetto di filtro                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-nop-flt          .
       opn-opn-600.
      *              *-------------------------------------------------*
      *              * Determinazione tipo di runtime utilizzato, se   *
      *              * non gia' determinato                            *
      *              *-------------------------------------------------*
           perform   det-tru-000          thru det-tru-999            .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di runtime      *
      *              *-------------------------------------------------*
           if        w-cnt-cod-run        =    01
                     go to opn-opn-800
           else if   w-cnt-cod-run        =    02
                     go to opn-opn-900.
       opn-opn-700.
      *              *-------------------------------------------------*
      *              * Se : ACucobol 85                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      w-cnt-fil-nam        to   g-nam                  .
           move      w-cnt-fil-sqz        to   g-pat                  .
           call      "swd/mod/prg/obj/mcvo00"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita con status Ko            *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to opn-opn-999.
      *                  *---------------------------------------------*
      *                  * Tipo di Open attualmente in corso           *
      *                  *---------------------------------------------*
           move      "O"                  to   w-cnt-tip-opn          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-opn-999.
       opn-opn-800.
      *              *-------------------------------------------------*
      *              * Se : Austec RM-Master Cobol 74                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      w-cnt-fil-nam        to   g-nam                  .
           move      w-cnt-fil-sqz        to   g-pat                  .
           call      "swd/mod/prg/obj/mcvo01"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita con status Ko            *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to opn-opn-999.
      *                  *---------------------------------------------*
      *                  * Tipo di Open attualmente in corso           *
      *                  *---------------------------------------------*
           move      "O"                  to   w-cnt-tip-opn          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-opn-999.
       opn-opn-900.
      *              *-------------------------------------------------*
      *              * Se : Ryan McFarland RM-Cobol 85                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      w-cnt-fil-nam        to   g-nam                  .
           move      w-cnt-fil-sqz        to   g-pat                  .
           call      "swd/mod/prg/obj/mcvo02"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita con status Ko            *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to opn-opn-999.
      *                  *---------------------------------------------*
      *                  * Tipo di Open attualmente in corso           *
      *                  *---------------------------------------------*
           move      "O"                  to   w-cnt-tip-opn          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-opn-999.
       opn-opn-999.
           exit.

      *    *===========================================================*
      *    * Filter Type                                               *
      *    *-----------------------------------------------------------*
       flt-typ-000.
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di filtro richiesto            *
      *              *-------------------------------------------------*
           move      s-num                to   w-cnt-tip-flt          .
      *              *-------------------------------------------------*
      *              * Salvataggio nome filtro o pathname programma    *
      *              * oggetto di filtro                               *
      *              *-------------------------------------------------*
           move      s-pat                to   w-cnt-nop-flt          .
       flt-typ-200.
      *              *-------------------------------------------------*
      *              * Open modulo filtro conversione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun filtro : nessuna operazione       *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     go to flt-typ-999.
      *                  *---------------------------------------------*
      *                  * Open modulo                    "msqzff"     *
      *                  *---------------------------------------------*
           move      "OP"                 to   x-ope                  .
           move      w-cnt-tip-flt        to   x-tpf                  .
           move      w-cnt-nop-flt        to   x-nmf                  .
           call      "swd/mod/prg/obj/msqzff"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con status ad errore     *
      *                  *---------------------------------------------*
           if        x-sts                not  = spaces
                     move  "##"           to   s-sts
                     go to flt-typ-999.
      *                  *---------------------------------------------*
      *                  * Se Ok : uscita                              *
      *                  *---------------------------------------------*
           go to     flt-typ-999.
       flt-typ-999.
           exit.

      *    *===========================================================*
      *    * Put 0 : Inizio put next record to line sequential file    *
      *    *-----------------------------------------------------------*
       put-zer-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice corrispondente al nume- *
      *              * ro blocco da 80 caratteri in corso di tratta-   *
      *              * mento                                           *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-inx-blo          .
      *              *-------------------------------------------------*
      *              * Spaces in tutto il record da comporre a blocchi *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       put-zer-999.
           exit.

      *    *===========================================================*
      *    * Put 5 : Concatenamento blocco di 80 caratteri in record   *
      *    *         per put next                                      *
      *    *-----------------------------------------------------------*
       put-cin-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valore attuale indice               *
      *              *-------------------------------------------------*
           move      w-cnt-inx-blo        to   w-cnt-inx-sav          .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           string    s-alf      delimited by   size
                                          into g-rec
                                  with pointer w-cnt-inx-sav          .
      *              *-------------------------------------------------*
      *              * Incremento indice per concatenamento            *
      *              *-------------------------------------------------*
           add       80                   to   w-cnt-inx-blo          .
       put-cin-999.
           exit.

      *    *===========================================================*
      *    * Put 9 : Fine put next record to line sequential file      *
      *    *-----------------------------------------------------------*
       put-nov-000.
      *              *-------------------------------------------------*
      *              * Esecuzione filtro per mezzo del modulo "msqzff" *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun filtro : nessuna operazione       *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     go to put-nov-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record in area di link          *
      *                  *---------------------------------------------*
           move      g-rec                to   x-rec                  .
      *                  *---------------------------------------------*
      *                  * Richiamo modulo per esecuzione              *
      *                  *---------------------------------------------*
           move      "FF"                 to   x-ope                  .
           call      "swd/mod/prg/obj/msqzff"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con status di errore     *
      *                  *---------------------------------------------*
           if        x-sts                not  = spaces
                     move  "##"           to   s-sts
                     go to put-nov-999.
      *                  *---------------------------------------------*
      *                  * Ripristino record da area di link           *
      *                  *---------------------------------------------*
           move      x-rec                to   g-rec                  .
       put-nov-600.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di runtime      *
      *              *-------------------------------------------------*
           if        w-cnt-cod-run        =    01
                     go to put-nov-800
           else if   w-cnt-cod-run        =    02
                     go to put-nov-900.
       put-nov-700.
      *              *-------------------------------------------------*
      *              * Se : ACucobol 85                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvo00"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Se errore : fatal error                     *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to put-nov-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-nov-999.
       put-nov-800.
      *              *-------------------------------------------------*
      *              * Se : Austec RM-Master Cobol 74                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvo01"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Se errore : fatal error                     *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to put-nov-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-nov-999.
       put-nov-900.
      *              *-------------------------------------------------*
      *              * Se : Ryan McFarland RM-Cobol 85                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvo02"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Se errore : fatal error                     *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to put-nov-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-nov-999.
       put-nov-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-cls-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di runtime      *
      *              *-------------------------------------------------*
           if        w-cnt-cod-run        =    01
                     go to cls-cls-400
           else if   w-cnt-cod-run        =    02
                     go to cls-cls-600.
       cls-cls-200.
      *              *-------------------------------------------------*
      *              * Se : Acucobol 85                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo Open at-    *
      *                  * tualmente in corso                          *
      *                  *---------------------------------------------*
           if        w-cnt-tip-opn        =    "O"
                     go to cls-cls-240
           else      go to cls-cls-280.
       cls-cls-240.
      *                  *---------------------------------------------*
      *                  * Se tipo Open attuale : output               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo del modulo                     *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvo00"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione del modulo                *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mcvo00"                         .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita con status Ko        *
      *                      *-----------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to cls-cls-999.
       cls-cls-280.
      *                  *---------------------------------------------*
      *                  * Tipo Open attuale : nessuno                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-tip-opn          .
      *                  *---------------------------------------------*
      *                  * A chiusura modulo di filtro                 *
      *                  *---------------------------------------------*
           go to     cls-cls-800.
       cls-cls-400.
      *              *-------------------------------------------------*
      *              * Se : Austec RM-Master Cobol 74                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo Open at-    *
      *                  * tualmente in corso                          *
      *                  *---------------------------------------------*
           if        w-cnt-tip-opn        =    "O"
                     go to cls-cls-440
           else      go to cls-cls-480.
       cls-cls-440.
      *                  *---------------------------------------------*
      *                  * Se tipo Open attuale : output               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo del modulo                     *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvo01"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione del modulo                *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mcvo01"                         .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita con status Ko            *
      *                  *---------------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to cls-cls-999.
       cls-cls-480.
      *                  *---------------------------------------------*
      *                  * Tipo Open attuale : nessuno                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-tip-opn          .
      *                  *---------------------------------------------*
      *                  * A chiusura modulo di filtro                 *
      *                  *---------------------------------------------*
           go to     cls-cls-800.
       cls-cls-600.
      *              *-------------------------------------------------*
      *              * Se : Ryan McFarland RM-Cobol 85                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo Open at-    *
      *                  * tualmente in corso                          *
      *                  *---------------------------------------------*
           if        w-cnt-tip-opn        =    "O"
                     go to cls-cls-640
           else      go to cls-cls-680.
       cls-cls-640.
      *                  *---------------------------------------------*
      *                  * Se tipo Open attuale : output               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo del modulo                     *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvo02"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione del modulo                *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mcvo02"                         .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita con status Ko        *
      *                      *-----------------------------------------*
           if        g-sts                not  = e-not-err
                     perform fat-err-000  thru fat-err-999
                     move  "##"           to   s-sts
                     go to cls-cls-999.
       cls-cls-680.
      *                  *---------------------------------------------*
      *                  * Tipo Open attuale : nessuno                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-tip-opn          .
      *                  *---------------------------------------------*
      *                  * A chiusura modulo di filtro                 *
      *                  *---------------------------------------------*
           go to     cls-cls-800.
       cls-cls-800.
      *              *-------------------------------------------------*
      *              * Close modulo filtro conversione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun filtro : nessuna operazione       *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     go to cls-cls-999.
      *                  *---------------------------------------------*
      *                  * Close modulo                       "msqzff" *
      *                  *---------------------------------------------*
           move      "CL"                 to   x-ope                  .
           call      "swd/mod/prg/obj/msqzff"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con status ad errore     *
      *                  *---------------------------------------------*
           if        x-sts                not  = spaces
                     move  "##"           to   s-sts
                     go to cls-cls-999.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo               "msqzff" *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/msqzff"                         .
       cls-cls-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tipo di runtime utilizzato, se necessario, *
      *    * per chiamata del modulo di segreteria                     *
      *    *-----------------------------------------------------------*
       det-tru-000.
      *              *-------------------------------------------------*
      *              * Se gia' determinato : uscita                    *
      *              *-------------------------------------------------*
           if        w-cnt-det-run        not  = spaces
                     go to det-tru-999.
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per ottenere  *
      *              * il codice del tipo di runtime utilizzato        *
      *              *-------------------------------------------------*
           move      "R?"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione del codice del tipo di runtime   *
      *              * utilizzato                                      *
      *              *-------------------------------------------------*
           move      s-num                to   w-cnt-cod-run          .
      *              *-------------------------------------------------*
      *              * Segnale di determinazione eseguita              *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-det-run          .
       det-tru-999.
           exit.

      *    *===========================================================*
      *    * Richiamo del modulo di segreteria per l'emissione  del    *
      *    * messaggio di fatal i-o error e per la terminazione.       *
      *    *-----------------------------------------------------------*
       fat-err-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri                          *
      *              *-------------------------------------------------*
           move      "FE"                 to   s-ope                  .
           move      w-cnt-fil-nam        to   s-nam                  .
           move      w-cnt-fil-sqz        to   s-pat                  .
           move      g-sts                to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fat-err-999.
           exit.
