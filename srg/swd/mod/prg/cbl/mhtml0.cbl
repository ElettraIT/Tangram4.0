       Identification Division.
       Program-Id.                                 mhtml0             .
      *================================================================*
      *                                                                *
      *                    Modulo per generazione in formato 'html'    *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 08/05/23    *
      *                       Ultima revisione:    NdK del 30/01/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo per generare dati in formato 'html'  *
      *                                                                *
      *                    ___ DA COMPLETARE ___                       *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *                              Note                              *
      *                                                                *
      * -  Viene utilizzata l'area di interfaccia 'h'.                 *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "HH"        Header per emissione html                          *
      *                                                                *
      *             Input  : h-ope = "HH"                              *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "HF"        Header per emissione icona 'favicon'               *
      *                                                                *
      *             Input  : h-ope = "HF"                              *
      *                                                                *
      *                      h-prm = directory per l'icona             *
      *                                                                *
      *                      h-alf = link icona                        *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "HT"        Header per emissione titolo                        *
      *                                                                *
      *             Input  : h-ope = "HT"                              *
      *                                                                *
      *                      h-alf = titolo da emettere                *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "HC"        Header per link a script CSS                       *
      *                                                                *
      *             Input  : h-ope = "HC"                              *
      *                                                                *
      *                      h-prm = directory per gli script          *
      *                                                                *
      *                      h-alf = link dello script                 *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "HJ"        Header per link a script Javascript                *
      *                                                                *
      *             Input  : h-ope = "HJ"                              *
      *                                                                *
      *                      h-prm = directory per gli script          *
      *                                                                *
      *                      h-alf = link dello script                 *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "ET"        Emissione Tag                                      *
      *                                                                *
      *             Input  : h-ope = "ET"                              *
      *                                                                *
      *                      h-sub = "O" : open                        *
      *                              "I" : open, con 'id'              *
      *                              "C" : Close                       *
      *                              "V" : Open e close con valore     *
      *                                                                *
      *                      h-tag = Tag da aprire                     *
      *                                                                *
      *                      h-idx = 'id'    (facoltativo)             *
      *                                                                *
      *                      h-nam = 'name'  (facoltativo)             *
      *                                                                *
      *                      h-cla = 'class' (facoltativo)             *
      *                                                                *
      *                      h-alf = valore (facoltativo)              *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "EA"        Emissione anchor                                   *
      *                                                                *
      *             Input  : h-ope = "EA"                              *
      *                                                                *
      *                      h-hrf = href                              *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "EC"        Emissione commento                                 *
      *                                                                *
      *             Input  : h-ope = "EC"                              *
      *                                                                *
      *                      h-alf = Testo del commento                *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "TD"        Emissione Tag td o th                              *
      *                                                                *
      *             Input  : h-ope = "TD"                              *
      *                                                                *
      *                      h-sub = "td" : Tag normale                *
      *                              "th" : Tag header                 *
      *                                                                *
      *                      h-alf = Valore (alfanumerico) campo       *
      *                                                                *
      *                      h-col = Valore (alfanumerico) delle       *
      *                              colonne occupate dalla cella      *
      *                                                                *
      *                      h-all = "L" : allineamento a sinistra     *
      *                              "C" : allineamento al centro      *
      *                              "R" : allineamento a destra       *
      *                                                                *
      *                      h-wdt = "S" : Small      (Attualmente non *
      *                              "M" : Medium      gestito)        *
      *                              "B" : Big                         *
      *                                                                *
      *                      h-stl = Stile - Valori previsti:          *
      *                                                                *
      *                              - "B" : Bold (grassetto)          *
      *                              - "N" : Normale                   *
      *                              - "x" : Un colore indicato        *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "IH"        Campo hidden                                       *
      *                                                                *
      *             Input  : h-ope = "IH"                              *
      *                                                                *
      *                      h-nam = Nome campo (e ID campo)           *
      *                                                                *
      *                      h-alf = Valore (alfanumerico) campo       *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      *                                                                *
      * "IN"        Campo di Input    ___ DA IMPLEMENTARE ___          *
      *                                                                *
      *             Input  : h-ope = "IN"                              *
      *                                                                *
      *                      h-tip = tipo campo                        *
      *                                                                *
      *                      h-___ = ___                               *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "IM"        Immagine                                           *
      *                                                                *
      *             Input  : h-ope = "IM"                              *
      *                                                                *
      *                      h-src = percorso immagine                 *
      *                                                                *
      *                      h-wdt = larghezza immagine                *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "BT"        Bottone                                            *
      *                                                                *
      *             Input  : h-ope = "BT"                              *
      *                                                                *
      *                      h-nam = Nome campo (e ID campo)           *
      *                                                                *
      *                      h-tip = "but" : Bottone                   *
      *                              "sub" : Submit                    *
      *                              "res" : Reset                     *
      *                                                                *
      *                      h-cla = classe                            *
      *                                                                *
      *                      h-src = percorso immagine                 *
      *                                                                *
      *                      h-wdt = larghezza immagine                *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "DU"        Decodifica testo contenente caratteri esadecimali  *
      *             in formato 'unicode'                               *
      *                                                                *
      *             Input  : h-alf = stringa da decodificare           *
      *                                                                *
      *                      h-tip = "nnn" : nessuna trasformazione    *
      *                              "upp" : uppercase                 *
      *                              "low" : lowercase                 *
      *                                                                *
      *                                                                *
      *             Output : h-alf = stringa decodificata              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * "CY"        Emissione Copyright                                *
      *                                                                *
      *             Input  : h-ope = "CY"                              *
      *                                                                *
      *                                                                *
      *             Output : (nessuno)                                 *
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
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per emissione Tag td                           *
      *        *-------------------------------------------------------*
           05  w-exe-ttd-wdw              pic  x(20)                  .
           05  w-exe-ttd-alw              pic  x(20)                  .
           05  w-exe-ttd-tg1              pic  x(20)                  .
           05  w-exe-ttd-tg2              pic  x(30)                  .
           05  w-exe-ttd-cow              pic  x(20)                  .
           05  w-exe-ttd-st1              pic  x(25)                  .
           05  w-exe-ttd-st2              pic  x(20)                  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per decodifica 'unicode'                              *
      *        *-------------------------------------------------------*
           05  w-det-dec-unc.
      *            *---------------------------------------------------*
      *            * Stringa in input                                  *
      *            *---------------------------------------------------*
               10  w-det-dec-unc-inp      pic  x(240)                 .
      *            *---------------------------------------------------*
      *            * Stringa in output                                 *
      *            *---------------------------------------------------*
               10  w-det-dec-unc-out      pic  x(240)                 .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-dec-unc-num      pic  9(02)                  .
               10  w-det-dec-unc-ctr      pic  9(02)                  .
               10  w-det-dec-unc-max      pic  9(02) value 10         .
      *            *---------------------------------------------------*
      *            * Stringhe estratte                                 *
      *            *---------------------------------------------------*
               10  w-det-dec-unc-str  occurs  10.
                   15  w-det-dec-unc-unc  pic  x(02)                  .
                   15  w-det-dec-unc-unr  pic  x(78)                  .
      *            *---------------------------------------------------*
      *            * Tabella di decodifica caratteri unicode Latino    *
      *            * base                                              *
      *            *                                                   *
      *            * 31 elementi decodificati                          *
      *            *---------------------------------------------------*
               10  w-det-dec-unc-val.
      *                *===============================================*
      *                * Gruppo '2'                                    *
      *                *                                               *
      *                * ' '                                           *
      *                *                                               *
      *                * N.B.: il carattere 'blank' viene passato come *
      *                *       '+' dal server Web, per cui non si rie- *
      *                *       sce a tradurre come 'spazio'            *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "20"      .
                   15  filler             pic  x(01) value H"20"      .
      *                *-----------------------------------------------*
      *                * '!'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "21"      .
                   15  filler             pic  x(01) value H"21"      .
      *                *-----------------------------------------------*
      *                * '"'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "22"      .
                   15  filler             pic  x(01) value H"22"      .
      *                *-----------------------------------------------*
      *                * '#'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "23"      .
                   15  filler             pic  x(01) value H"23"      .
      *                *-----------------------------------------------*
      *                * '$'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "24"      .
                   15  filler             pic  x(01) value H"24"      .
      *                *-----------------------------------------------*
      *                * '%'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "25"      .
                   15  filler             pic  x(01) value H"25"      .
      *                *-----------------------------------------------*
      *                * '&'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "26"      .
                   15  filler             pic  x(01) value H"26"      .
      *                *-----------------------------------------------*
      *                * '''                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "27"      .
                   15  filler             pic  x(01) value H"27"      .
      *                *-----------------------------------------------*
      *                * '('                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "28"      .
                   15  filler             pic  x(01) value H"28"      .
      *                *-----------------------------------------------*
      *                * ')'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "29"      .
                   15  filler             pic  x(01) value H"29"      .
      *                *-----------------------------------------------*
      *                * '*'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "2A"      .
                   15  filler             pic  x(01) value H"2A"      .
      *                *-----------------------------------------------*
      *                * '+'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "2B"      .
                   15  filler             pic  x(01) value H"2B"      .
      *                *-----------------------------------------------*
      *                * ','                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "2C"      .
                   15  filler             pic  x(01) value H"2C"      .
      *                *-----------------------------------------------*
      *                * '-'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "2D"      .
                   15  filler             pic  x(01) value H"2D"      .
      *                *-----------------------------------------------*
      *                * '.'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "2E"      .
                   15  filler             pic  x(01) value H"2E"      .
      *                *-----------------------------------------------*
      *                * '/'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "2F"      .
                   15  filler             pic  x(01) value H"2F"      .
      *                *===============================================*
      *                * Gruppo '3'                                    *
      *                *                                               *
      *                * ':'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "3A"      .
                   15  filler             pic  x(01) value H"3A"      .
      *                *-----------------------------------------------*
      *                * ';'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "3B"      .
                   15  filler             pic  x(01) value H"3B"      .
      *                *-----------------------------------------------*
      *                * '<'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "3C"      .
                   15  filler             pic  x(01) value H"3C"      .
      *                *-----------------------------------------------*
      *                * '='                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "3D"      .
                   15  filler             pic  x(01) value H"3D"      .
      *                *-----------------------------------------------*
      *                * '>'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "3E"      .
                   15  filler             pic  x(01) value H"3E"      .
      *                *-----------------------------------------------*
      *                * '?'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "3F"      .
                   15  filler             pic  x(01) value H"3F"      .
      *                *===============================================*
      *                * Gruppo '4'                                    *
      *                *                                               *
      *                * '@'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "40"      .
                   15  filler             pic  x(01) value H"40"      .
      *                *===============================================*
      *                * Gruppo '5'                                    *
      *                *                                               *
      *                * '['                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "5B"      .
                   15  filler             pic  x(01) value H"5B"      .
      *                *-----------------------------------------------*
      *                * '\'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "5C"      .
                   15  filler             pic  x(01) value H"5C"      .
      *                *-----------------------------------------------*
      *                * ']'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "5D"      .
                   15  filler             pic  x(01) value H"5D"      .
      *                *-----------------------------------------------*
      *                * '^'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "5E"      .
                   15  filler             pic  x(01) value H"5E"      .
      *                *-----------------------------------------------*
      *                * '_'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "5F"      .
                   15  filler             pic  x(01) value H"5F"      .
      *                *===============================================*
      *                * Gruppo '7'                                    *
      *                *                                               *
      *                * '{'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "7B"      .
                   15  filler             pic  x(01) value H"7B"      .
      *                *-----------------------------------------------*
      *                * '|'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "7C"      .
                   15  filler             pic  x(01) value H"7C"      .
      *                *-----------------------------------------------*
      *                * '}'                                           *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02) value  "7D"      .
                   15  filler             pic  x(01) value H"7D"      .
      *            *---------------------------------------------------*
      *            * Ridefinizione tabella                             *
      *            *---------------------------------------------------*
               10  w-det-dec-unc-r01 redefines
                   w-det-dec-unc-val.
      *                *-----------------------------------------------*
      *                * Elementi tabella                              *
      *                *-----------------------------------------------*
                   15  w-det-dec-unc-ele occurs 31
                                        indexed by w-det-dec-unc-inx  .
      *                    *-------------------------------------------*
      *                    * Valore esadecimale                        *
      *                    *-------------------------------------------*
                       20  w-det-dec-unc-cod  pic  x(02)              .
      *                    *-------------------------------------------*
      *                    * Carattere unicode                         *
      *                    *-------------------------------------------*
                       20  w-det-dec-unc-chr  pic  x(01)              .

      *    *===========================================================*
      *    * Work-area per editing ed allineamenti                     *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-edt-dat-exe              pic  9(07)                  .

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
      *    * Area di comunicazione per modulo                "mhtml0"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/h"                                  .

      ******************************************************************
       Procedure Division                using h                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        h-ope                =    "HH"
                     perform   hea-000    thru hea-999
           else if   h-ope                =    "HF"
                     perform   hef-000    thru hef-999
           else if   h-ope                =    "HT"
                     perform   het-000    thru het-999
           else if   h-ope                =    "HC"
                     perform   hec-000    thru hec-999
           else if   h-ope                =    "HJ"
                     perform   hej-000    thru hej-999
           else if   h-ope                =    "EC"
                     perform   ecm-000    thru ecm-999
           else if   h-ope                =    "EA"
                     perform   eac-000    thru eac-999
           else if   h-ope                =    "ET"
                     perform   emt-000    thru emt-999
           else if   h-ope                =    "IH"
                     perform   hid-000    thru hid-999
           else if   h-ope                =    "TD"
                     perform   etd-000    thru etd-999
           else if   h-ope                =    "IN"
                     perform   inp-000    thru inp-999
           else if   h-ope                =    "IM"
                     perform   imm-000    thru imm-999
           else if   h-ope                =    "BT"
                     perform   but-000    thru but-999
           else if   h-ope                =    "DU"
                     perform   dun-000    thru dun-999
           else if   h-ope                =    "CY"
                     perform   cpy-000    thru cpy-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Header                                                    *
      *    *-----------------------------------------------------------*
       hea-000.
      *              *-------------------------------------------------*
      *              * Premessa                                        *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   "<!DOCTYPE html>"                                .
           display   "<html>"                                         .
           display   "<head>"                                         .
       hea-999.
           exit.

      *    *===========================================================*
      *    * Favicon                                                   *
      *    *-----------------------------------------------------------*
       hef-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<link rel='icon' type='image/x-icon' href='"
                                          to   w-all-str-cat (1)      .
      *
           if        h-prm                =    spaces
                     move  spaces         to   w-all-str-cat (2)
           else      move  h-prm          to   w-all-str-cat (2)      .
      *
           move      h-alf                to   w-all-str-cat (3)      .
           move      "'>"                 to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       hef-999.
           exit.

      *    *===========================================================*
      *    * Header - titolo                                           *
      *    *-----------------------------------------------------------*
       het-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        h-alf                =    spaces
                     go to het-999.
      *              *-------------------------------------------------*
      *              * Apertura 'title'                                *
      *              *-------------------------------------------------*
           move      "title"              to   h-tag                  .
           move      "O"                  to   h-sub                  .
           perform   emt-000              thru emt-999                .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   h-alf                                            .
      *              *-------------------------------------------------*
      *              * Chiusura 'title'                                *
      *              *-------------------------------------------------*
           move      "title"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           perform   emt-000              thru emt-999                .
       het-999.
           exit.

      *    *===========================================================*
      *    * Header - script CSS                                       *
      *    *-----------------------------------------------------------*
       hec-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        h-alf                =    spaces
                     go to hec-999.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<link rel='stylesheet' type='text/css' href='"
                                          to   w-all-str-cat (1)      .
      *
           if        h-prm                =    spaces
                     move  spaces         to   w-all-str-cat (2)
           else      move  h-prm          to   w-all-str-cat (2)      .
      *
           move      h-alf                to   w-all-str-cat (3)      .
           move      "'>"                 to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       hec-999.
           exit.

      *    *===========================================================*
      *    * Header - script Javascript                                *
      *    *-----------------------------------------------------------*
       hej-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        h-alf                =    spaces
                     go to hej-999.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<script src='"      to   w-all-str-cat (1)      .
      *
           if        h-prm                =    spaces
                     move  spaces         to   w-all-str-cat (2)
           else      move  h-prm          to   w-all-str-cat (2)      .
      *
           move      h-alf                to   w-all-str-cat (3)      .
           move      "'></script>"        to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       hej-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag semplice                                    *
      *    *-----------------------------------------------------------*
       ecm-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        h-alf                =    spaces
                     go to ecm-999.
      *              *-------------------------------------------------*
      *              * Fincatura superiore                             *
      *              *-------------------------------------------------*
           display   "<!-- *********************************** -->"   .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<!--"               to   w-all-str-cat (1)      .
           move      h-alf                to   w-all-str-cat (2)      .
           move      "-->"                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Fincatura inferiore                             *
      *              *-------------------------------------------------*
           display   "<!-- *********************************** -->"   .
       ecm-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag 'anchor'                                    *
      *    *-----------------------------------------------------------*
       eac-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        h-hrf                =    spaces
                     go to eac-999.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<a href='"          to   w-all-str-cat (1)      .
           move      h-hrf                to   w-all-str-cat (2)      .
           move      "'>"                 to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       eac-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag semplice                                    *
      *    *-----------------------------------------------------------*
       emt-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        h-tag                =    spaces
                     go to emt-999.
      *              *-------------------------------------------------*
      *              * Test se Tag con identificativi                  *
      *              *-------------------------------------------------*
           if        h-sub                =    "I"
                     go to emt-300
           else if   h-sub                =    "V"
                     go to emt-500
           else      go to emt-100.
       emt-100.
      *              *=================================================*
      *              * Assemblaggio Tag senza identificativi           *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
      *
           if        h-sub                =    "O"
                     move  spaces         to   w-all-str-cat (2)
           else      move  "/"            to   w-all-str-cat (2)      .
      *
           move      h-tag                to   w-all-str-cat (3)      .
           move      ">"                  to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emt-999.
       emt-300.
      *              *=================================================*
      *              * Assemblaggio Tag con identificativi             *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
           move      h-tag                to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "id='"               to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      07                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      h-idx                to   w-all-str-cat (2)      .
           move      "' name='"           to   w-all-str-cat (3)      .
           move      h-nam                to   w-all-str-cat (4)      .
      *
           if        h-cla                =    spaces
                     move  spaces         to   w-all-str-cat (5)
                     move  spaces         to   w-all-str-cat (6)
           else      move  "' class='"    to   w-all-str-cat (5)
                     move  h-cla          to   w-all-str-cat (6)      .
      *
           move      "'>"                 to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emt-999.
       emt-500.
      *              *=================================================*
      *              * Assemblaggio Tag con valore                     *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
           move      h-tag                to   w-all-str-cat (2)      .
           move      ">"                  to   w-all-str-cat (3)      .
           move      h-alf                to   w-all-str-cat (4)      .
           move      "</"                 to   w-all-str-cat (5)      .
           move      h-tag                to   w-all-str-cat (6)      .
           move      ">"                  to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emt-999.
       emt-999.
           exit.

      *    *===========================================================*
      *    * Emissione input hidden                                    *
      *    *-----------------------------------------------------------*
       hid-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        h-nam                =    spaces
                     go to hid-999.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "<input type='hidden' name='"
                                          to   w-all-str-cat (1)      .
           move      h-nam                to   w-all-str-cat (2)      .
           move      "' id='"             to   w-all-str-cat (3)      .
           move      h-nam                to   w-all-str-cat (4)      .
           move      "' value='"          to   w-all-str-cat (5)      .
           move      h-alf                to   w-all-str-cat (6)      .
           move      "'>"                 to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Emissione TAG                                   *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       hid-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag 'td' o 'th'                                 *
      *    *-----------------------------------------------------------*
       etd-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione contenuto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tag                                         *
      *                  *---------------------------------------------*
           if        h-sub                =    "td"
                     move  "<td"          to   w-exe-ttd-tg1
                     move  "</td>"        to   w-exe-ttd-tg2
           else      move  "<th"          to   w-exe-ttd-tg1
                     move  "</th>"        to   w-exe-ttd-tg2          .
      *                  *---------------------------------------------*
      *                  * Testo                                       *
      *                  *---------------------------------------------*
           if        h-alf                =    spaces
                     move  "&nbsp;"       to   h-alf                  .
      *                  *---------------------------------------------*
      *                  * Larghezza                                   *
      *                  *                                             *
      *                  * ATTUALMENTE FORZATA A SPAZI                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-ttd-wdw          .
      *                  *---------------------------------------------*
      *                  * Allineamento                                *
      *                  *---------------------------------------------*
           if        h-all                =    "L"
                     move  "align='left'" 
                                          to   w-exe-ttd-alw
           else if   h-all                =    "R"
                     move  "align='right'"
                                          to   w-exe-ttd-alw
           else if   h-all                =    "C"
                     move  "align='center'"
                                          to   w-exe-ttd-alw
           else      move  spaces         to   w-exe-ttd-alw          .
      *                  *---------------------------------------------*
      *                  * Colonne                                     *
      *                  *---------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "colspan='"          to   w-all-str-cat (1)      .
           move      h-col                to   w-all-str-cat (2)      .
           move      "'"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-ttd-cow          .
      *                      *-----------------------------------------*
      *                      * Se una non si emette                    *
      *                      *-----------------------------------------*
           if        h-col                =    "1"
                     move  spaces         to   w-exe-ttd-cow          .   
      *                  *---------------------------------------------*
      *                  * Stile                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bold                                    *
      *                      *-----------------------------------------*
           if        h-stl                =    "B"
                     move  "<B>"          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2
           else      move  spaces         to   w-exe-ttd-st1
                     move  spaces         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Testo rosso                             *
      *                      *-----------------------------------------*
           if        h-stl                =    "r"
                     move  "<B style='color:red'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Testo grigio                            *
      *                      *-----------------------------------------*
           if        h-stl                =    "g"
                     move  "<B style='color:grey'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Testo blu                               *
      *                      *-----------------------------------------*
           if        h-stl                =    "b"
                     move  "<B style='color:blue'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Testo verde                             *
      *                      *-----------------------------------------*
           if        h-stl                =    "v"
                     move  "<B style='color:green'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Testo arancio                           *
      *                      *-----------------------------------------*
           if        h-stl                =    "o"
                     move  "<B style='color:orange'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
       etd-800.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      09                   to   w-all-str-num          .
           move      w-exe-ttd-tg1        to   w-all-str-cat (1)      .
           move      w-exe-ttd-cow        to   w-all-str-cat (2)      .
           move      w-exe-ttd-alw        to   w-all-str-cat (3)      .
           move      w-exe-ttd-wdw        to   w-all-str-cat (4)      .
           move      ">"                  to   w-all-str-cat (5)      .
           move      w-exe-ttd-st1        to   w-all-str-cat (6)      .
           move      h-alf                to   w-all-str-cat (7)      .
           move      w-exe-ttd-st2        to   w-all-str-cat (8)      .
           move      w-exe-ttd-tg2        to   w-all-str-cat (9)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Emissione TAG 'td' o 'th'                       *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       etd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     etd-999.
       etd-999.
           exit.

      *    *===========================================================*
      *    * Input                                                     *
      *    *-----------------------------------------------------------*
       inp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
       inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     inp-999.
       inp-999.
           exit.

      *    *===========================================================*
      *    * Immagine                                                  *
      *    *-----------------------------------------------------------*
       imm-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      "<img src='"         to   w-all-str-cat (1)      .
           move      h-src                to   w-all-str-cat (2)      .
           move      "' width='"          to   w-all-str-cat (3)      .
           move      h-wdt                to   w-all-str-cat (4)      .
           move      "'>"                 to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Emissione TAG                                   *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       imm-999.
           exit.

      *    *===========================================================*
      *    * Bottone                                                   *
      *    *-----------------------------------------------------------*
       but-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio parametri bottone                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri stringa                           *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      09                   to   w-all-str-num          .
      *                  *---------------------------------------------*
      *                  * Tipo bottone                                *
      *                  *---------------------------------------------*
           move      "<button type='"     to   w-all-str-cat (1)      .
      *
           if        h-tip                =    "but"
                     move  "button"       to   w-all-str-cat (2)
           else if   h-tip                =    "sub"
                     move  "submit"       to   w-all-str-cat (2)
           else if   h-tip                =    "res"
                     move  "reset"        to   w-all-str-cat (2)
           else      move  "button"       to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Nome e ID, se presenti                      *
      *                  *---------------------------------------------*
           if        h-nam                not  = spaces
                     move   "' name='"    to   w-all-str-cat (3)
                     move   h-nam         to   w-all-str-cat (4)
                     move   "' id='"      to   w-all-str-cat (5)
                     move   h-nam         to   w-all-str-cat (6)
           else      move   spaces        to   w-all-str-cat (3)
                     move   spaces        to   w-all-str-cat (4)
                     move   spaces        to   w-all-str-cat (5)
                     move   spaces        to   w-all-str-cat (6)      .
      *                  *---------------------------------------------*
      *                  * Classe, se presente                         *
      *                  *---------------------------------------------*
           if        h-cla                not  = spaces
                     move   "' class='"   to   w-all-str-cat (7)
                     move   h-cla         to   w-all-str-cat (8)
           else      move   spaces        to   w-all-str-cat (7)
                     move   spaces        to   w-all-str-cat (8)      .
      *                  *---------------------------------------------*
      *                  * Chiusura TAG                                *
      *                  *---------------------------------------------*
           move      "'>"                 to   w-all-str-cat (9)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Emissione parametri bottone                     *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       but-200.
      *              *-------------------------------------------------*
      *              * Assemblaggio immagine bottone                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se immagine associata                  *
      *                  *---------------------------------------------*
           if        h-src                =    spaces
                     go to but-800.
      *                  *---------------------------------------------*
      *                  * Parametri stringa                           *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
      *                  *---------------------------------------------*
      *                  * Definizione immagine                        *
      *                  *---------------------------------------------*
           move      "<img src='"         to   w-all-str-cat (1)      .
           move      h-src                to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Larghezza immagine                          *
      *                  *---------------------------------------------*
           if        h-wdt                not  = spaces
                     move   "' width='"   to   w-all-str-cat (3)
                     move   h-wdt         to   w-all-str-cat (4)
           else      move   spaces        to   w-all-str-cat (3)
                     move   spaces        to   w-all-str-cat (4)      .
      *                  *---------------------------------------------*
      *                  * Chiusura TAG                                *
      *                  *---------------------------------------------*
           move      "'>"                 to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Emissione parametri immagine                    *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       but-800.
      *              *-------------------------------------------------*
      *              * Emissione chiusura bottone                      *
      *              *-------------------------------------------------*
           display   "</button>"                                      .
       but-999.
           exit.

      *    *===========================================================*
      *    * Decodifica caratteri Unicode Latino Base                  *
      *    *                                                           *
      *    * La ricodifica prevede un massimo di 10 caratteri in una   *
      *    * stringa alfanumerica                                      *
      *    *                                                           *
      *    * Caratteri Unicode attualmente implementati                *
      *    * ------------------------------------------                *
      *    *                                                           *
      *    * - %20 =        (blank)                                    *
      *    * - %21 = !      (*)                                        *
      *    * - %22 = "                                                 *
      *    * - %23 = #                                                 *
      *    * - %24 = $      (*)                                        *
      *    * - %25 = %      (*)                                        *
      *    * - %26 = &      (*)                                        *
      *    * - %27 = '                                                 *
      *    * - %28 = (                                                 *
      *    * - %29 = )                                                 *
      *    * - %2A = *      (*)                                        *
      *    * - %2B = +      (*)                                        *
      *    * - %2C = ,                                                 *
      *    * - %2D = -      (*)                                        *
      *    * - %2E = .                                                 *
      *    * - %2F = /                                                 *
      *    *                                                           *
      *    * - %3A = :                                                 *
      *    * - %3B = ;                                                 *
      *    * - %3C = <                                                 *
      *    * - %3D = =                                                 *
      *    * - %3E = >                                                 *
      *    * - %3F = ?      (*)                                        *
      *    *                                                           *
      *    * - %40 = @                                                 *
      *    *                                                           *
      *    * - %5B = [                                                 *
      *    * - %5C = \                                                 *
      *    * - %5D = ]                                                 *
      *    * - %5E = ^                                                 *
      *    * - %5F = _      (*)                                        *
      *    *                                                           *
      *    * - %7B = {                                                 *
      *    * - %7C = |                                                 *
      *    * - %7D = }                                                 *
      *    *                                                           *
      *    * - %C2%A3 = £ (Non imputabile in Tangram) ___ NO ___       *
      *    *                                                           *
      *    * N.B.: Attualmente NON vengono decodificati i codici dop-  *
      *    *       pi come '£'                                         *
      *    *                                                           *
      *    *       Valutare di ignorare i pre-codici come '%C2'        *
      *    *                                                           *
      *    *       (*) Carattere per la password                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dun-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-dec-unc-out      .
           move      h-alf                to   w-det-dec-unc-inp      .
      *              *-------------------------------------------------*
      *              * Test su valore del campo in input               *
      *              *-------------------------------------------------*
           if        w-det-dec-unc-inp    =    spaces
                     go to dun-900.
       dun-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione stringhe da estrarre            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dec-unc-ctr      .
       dun-120.
           add       1                    to   w-det-dec-unc-ctr      .
           if        w-det-dec-unc-ctr    >    w-det-dec-unc-max
                     go to dun-150.
           move      spaces               to   w-det-dec-unc-unc 
                                              (w-det-dec-unc-ctr)     .
           move      spaces               to   w-det-dec-unc-unr 
                                              (w-det-dec-unc-ctr)     .
           go to     dun-120.
       dun-150.
      *              *-------------------------------------------------*
      *              * Test se presente il carattere '%'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-inp    to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-chr-000      thru all-str-chr-999        .
      *                  *---------------------------------------------*
      *                  * Se non trovato: ad uscita                   *
      *                  *---------------------------------------------*
           if        w-all-str-num        =    zero
                     move  w-det-dec-unc-inp
                                          to   w-det-dec-unc-out
                     go to dun-800.
       dun-200.
      *              *-------------------------------------------------*
      *              * Estrazione di 'n' stringhe separate da '%'      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scomposizione                               *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-inp    to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione elementi                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-dec-unc-ctr      .
       dun-220.
           add       1                    to   w-det-dec-unc-ctr      .
      *                  *---------------------------------------------*
      *                  * Test se superato il massimo di max stringhe *
      *                  *---------------------------------------------*
           if        w-det-dec-unc-ctr    >    w-det-dec-unc-max
                     go to dun-300.
      *                  *---------------------------------------------*
      *                  * Test se stringa a spazi                     *
      *                  *---------------------------------------------*
           if        w-all-str-cat
                    (w-det-dec-unc-ctr)   =    spaces
                     go to dun-220.
      *                  *---------------------------------------------*
      *                  * Memorizzazione eventuale 'unicode'          *
      *                  *---------------------------------------------*
           move      w-all-str-cat
                    (w-det-dec-unc-ctr)
                    (01 : 02)             to   w-det-dec-unc-unc
                                              (w-det-dec-unc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Memorizzazione restante stringa             *
      *                  *---------------------------------------------*
           move      w-all-str-cat
                    (w-det-dec-unc-ctr)
                    (03 : 78)             to   w-det-dec-unc-unr
                                              (w-det-dec-unc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     dun-220.
       dun-300.
      *              *-------------------------------------------------*
      *              * Scansione elementi estratti                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo per max stringhe                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-dec-unc-ctr      .
       dun-320.
           add       1                    to   w-det-dec-unc-ctr      .
      *                  *---------------------------------------------*
      *                  * Test se superato il massimo di max stringhe *
      *                  *---------------------------------------------*
           if        w-det-dec-unc-ctr    >    w-det-dec-unc-max
                     go to dun-800.
      *                  *---------------------------------------------*
      *                  * Test se entrambe le stringhe di decodifica  *
      *                  * a spazi                                     *
      *                  *---------------------------------------------*
           if        w-det-dec-unc-unc
                    (w-det-dec-unc-ctr)   =    spaces and
                     w-det-dec-unc-unr
                    (w-det-dec-unc-ctr)   =    spaces
                     go to dun-320.
      *                  *---------------------------------------------*
      *                  * Ricerca codice 'unicode' esadecimale        *
      *                  *---------------------------------------------*
           set       w-det-dec-unc-inx    to   1                      .
           search    w-det-dec-unc-ele
                     when    w-det-dec-unc-cod
                            (w-det-dec-unc-inx)
                                          =    w-det-dec-unc-unc
                                              (w-det-dec-unc-ctr)
                     go to dun-340.
       dun-330.
      *                  *---------------------------------------------*
      *                  * Se codice NON trovato: ad assemblaggio      *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-unc
                    (w-det-dec-unc-ctr)
                                          to   w-all-str-cat (2)      .
           move      w-det-dec-unc-ele
                    (w-det-dec-unc-ctr)   to   w-all-str-cat (3)      .
      *                  *---------------------------------------------*
      *                  * Ad assemblaggio                             *
      *                  *---------------------------------------------*
           go to dun-400.
       dun-340.
      *                  *---------------------------------------------*
      *                  * Se codice trovato: ricodifica               *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-chr
                    (w-det-dec-unc-inx)   to   w-all-str-cat (2)      .
      
           move      w-det-dec-unc-unr
                    (w-det-dec-unc-ctr)   to   w-all-str-cat (3)      .
       dun-400.
      *                  *---------------------------------------------*
      *                  * Riassemblaggio                              *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-det-dec-unc-out    to   w-all-str-cat (1)      .
      *
           move      w-det-dec-unc-unr
                    (w-det-dec-unc-ctr)   to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-det-dec-unc-out      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to dun-320.
       dun-800.
      *              *-------------------------------------------------*
      *              * Valore determinato                              *
      *              *-------------------------------------------------*
           move      w-det-dec-unc-out    to   h-alf                  .
      *              *-------------------------------------------------*
      *              * Eventuale trasformazione                        *
      *              *-------------------------------------------------*
           if        h-tip                =    "upp"
                     go to dun-820
           else if   h-tip                =    "low"
                     go to dun-840
           else      go to dun-900.
       dun-820.
      *              *-------------------------------------------------*
      *              * Uppercase                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza stringa            *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-out    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
      *                  *---------------------------------------------*
      *                  * Routine                                     *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-out    to   w-all-str-alf          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   h-alf                  .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dun-900.
       dun-840.
      *              *-------------------------------------------------*
      *              * Lowercase                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza stringa            *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-out    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
      *                  *---------------------------------------------*
      *                  * Routine                                     *
      *                  *---------------------------------------------*
           move      w-det-dec-unc-out    to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dun-900.
       dun-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dun-999.
       dun-999.
           exit.

      *    *===========================================================*
      *    * Copyright                                                 *
      *    *-----------------------------------------------------------*
       cpy-000.
      *              *-------------------------------------------------*
      *              * Box per Copyright                               *
      *              *-------------------------------------------------*
           display   "<div id='copy' name='copy'>"                    .
           display   "<address>"                                      .
           display   "<font size=3>&copy wip - tangram"               .
           display   "</address>"                                     .
           display   "</div>"                                         .
       cpy-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
