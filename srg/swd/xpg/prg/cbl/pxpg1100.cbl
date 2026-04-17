       Identification Division.
       Program-Id.                                 pxpg1100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    lpg                 *
      *                                   Fase:    xpg110              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Ultima revisione:    NdK del 23/03/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione file per licenza d'uso [lcu]       *
      *                                                                *
      *                    PASSWORD DI SBLOCCO : "lascis88"            *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [upn]                                        *
      *    *-----------------------------------------------------------*
           select  optional  upn   assign to disk         f-upn-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is upn-key
                             file status  is f-upn-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *================================================================*
      *    File Description [upn]                                      *
      *----------------------------------------------------------------*
       fd  upn       label record standard                            .
       01  upn-rec.
           05  upn-key.
               10  upn-tre                pic  x(04)                  .
               10  upn-kre                pic  x(12)                  .
           05  upn-dat.
               10  upn-prg                pic  9(04)                  .
               10  upn-l01                pic  x(12)                  .
               10  upn-l02                pic  x(12)                  .
               10  upn-l03                pic  x(12)                  .
               10  upn-l04                pic  x(12)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "swd"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "xpg"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "lpg"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg110"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg1100  "                                     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "             LICENZA D'USO              "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .
       
      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
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
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [upn]                *
      *    *-----------------------------------------------------------*
       01  f-upn.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-upn-nam                  pic  x(04) value "upn "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-upn-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-upn-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine det-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-det-lic-uso      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine ges-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-ges-lic-uso      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-lic-uso      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine scr-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-scr-lic-uso      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine ins-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-ins-lic-uso      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine mod-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-mod-lic-uso      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine acc-lic-uso-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-acc-lic-uso      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di controllo per primo giro di esecuzione        *
      *        *-------------------------------------------------------*
           05  w-cnt-uno.
               10  w-cnt-uno-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di controllo per tipo funzionamento              *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
               10  w-cnt-tip-fun          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per controllo chiamate a modulo "mopsys"        *
      *    *-----------------------------------------------------------*
       01  w-ops.
      *        *-------------------------------------------------------*
      *        * Tipo di controllo da effettuarsi sulla licenza d'uso, *
      *        * come stabilito in funzione del sistema operativo o-   *
      *        * spite                                                 *
      *        *  - "00" : Nessun controllo                            *
      *        *  - "01" : Controllo solo su licenziatario             *
      *        *  - "02" : Controllo sia sul licenziatario che sul nu- *
      *        *           mero di utenti                              *
      *        *-------------------------------------------------------*
           05  w-ops-tlu                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Status                                                *
      *        *-------------------------------------------------------*
           05  w-ops-sts                  pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per denominazione licenziatario d'uso           *
      *    *-----------------------------------------------------------*
       01  w-lic.
      *        *-------------------------------------------------------*
      *        * Area per composizione parola passepartout             *
      *        *-------------------------------------------------------*
           05  w-lic-ppt.
               10  w-lic-ppx  occurs 08   pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per Host-Id ottenuto dalla segreteria            *
      *        *-------------------------------------------------------*
           05  w-lic-hoi                  pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione record di file [lcu]                    *
      *        *-------------------------------------------------------*
           05  w-lic-rec.
      *            *---------------------------------------------------*
      *            * Password di controllo                             *
      *            *---------------------------------------------------*
               10  w-lic-rec-pwd          pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Max numero utenti                                 *
      *            *---------------------------------------------------*
               10  w-lic-rec-nux.
                   15  w-lic-rec-nut      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale                                   *
      *            *---------------------------------------------------*
               10  w-lic-rec-rag          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Scadenza                                          *
      *            *---------------------------------------------------*
               10  w-lic-rec-scx.
                   15  w-lic-rec-scd      pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Area per password risultante dalla crittografazione   *
      *        * del numero max di utenti concatenato con la ragione   *
      *        * sociale e con l' Host-Id, per i caratteri 45..52      *
      *        *-------------------------------------------------------*
           05  w-pwd-045-052              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Messaggio di errore su file di licenza d'uso          *
      *        *-------------------------------------------------------*
           05  w-lic-des-err              pic  x(76)                  .
      *        *-------------------------------------------------------*
      *        * Contatore e massimo per deterrente a reiterazione     *
      *        *-------------------------------------------------------*
           05  w-lic-ctr-det              pic  9(03)                  .
           05  w-lic-ctr-max              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Area per impostazione                                 *
      *        *-------------------------------------------------------*
           05  w-lic-acc.
               10  w-lic-acc-rag          pic  x(40)                  .
               10  w-lic-acc-nux.
                   15  w-lic-acc-nut      pic  9(04)                  .
               10  w-lic-acc-scx.
                   15  w-lic-acc-scd      pic  9(04)                  .
               10  w-lic-acc-hoi          pic  x(20)                  .
               10  w-lic-acc-pwd          pic  x(08)                  .

      *    *===========================================================*
      *    * Work-area per funzione di crittografazione                *
      *    *-----------------------------------------------------------*
       01  w-cry.
      *        *-------------------------------------------------------*
      *        * Area parametri                                        *
      *        *-------------------------------------------------------*
           02  w-cry-prm.
               03  w-cry-are.
                   04  w-cry-arx occurs 64
                                          pic  x(01)                  .
               03  w-cry-cnt.
                   04  w-cry-dpl          pic  9(04)                  .
                   04  w-cry-sum          pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Area di lavoro vera e propria                         *
      *        *-------------------------------------------------------*
           02  w-cry-t01.
               03  w-cry-tbl              pic  x(95) value
                     "h%2:JB *d)g<C+j(Rm|7lA9t.a~ye8z@E0n,^Z?4bs}cMN>i1x
      -              "[`5Fr-Ou#Yqf\XP6W=I/wvUH3;To{L$QpK_]G&S'V!Dk""" .
           02  w-cry-t02                  redefines
               w-cry-t01                                              .
               03  w-cry-tbx    occurs 95 pic  x(01)                  .
           02  w-cry-t03.
               03  w-cry-tbm              pic  x(95) value
                     "hf2ejbdcdbgaczjyrmx7la9twavye8zue0ntszr4bsqcmnpi1x
      -              "on5frmoulyqfkxp6wjiiwvuh3htoglfqpkedg&scvbdka" .
           02  w-cry-t04                  redefines
               w-cry-t03                                              .
               03  w-cry-tby    occurs 95 pic  x(01)                  .
           02  w-cry-i01                  pic  9(03)                  .
           02  w-cry-i02                  pic  9(03)                  .
           02  w-cry-i03                  pic  9(03)                  .

      *    *===========================================================*
      *    * Work per variabili di i.p.c.                              *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabile di i.p.c. 'lic-fun' letta ad inizio pro-    *
      *        * gramma per selezione automatica del tipo funzione     *
      *        * - G : Gestione                                        *
      *        * - D : Determinazione                                  *
      *        *-------------------------------------------------------*
           05  w-ipc-lic-fun              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Variabile di i.p.c. 'lic-uso' scritta in risposta al  *
      *        * tipo funzione 'D'                                     *
      *        *-------------------------------------------------------*
           05  w-ipc-lic-uso.
      *            *---------------------------------------------------*
      *            * Esito                                             *
      *            * - S : Licenza d'uso esistente e corretta          *
      *            * - N : Licenza d'uso non esistente o scorretta     *
      *            * - E : Errore grave in lettura licenza d'uso       *
      *            *---------------------------------------------------*
               10  w-ipc-lic-sne          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale intestatario licenza              *
      *            *---------------------------------------------------*
               10  w-ipc-lic-rag          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Max numero utenti                                 *
      *            *---------------------------------------------------*
               10  w-ipc-lic-nut          pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Scadenza licenza                                  *
      *            *---------------------------------------------------*
               10  w-ipc-lic-scd          pic  9(04)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Test su variabile di i.p.c. 'lic-fun'           *
      *              *-------------------------------------------------*
           if        w-ipc-lic-fun        =    "D"
                     go to main-300
           else      go to main-600.
       main-300.
      *              *-------------------------------------------------*
      *              * Determinazione licenza d'uso                    *
      *              *-------------------------------------------------*
           perform   det-lic-uso-000      thru det-lic-uso-999        .
           go to     main-800.
       main-600.
      *              *-------------------------------------------------*
      *              * Gestione licenza d'uso                          *
      *              *-------------------------------------------------*
           perform   ges-lic-uso-000      thru ges-lic-uso-999        .
       main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Flag di controllo per primo giro di esecuzione  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-uno-flg          .
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Funzione di determinazione tipo di licenza d'u- *
      *              * so a seconda del sistema operativo ospite       *
      *              *-------------------------------------------------*
           move      "LU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione del tipo di licenza d'uso a se-  *
      *              * conda del sistema operativo ospite              *
      *              *-------------------------------------------------*
           move      o-sts                to   w-ops-tlu              .
      *              *-------------------------------------------------*
      *              * Richiesta Host-Id dalla segreteria e sua memo-  *
      *              * rizzazione                                      *
      *              *-------------------------------------------------*
           move      "H?"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-lic-hoi              .
      *              *-------------------------------------------------*
      *              * Composizione parola passepartout                *
      *              *-------------------------------------------------*
           move      "s"                  to   w-lic-ppx (03)         .
           move      "8"                  to   w-lic-ppx (07)         .
           move      "8"                  to   w-lic-ppx (08)         .
           move      "c"                  to   w-lic-ppx (04)         .
           move      "a"                  to   w-lic-ppx (02)         .
           move      "l"                  to   w-lic-ppx (01)         .
           move      "i"                  to   w-lic-ppx (05)         .
           move      "s"                  to   w-lic-ppx (06)         .
      *              *-------------------------------------------------*
      *              * Lettura variabile di i.p.c. 'lic-fun'           *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "lic-fun"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces and
                     s-tip                =    "A"
                     move  s-alf          to   w-ipc-lic-fun
           else      move  spaces         to   w-ipc-lic-fun          .
           if        w-ipc-lic-fun        not  = "G" and
                     w-ipc-lic-fun        not  = "D"
                     move  spaces         to   w-ipc-lic-fun          .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione licenza d'uso                              *
      *    *-----------------------------------------------------------*
       det-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita da programma   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-det-lic-uso      .
      *              *-------------------------------------------------*
      *              * Lettura file [lcu]                              *
      *              *-------------------------------------------------*
           perform   let-lic-uso-000      thru let-lic-uso-999        .
      *              *-------------------------------------------------*
      *              * Test su esito lettura file [lcu]                *
      *              *-------------------------------------------------*
           if        w-cnt-let-lic-uso    =    "E"
                     go to det-lic-uso-100
           else if   w-cnt-let-lic-uso    =    "N"
                     go to det-lic-uso-200
           else if   w-cnt-let-lic-uso    =    "U"
                     go to det-lic-uso-300
           else      go to det-lic-uso-400.
       det-lic-uso-100.
      *              *-------------------------------------------------*
      *              * Se errore irreparabile in lettura               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   err-lic-uso-000      thru err-lic-uso-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : 'E'                      *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-det-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'lic-uso'     *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "lic-uso"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      45                   to   s-car                  .
           move      "E"                  to   w-ipc-lic-sne          .
           move      spaces               to   w-ipc-lic-rag          .
           move      zero                 to   w-ipc-lic-nut          .
           move      zero                 to   w-ipc-lic-scd          .
           move      w-ipc-lic-uso        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-lic-uso-999.
       det-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Se licenza d'uso non esistente o non corretta   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : 'N'                      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-det-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'lic-uso'     *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "lic-uso"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      45                   to   s-car                  .
           move      "N"                  to   w-ipc-lic-sne          .
           move      spaces               to   w-ipc-lic-rag          .
           move      zero                 to   w-ipc-lic-nut          .
           move      zero                 to   w-ipc-lic-scd          .
           move      w-ipc-lic-uso        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-lic-uso-999.
       det-lic-uso-300.
      *              *-------------------------------------------------*
      *              * Se licenza d'uso esistente e corretta, ma file  *
      *              * [upn] non corretto                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : 'U'                      *
      *                  *---------------------------------------------*
           move      "U"                  to   w-cnt-det-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'lic-uso'     *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "lic-uso"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      45                   to   s-car                  .
           move      "U"                  to   w-ipc-lic-sne          .
           move      w-lic-rec-rag        to   w-ipc-lic-rag          .
           move      w-lic-rec-nut        to   w-ipc-lic-nut          .
           move      w-lic-rec-scd        to   w-ipc-lic-scd          .
           move      w-ipc-lic-uso        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-lic-uso-999.
       det-lic-uso-400.
      *              *-------------------------------------------------*
      *              * Se licenza d'uso esistente e corretta, ed anche *
      *              * file [upn] esistente e corretto                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : 'S'                      *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-det-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'lic-uso'     *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "lic-uso"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      45                   to   s-car                  .
           move      "S"                  to   w-ipc-lic-sne          .
           move      w-lic-rec-rag        to   w-ipc-lic-rag          .
           move      w-lic-rec-nut        to   w-ipc-lic-nut          .
           move      w-lic-rec-scd        to   w-ipc-lic-scd          .
           move      w-ipc-lic-uso        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Comunicazione alla segreteria della ragione *
      *                  * sociale del licenziatario                   *
      *                  *---------------------------------------------*
           move      "LU"                 to   s-ope                  .
           move      w-lic-rec-rag        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-lic-uso-999.
       det-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Gestione licenza d'uso                                    *
      *    *-----------------------------------------------------------*
       ges-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Se il sistema operativo ospite non prevede al-  *
      *              * cun tipo di controllo sulla licenza d'uso       *
      *              *-------------------------------------------------*
       ges-lic-uso-010.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-ops-tlu            not  = "00"
                     go to ges-lic-uso-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "L'installazione non prevede il controllo sulla lic
      -              "enza d'uso.               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Pertanto il programma 'xpg110' non puo' essere ese
      -              "guito.                [ ] "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in on                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      spaces               to   v-ufk                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ges-lic-uso-999.
       ges-lic-uso-050.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita da programma   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-ges-lic-uso      .
      *              *-------------------------------------------------*
      *              * Lettura file [lcu]                              *
      *              *-------------------------------------------------*
           perform   let-lic-uso-000      thru let-lic-uso-999        .
      *              *-------------------------------------------------*
      *              * Test su esito lettura file [lcu]                *
      *              *-------------------------------------------------*
           if        w-cnt-let-lic-uso    =    "E"
                     go to ges-lic-uso-100
           else if   w-cnt-let-lic-uso    =    "N"
                     go to ges-lic-uso-200
           else      go to ges-lic-uso-300.
       ges-lic-uso-100.
      *              *-------------------------------------------------*
      *              * Se errore irreparabile in lettura               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   err-lic-uso-000      thru err-lic-uso-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : errore                   *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-ges-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ges-lic-uso-999.
       ges-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Se licenza d'uso da inserire                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inserimento licenza d'uso                   *
      *                  *---------------------------------------------*
           perform   ins-lic-uso-000      thru ins-lic-uso-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione e preparazione     *
      *                  * flag di uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-ins-lic-uso    not  = spaces
                     move  "E"            to   w-cnt-ges-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ges-lic-uso-999.
       ges-lic-uso-300.
      *              *-------------------------------------------------*
      *              * Se licenza d'uso da modificare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Modifica licenza d'uso                      *
      *                  *---------------------------------------------*
           perform   mod-lic-uso-000      thru mod-lic-uso-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita sempre a OK                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-ges-lic-uso      .
       ges-lic-uso-999.
           exit.

      *================================================================*
      *       Subroutines                                              *
      *================================================================*

      *================================================================*
      *       Visualizzazione della descrizione della funzione in atto *
      *----------------------------------------------------------------*
       vis-des-fun-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      64                   to   v-pos                  .
           if        w-cnt-tip-fun        =    "I"
                     move  "    (Inserimento)"
                                          to   v-alf
           else if   w-cnt-tip-fun        =    "M"
                     move  "       (Modifica)"
                                          to   v-alf
           else if   w-cnt-tip-fun        =    "V"
                     move  "(Visualizzazione)"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fun-999.
           exit.

      *    *===========================================================*
      *    * Lettura file licenza d'uso                                *
      *    *-----------------------------------------------------------*
       let-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita dalla routine  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-lic-uso      .
       let-lic-uso-100.
      *              *-------------------------------------------------*
      *              * Se il sistema operativo ospite non prevede al-  *
      *              * cun tipo di controllo sulla licenza d'uso       *
      *              *-------------------------------------------------*
       let-lic-uso-110.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-ops-tlu            not  = "00"
                     go to let-lic-uso-150.
      *                  *---------------------------------------------*
      *                  * Password di controllo a spaces              *
      *                  *---------------------------------------------*
           move      spaces               to   w-lic-rec-pwd          .
      *                  *---------------------------------------------*
      *                  * Max numero utenti a zero                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-lic-rec-nut          .
      *                  *---------------------------------------------*
      *                  * Ragione sociale a spaces                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-lic-rec-rag          .
      *                  *---------------------------------------------*
      *                  * Scadenza licenza a zero                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-lic-rec-scd          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-lic-uso-999.
       let-lic-uso-150.
      *              *-------------------------------------------------*
      *              * Open [lcu] in input                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione name per [lcu]                 *
      *                  *---------------------------------------------*
           move      "lcu "               to   g-nam                  .
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [lcu]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "lcu"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   g-pat                  .
      *                  *---------------------------------------------*
      *                  * Open input                                  *
      *                  *---------------------------------------------*
           move      "OI"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
           if        g-sts                =    e-not-err
                     go to let-lic-uso-200.
      *                      *-----------------------------------------*
      *                      * Se errori su open                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione descrizione errore e   *
      *                          * flag di uscita                      *
      *                          *-------------------------------------*
           if        g-sts                =    e-use-err
                     move  "File di licenza d'uso bloccato !        "
                                          to   w-lic-des-err
                     move  "E"            to   w-cnt-let-lic-uso
           else if   g-sts                =    e-fil-inc
                     move  "E"            to   w-cnt-let-lic-uso
                     move  "Formato file licenza d'uso errato !     "
                                          to   w-lic-des-err
           else if   g-sts                =    e-opn-err
                     move  "N"            to   w-cnt-let-lic-uso
                     move  "Manca il file di licenza d'uso !        "
                                          to   w-lic-des-err
           else      move  "E"            to   w-cnt-let-lic-uso
                     move  "Errore fatale su file licenza d'uso !   "
                                          to   w-lic-des-err          .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [lcu]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     let-lic-uso-999.
       let-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Get next                                    *
      *                  *---------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test su eventuali errori                    *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to let-lic-uso-300.
      *                  *---------------------------------------------*
      *                  * Se errori su lettura                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione errore e flag  *
      *                      * di uscita                               *
      *                      *-----------------------------------------*
           if        g-sts                =    e-end-fil
                     move  "N"            to   w-cnt-let-lic-uso
                     move  "File di licenza d'uso vuoto !           "
                                          to   w-lic-des-err
           else      move  "E"            to   w-cnt-let-lic-uso
                     move  "Errore fatale su file licenza d'uso !   "
                                          to   w-lic-des-err          .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [lcu]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-lic-uso-999.
       let-lic-uso-300.
      *              *-------------------------------------------------*
      *              * Spostamento record letto in area di comodo      *
      *              *-------------------------------------------------*
           move      g-rec                to   w-lic-rec              .
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                            *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *              *-------------------------------------------------*
      *              * Test se formato record corretto                 *
      *              *-------------------------------------------------*
           if        w-lic-rec-nut        not  numeric
                     go to let-lic-uso-400
           else if   w-lic-rec-pwd        =    spaces
                     go to let-lic-uso-400
           else      go to let-lic-uso-500.
       let-lic-uso-400.
      *                  *---------------------------------------------*
      *                  * Se formato record non corretto              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione errore e flag  *
      *                      * di uscita                               *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-let-lic-uso      .
           move      "Contenuto file licenza d'uso errato !   "
                                          to   w-lic-des-err          .
           go to     let-lic-uso-999.
       let-lic-uso-500.
      *                  *---------------------------------------------*
      *                  * Se formato record corretto                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Eventuale normalizzazione scadenza      *
      *                      *-----------------------------------------*
           if        w-lic-rec-scx        not  numeric
                     move  all zero       to   w-lic-rec-scx          .
      *                      *-----------------------------------------*
      *                      * Crittografazione max numero utenti con- *
      *                      * catenato con ragione sociale e Host-Id  *
      *                      *-----------------------------------------*
           move      spaces               to   w-cry-are              .
           move      w-lic-rec-nux        to   w-cry-are (01: 04)     .
           move      w-lic-rec-rag        to   w-cry-are (05: 40)     .
      *
           if        w-lic-rec-scx        not  = zero
                     move  w-lic-rec-scx (01: 04)
                                          to   w-cry-are (41: 04)     .
      *
           move      w-lic-hoi            to   w-cry-are (45: 20)     .
           perform   cry-enc-dat-000      thru cry-enc-dat-999        .
      *                      *-----------------------------------------*
      *                      * Controllo che i caratteri 45..52 cor-   *
      *                      * rispondano alla password                *
      *                      *-----------------------------------------*
           move      w-cry-are (45: 08)   to   w-pwd-045-052          .
      *                      *-----------------------------------------*
      *                      * Controllo de-crittografazione           *
      *                      *-----------------------------------------*
           if        w-pwd-045-052        =    w-lic-rec-pwd
                     go to let-lic-uso-600.
      *                          *-------------------------------------*
      *                          * Valori di controllo errati          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione descrizione errore *
      *                              * e flag di uscita                *
      *                              *---------------------------------*
           move      "N"                  to   w-cnt-let-lic-uso      .
           move      "Contenuto file licenza d'uso errato !   "
                                          to   w-lic-des-err          .
      *                              *---------------------------------*
      *                              * Uscita dalla routine            *
      *                              *---------------------------------*
           go to     let-lic-uso-999.
       let-lic-uso-600.
      *                          *-------------------------------------*
      *                          * Valori di controllo esatti          *
      *                          *-------------------------------------*
       let-lic-uso-650.
      *                              *---------------------------------*
      *                              * Test che esita il file [upn] e  *
      *                              * che contenga il record di con-  *
      *                              * trollo per il numero progressi- *
      *                              * vo utenti in uso, con deviazio- *
      *                              * ne a seconda dell'esito del     *
      *                              * controllo                       *
      *                              *---------------------------------*
       let-lic-uso-675.
      *                                  *-----------------------------*
      *                                  * Se il sistema operativo o-  *
      *                                  * spite non prevede il con-   *
      *                                  * trollo sul max numero di u- *
      *                                  * tenti : uscita              *
      *                                  *-----------------------------*
           if        w-ops-tlu            not  = "02"
                     go to let-lic-uso-999.
       let-lic-uso-750.
      *                                  *-----------------------------*
      *                                  * Preparazione pathname per   *
      *                                  * il file [upn]               *
      *                                  *-----------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "upn"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-upn-pat              .
      *                                  *-----------------------------*
      *                                  * Open [upn]                  *
      *                                  *-----------------------------*
           open      i-o    upn                                       .
      *                                  *-----------------------------*
      *                                  * Lettura record da [upn]     *
      *                                  *-----------------------------*
           move      "RDCG"               to   upn-tre                .
           move      "            "       to   upn-kre                .
           read      upn    with no lock
                            invalid key
                            go to   let-lic-uso-775.
           go to     let-lic-uso-800.
       let-lic-uso-775.
      *                              *---------------------------------*
      *                              * Se controllo non superato       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Unlock [upn]                *
      *                                  *-----------------------------*
           unlock    upn    records                                   .
      *                                  *-----------------------------*
      *                                  * Close [upn]                 *
      *                                  *-----------------------------*
           close     upn                                              .
      *                                  *-----------------------------*
      *                                  * Status di uscita a "U"      *
      *                                  *-----------------------------*
           move      "U"                  to   w-cnt-let-lic-uso      .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     let-lic-uso-999.
       let-lic-uso-800.
      *                              *---------------------------------*
      *                              * Se controllo superato           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Unlock [upn]                *
      *                                  *-----------------------------*
           unlock    upn    records                                   .
      *                                  *-----------------------------*
      *                                  * Close [upn]                 *
      *                                  *-----------------------------*
           close     upn                                              .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     let-lic-uso-999.
       let-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Messaggio di errore su licenza d'uso                      *
      *    *-----------------------------------------------------------*
       err-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-lic-des-err        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Dicitura di presa visione                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in on                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione OK di presa visione                *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       err-lic-uso-400.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to err-lic-uso-800.
           if        v-alf                not  = "OK"
                     go to err-lic-uso-400.
       err-lic-uso-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       err-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Inserimento licenza d'uso                                 *
      *    *-----------------------------------------------------------*
       ins-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita dalla routine  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-ins-lic-uso      .
      *              *-------------------------------------------------*
      *              * Descrizione della funzione in atto              *
      *              *-------------------------------------------------*
           move      "I"                  to   w-cnt-tip-fun          .
           perform   vis-des-fun-000      thru vis-des-fun-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di impostazione       *
      *              *-------------------------------------------------*
           move      spaces               to   w-lic-acc-rag          .
           move      zero                 to   w-lic-acc-nut          .
           move      zero                 to   w-lic-acc-scd          .
           move      w-lic-hoi            to   w-lic-acc-hoi          .
           move      spaces               to   w-lic-acc-pwd          .
      *              *-------------------------------------------------*
      *              * Accettazione licenza d'uso                      *
      *              *-------------------------------------------------*
           perform   acc-lic-uso-000      thru acc-lic-uso-999        .
      *              *-------------------------------------------------*
      *              * Test su esito accettazione licenza d'uso        *
      *              *-------------------------------------------------*
           if        w-cnt-acc-lic-uso    not  = spaces
                     move  "#"            to   w-cnt-ins-lic-uso
                     go to ins-lic-uso-999
           else      move  spaces         to   w-cnt-ins-lic-uso      .
      *              *-------------------------------------------------*
      *              * Scrittura file licenza d'uso                    *
      *              *-------------------------------------------------*
           perform   scr-lic-uso-000      thru scr-lic-uso-999        .
       ins-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Modifica licenza d'uso                                    *
      *    *-----------------------------------------------------------*
       mod-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita dalla routine  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mod-lic-uso      .
      *              *-------------------------------------------------*
      *              * Descrizione della funzione in atto              *
      *              *-------------------------------------------------*
           move      "M"                  to   w-cnt-tip-fun          .
           perform   vis-des-fun-000      thru vis-des-fun-999        .
      *              *-------------------------------------------------*
      *              * Preparazione parametri di impostazione          *
      *              *-------------------------------------------------*
           move      w-lic-rec-rag        to   w-lic-acc-rag          .
           move      w-lic-rec-nut        to   w-lic-acc-nut          .
           move      w-lic-rec-scd        to   w-lic-acc-scd          .
           move      w-lic-hoi            to   w-lic-acc-hoi          .
           move      w-lic-rec-pwd        to   w-lic-acc-pwd          .
      *              *-------------------------------------------------*
      *              * Accettazione licenza d'uso                      *
      *              *-------------------------------------------------*
           perform   acc-lic-uso-000      thru acc-lic-uso-999        .
      *              *-------------------------------------------------*
      *              * Test su esito accettazione licenza d'uso        *
      *              *-------------------------------------------------*
           if        w-cnt-acc-lic-uso    not  = spaces
                     move  "#"            to   w-cnt-ins-lic-uso
                     go to mod-lic-uso-999
           else      move  spaces         to   w-cnt-ins-lic-uso      .
      *              *-------------------------------------------------*
      *              * Scrittura file licenza d'uso                    *
      *              *-------------------------------------------------*
           perform   scr-lic-uso-000      thru scr-lic-uso-999        .
       mod-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Accettazione licenza d'uso                                *
      *    *-----------------------------------------------------------*
       acc-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita dalla routine  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-lic-uso      .
       acc-lic-uso-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione maschera                        *
      *              *-------------------------------------------------*
       acc-lic-uso-105.
      *                  *---------------------------------------------*
      *                  * Ragione sociale licenziatario               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale licenziatario :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-110.
      *                  *---------------------------------------------*
      *                  * Max numero utenti, solo se il sistema ope-  *
      *                  * rativo ospite lo prevede                    *
      *                  *---------------------------------------------*
           if        w-ops-tlu            not  = "02"
                     go to acc-lic-uso-113.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Massimo numero di utenti      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-113.
      *                  *---------------------------------------------*
      *                  * Data scadenza licenza                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Scadenza (MMAA)               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-115.
      *                  *---------------------------------------------*
      *                  * Host-Id                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Identificatore unico          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-120.
      *                  *---------------------------------------------*
      *                  * Password di controllo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Password di controllo         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-150.
      *              *-------------------------------------------------*
      *              * Visualizzazione preliminare dati                *
      *              *-------------------------------------------------*
       acc-lic-uso-155.
      *                  *---------------------------------------------*
      *                  * Ragione sociale licenziatario               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-rag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-160.
      *                  *---------------------------------------------*
      *                  * Max numero utenti, solo se il sistema ope-  *
      *                  * rativo ospite lo prevede                    *
      *                  *---------------------------------------------*
           if        w-ops-tlu            not  = "02"
                     go to acc-lic-uso-165.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-nut        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-165.
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      10                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-scd        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-170.
      *                  *---------------------------------------------*
      *                  * Host-Id                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-hoi        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-175.
      *                  *---------------------------------------------*
      *                  * Password di controllo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "W"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-pwd        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Accettazione vera e propria                     *
      *              *-------------------------------------------------*
       acc-lic-uso-250.
      *                  *---------------------------------------------*
      *                  * Ragione sociale licenziatario               *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-rag        to   v-alf                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-lic-acc-rag          .
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
           if        v-key                =    "DO  "
                     go to acc-lic-uso-700.
       acc-lic-uso-300.
      *                  *---------------------------------------------*
      *                  * Max numero utenti, solo se il sistema ope-  *
      *                  * rativo ospite lo prevede                    *
      *                  *---------------------------------------------*
           if        w-ops-tlu            =   "02"
                     go to acc-lic-uso-310.
           if        v-key                =    "UP  "
                     go to acc-lic-uso-250
           else      go to acc-lic-uso-400.
       acc-lic-uso-310.
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-nut        to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lic-acc-nut          .
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
           if        v-key                =    "DO  "
                     go to acc-lic-uso-700.
           if        v-key                =    "UP  "
                     go to acc-lic-uso-250.
       acc-lic-uso-330.
      *                  *---------------------------------------------*
      *                  * Data di scadenza licenza                    *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      10                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-scd        to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lic-acc-scd          .
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
           if        v-key                =    "DO  "
                     go to acc-lic-uso-700.
           if        v-key                =    "UP  "
                     go to acc-lic-uso-300.
           go to     acc-lic-uso-400.
       acc-lic-uso-350.
      *                  *---------------------------------------------*
      *                  * Accettazione Host-Id                        *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-hoi        to   v-alf                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-lic-acc-hoi          .
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
      *                  *---------------------------------------------*
      *                  * Esecuzione crittografazione                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cry-are              .
           move      w-lic-acc-nux        to   w-cry-are (01: 04)     .
           move      w-lic-acc-rag        to   w-cry-are (05: 40)     .
      *
           if        w-lic-acc-scx        not  = zero
                     move  w-lic-acc-scx (01: 04)
                                          to   w-cry-are (41: 04)     .
      *
           move      w-lic-acc-hoi        to   w-cry-are (45: 20)     .
           perform   cry-enc-dat-000      thru cry-enc-dat-999        .
      *                  *---------------------------------------------*
      *                  * Password risultante in valore di accetta-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-cry-are (45: 08)   to   w-lic-acc-pwd          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione password risultante         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-pwd        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-lic-uso-400.
      *                  *---------------------------------------------*
      *                  * Password di controllo                       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-pwd        to   v-alf                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-lic-acc-pwd          .
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
           if        v-key                =    "DO  "
                     go to acc-lic-uso-700.
           if        v-key                =    "UP"
                     go to acc-lic-uso-300.
      *                  *---------------------------------------------*
      *                  * Se impostata richiesta di impostazione del- *
      *                  * la parola passepartout                      *
      *                  *---------------------------------------------*
           if        w-lic-acc-pwd        =    "."
                     go to acc-lic-uso-500
           else      go to acc-lic-uso-700.
       acc-lic-uso-500.
      *                  *---------------------------------------------*
      *                  * Impostazione della parola passepartout      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Display area a Spaces                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-lic-acc-pwd          .
      *                      *-----------------------------------------*
      *                      * Display area a Spaces                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Test su tasto funzione                  *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
           if        v-key                =    "DO  "
                     go to acc-lic-uso-700.
           if        v-key                =    "UP"
                     go to acc-lic-uso-300.
      *                      *-----------------------------------------*
      *                      * Test se parola passepartout             *
      *                      *-----------------------------------------*
           if        w-lic-acc-pwd        =    w-lic-ppt
                     move  spaces         to   w-lic-acc-pwd
                     go to acc-lic-uso-350.
       acc-lic-uso-700.
      *              *-------------------------------------------------*
      *              * Uscita per tasto "DO  "                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Password di controllo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "W"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-lic-acc-pwd        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Se manca un valore si torna a reimpostarlo  *
      *                  *                                             *
      *                  * Nota : E' ammesso il valore zero per il nu- *
      *                  *        mero max di utenti.                  *
      *                  *---------------------------------------------*
           if        w-lic-acc-rag        =    spaces
                     go to acc-lic-uso-200
           else if   w-lic-acc-pwd        =    spaces
                     go to acc-lic-uso-200.
      *                  *---------------------------------------------*
      *                  * Esecuzione crittografazione                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cry-are              .
           move      w-lic-acc-nux        to   w-cry-are (01: 04)     .
           move      w-lic-acc-rag        to   w-cry-are (05: 40)     .
      *
           if        w-lic-acc-scx        not  = zero
                     move  w-lic-acc-scx (01: 04)
                                          to   w-cry-are (41: 04)     .
      *
           move      w-lic-acc-hoi        to   w-cry-are (45: 20)     .
           perform   cry-enc-dat-000      thru cry-enc-dat-999        .
      *                  *---------------------------------------------*
      *                  * Password risultante in comodo               *
      *                  *---------------------------------------------*
           move      w-cry-are (45: 08)   to   w-pwd-045-052          .
      *                  *---------------------------------------------*
      *                  * Confronto tra password di controllo ritor-  *
      *                  * nata dalla crittografazione e password di   *
      *                  * controllo impostata dall'utente             *
      *                  *---------------------------------------------*
           if        w-pwd-045-052        =    w-lic-acc-pwd
                     go to acc-lic-uso-750.
      *                      *-----------------------------------------*
      *                      * Se valori di controllo errati           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione valore impostato    *
      *                          *-------------------------------------*
           move      spaces               to   w-lic-acc-pwd          .
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Password di controllo errata ! "
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Attesa di 5 secondi come deterrente *
      *                          * alla reiterazione                   *
      *                          *-------------------------------------*
           move      05                   to   w-lic-ctr-max          .
           move      zero                 to   w-lic-ctr-det          .
       acc-lic-uso-725.
           add       1                    to   w-lic-ctr-det          .
           if        w-lic-ctr-det        >    w-lic-ctr-max
                     go to acc-lic-uso-200.
      *                          *-------------------------------------*
      *                          * Attesa di 1 secondo                 *
      *                          *-------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     acc-lic-uso-725.
       acc-lic-uso-750.
      *                      *-----------------------------------------*
      *                      * Se valori di controllo esatti           *
      *                      *-----------------------------------------*
       acc-lic-uso-800.
      *                          *-------------------------------------*
      *                          * Richiesta di conferma               *
      *                          *-------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                             *----------------------------------*
      *                             * Se tasto "DO  "                  *
      *                             *----------------------------------*
           if        v-key                =    "DO  "
                     go to acc-lic-uso-999.
      *                             *----------------------------------*
      *                             * Se tasto "EXIT"                  *
      *                             *----------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-lic-uso-900.
      *                             *----------------------------------*
      *                             * Se tasto "UP  "                  *
      *                             *----------------------------------*
           if        v-key                =    "UP  "
                     go to acc-lic-uso-200.
      *                             *----------------------------------*
      *                             * Se risposta "S"                  *
      *                             *----------------------------------*
           if        v-alf                =    "S"
                     go to acc-lic-uso-999.
      *                             *----------------------------------*
      *                             * Se risposta "E"                  *
      *                             *----------------------------------*
           if        v-alf                =    "E"
                     go to acc-lic-uso-900.
      *                             *----------------------------------*
      *                             * Se risposta "N"                  *
      *                             *----------------------------------*
           go to     acc-lic-uso-200.
       acc-lic-uso-900.
      *              *-------------------------------------------------*
      *              * Uscita per tasto "EXIT"                         *
      *              *-------------------------------------------------*
           move      "E"                  to   w-cnt-acc-lic-uso      .
       acc-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Scrittura file licenza d'uso                              *
      *    *-----------------------------------------------------------*
       scr-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita dalla routine  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-scr-lic-uso      .
      *              *-------------------------------------------------*
      *              * Open [lcu] in output                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione name per [lcu]                 *
      *                  *---------------------------------------------*
           move      "lcu "               to   g-nam                  .
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [lcu]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "lcu"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   g-pat                  .
      *                  *---------------------------------------------*
      *                  * Open output                                 *
      *                  *---------------------------------------------*
           move      "OO"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to scr-lic-uso-200.
      *                  *---------------------------------------------*
      *                  * Se errori su open                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione                *
      *                      *-----------------------------------------*
           if        g-sts                =    e-use-err
                     move  "File di licenza d'uso bloccato !        "
                                          to   w-lic-des-err
                     move  "E"            to   w-cnt-scr-lic-uso
           else if   g-sts                =    e-fil-inc
                     move  "E"            to   w-cnt-scr-lic-uso
                     move  "Formato file licenza d'uso errato !     "
                                          to   w-lic-des-err
           else if   g-sts                =    e-opn-err
                     move  "N"            to   w-cnt-scr-lic-uso
                     move  "Manca il file di licenza d'uso !        "
                                          to   w-lic-des-err
           else      move  "E"            to   w-cnt-scr-lic-uso
                     move  "Errore fatale su file licenza d'uso !   "
                                          to   w-lic-des-err          .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [lcu]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita con messaggio di errore          *
      *                      *-----------------------------------------*
           go to     scr-lic-uso-900.
       scr-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Preparazione record                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-lic-rec              .
           move      w-lic-acc-nut        to   w-lic-rec-nut          .
           move      w-lic-acc-pwd        to   w-lic-rec-pwd          .
           move      w-lic-acc-rag        to   w-lic-rec-rag          .
           move      w-lic-acc-scd        to   w-lic-rec-scd          .
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record in area file            *
      *                  *---------------------------------------------*
           move      w-lic-rec            to   g-rec                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione di i-o             *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to scr-lic-uso-300.
      *                  *---------------------------------------------*
      *                  * Se errori su scrittura                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione errore e flag  *
      *                      * di uscita                               *
      *                      *-----------------------------------------*
           move      "Errore fatale su file licenza d'uso !   "
                                          to   w-lic-des-err          .
           move      "E"                  to   w-cnt-scr-lic-uso      .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [lcu]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita con messaggio di errore          *
      *                      *-----------------------------------------*
           go to     scr-lic-uso-900.
       scr-lic-uso-300.
      *              *-------------------------------------------------*
      *              * Chiusura [lcu]                                  *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                            *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to scr-lic-uso-999.
      *              *-------------------------------------------------*
      *              * Se errori su chiusura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione errore e flag di   *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           move      "Errore fatale su file licenza d'uso !   "
                                          to   w-lic-des-err          .
           move      "E"                  to   w-cnt-scr-lic-uso      .
      *                  *---------------------------------------------*
      *                  * Uscita con messaggio di errore              *
      *                  *---------------------------------------------*
           go to     scr-lic-uso-900.
       scr-lic-uso-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore                   *
      *              *-------------------------------------------------*
           perform   err-lic-uso-000      thru err-lic-uso-999        .
       scr-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Crittografazione                                          *
      *    *                                                           *
      *    * Input  : w-cry-are = area da crittografare                *
      *    *                                                           *
      *    * Output : w-cry-are = area crittografata                   *
      *    *          w-cry-dpl = valore numerico di controllo         *
      *    *          w-cry-sum = valore numerico di controllo         *
      *    *-----------------------------------------------------------*
       cry-enc-dat-000.
           move      zero                 to   w-cry-dpl
                                               w-cry-sum
                                               w-cry-i01              .
       cry-enc-dat-100.
           add       1                    to   w-cry-i01              .
           if        w-cry-i01            >    64
                     go to cry-enc-dat-400.
       cry-enc-dat-200.
           move      zero                 to   w-cry-i02              .
           inspect   w-cry-tbl        tallying w-cry-i02
                     for   characters   before
                                       initial w-cry-arx
                                              (w-cry-i01)             .
           if        w-cry-i02            =    95
                     move  spaces         to   w-cry-arx
                                              (w-cry-i01)
                     go to cry-enc-dat-200.
           add       1                    to   w-cry-i02              .
           add       w-cry-i02            to   w-cry-dpl              .
       cry-enc-dat-300.
           if        w-cry-dpl            >    95
                     subtract 95          from w-cry-dpl
                     go to cry-enc-dat-300.
           add       w-cry-dpl            to   w-cry-sum              .
           subtract  w-cry-dpl            from 96
                                        giving w-cry-i03              .
           move      w-cry-tby
                    (w-cry-i03)           to   w-cry-arx
                                              (w-cry-i01)             .
           go to     cry-enc-dat-100.
       cry-enc-dat-400.
           go to     cry-enc-dat-999.
       cry-enc-dat-999.
           exit.

      *    *===========================================================*
      *    * Decrittografazione                                        *
      *    *                                                           *
      *    * Input  : w-cry-are = area da de-crittografare             *
      *    *                                                           *
      *    * Output : w-cry-are = area de-crittografata                *
      *    *          w-cry-dpl = valore numerico di controllo         *
      *    *          w-cry-sum = valore numerico di controllo         *
      *    *                                                           *
      *    *          Nota : i due valori numerici di controllo hanno  *
      *    *                 lo stesso valore che avevano assunto in   *
      *    *                 fase di crittografazione                  *
      *    *-----------------------------------------------------------*
       cry-dec-dat-000.
           move      zero                 to   w-cry-dpl
                                               w-cry-sum
                                               w-cry-i01              .
       cry-dec-dat-100.
           add       1                    to   w-cry-i01              .
           if        w-cry-i01            >    64
                     go to cry-dec-dat-999.
       cry-dec-dat-200.
           move      zero                 to   w-cry-i02              .
           inspect   w-cry-tbl        tallying w-cry-i02
                     for   characters   before
                                       initial w-cry-arx
                                              (w-cry-i01)             .
           if        w-cry-i02            =    95
                     move  spaces         to   w-cry-arx
                                              (w-cry-i01)
                     go to cry-dec-dat-200.
           subtract  w-cry-i02            from 95
                                        giving w-cry-i03              .
           if        w-cry-i03            not  > w-cry-dpl
                     add   95             to   w-cry-i03              .
           subtract  w-cry-dpl            from w-cry-i03              .
           move      w-cry-tbx
                    (w-cry-i03)           to   w-cry-arx
                                              (w-cry-i01)             .
           add       w-cry-i03            to   w-cry-dpl              .
           if        w-cry-dpl            >    95
                     subtract 95          from w-cry-dpl              .
           add       w-cry-dpl            to   w-cry-sum              .
           go to     cry-dec-dat-100.
       cry-dec-dat-999.
           exit.
