       Identification Division.
       Program-Id.                                 pxpg0001           .
      *================================================================*
      *                                                                *
      * Inizializzazione per esecuzione in foreground                  *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [pid]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pid   assign to disk         f-pid-pat
                             organization is relative
                             access mode  is dynamic
                             relative key is f-pid-krn
                             file status  is f-pid-sts                .

      *    *===========================================================*
      *    * File Control [auc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  auc   assign to disk         f-auc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is auc-key
                             file status  is f-auc-sts                .

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
      *    File Description [pid]                                      *
      *----------------------------------------------------------------*
       fd  pid       label record standard                            .
       01  pid-rec.
           05  pid-uni-pid                pic  9(05)                  .

      *================================================================*
      *    File Description [auc]                                      *
      *----------------------------------------------------------------*
       fd  auc       label record standard                            .
       01  auc-rec.
           05  auc-key.
               10  auc-tre                pic  x(04)                  .
               10  auc-kre                pic  x(40)                  .
           05  auc-dat.
               10  auc-chr.
                   15  filler      occurs 1536
                                          pic  x(01)                  .

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
      *    * Area ausiliaria per controllo i-o su [pid]                *
      *    *-----------------------------------------------------------*
       01  f-pid.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-pid-nam                  pic  x(04) value "pid "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-pid-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-pid-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria addizionale per controllo i-o su [pid]    *
      *    *-----------------------------------------------------------*
       01  f-aux-pid.
      *        *-------------------------------------------------------*
      *        * Relative key                                          *
      *        *-------------------------------------------------------*
           05  f-pid-krn                  pic  9(05)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [auc]                *
      *    *-----------------------------------------------------------*
       01  f-auc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-auc-nam                  pic  x(04) value "auc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-auc-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-auc-sts                  pic  x(02)                  .

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
      *    * Area di comunicazione per modulo                "miosts"  *
      *    *-----------------------------------------------------------*
       01  c.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  c-nam                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  c-pat                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  c-sts                      pic  x(02)                  .

      *    *===========================================================*
      *    * Area di definizione del pacchetto applicativo e delle ca- *
      *    * ratteristiche della release                               *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/a"                                  .
       
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
      *        *-------------------------------------------------------*
      *        * Work per numero secondi di attesa                     *
      *        *-------------------------------------------------------*
           05  w-ops-nds                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per accettazione Time dal sistema                *
      *        *-------------------------------------------------------*
           05  w-ops-tim                  pic  9(08)                  .
           05  w-ops-tim-r01 redefines
               w-ops-tim.
               10  w-ops-hms              pic  9(06)                  .
               10  filler                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per hh:mm:ss iniziale                            *
      *        *-------------------------------------------------------*
           05  w-ops-ini                  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Work per hh:mm:ss attuale                             *
      *        *-------------------------------------------------------*
           05  w-ops-att                  pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per controllo prefisso per pathname unici per   *
      *    * files temporanei                                          *
      *    *-----------------------------------------------------------*
       01  w-pid.
      *        *-------------------------------------------------------*
      *        * Numero progressivo unico                              *
      *        *-------------------------------------------------------*
           05  w-pid-uni-pid              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per manipolazione di 'x-env-pfix'              *
      *        *-------------------------------------------------------*
           05  w-pid-xen-pfi.
      *            *---------------------------------------------------*
      *            * Valore completo pre-variazione                    *
      *            *---------------------------------------------------*
               10  w-pid-xen-pfi-pre      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Contatore caratteri '.' presenti nel valore com-  *
      *            * pleto pre-variazione                              *
      *            *---------------------------------------------------*
               10  w-pid-xen-pfi-ctr      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Valore completo post-variazione                   *
      *            *---------------------------------------------------*
               10  w-pid-xen-pfi-pos      pic  x(20)                  .

      *    *===========================================================*
      *    * Work area per flags di esistenza records appartenenti ai  *
      *    * file [auc]                                                *
      *    *-----------------------------------------------------------*
       01  w-fes.
           05  w-fes-psg                  pic  x(01)                  .
           05  w-fes-azi                  pic  x(01)                  .
           05  w-fes-ute                  pic  x(01)                  .
           05  w-fes-cmd                  pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per dichiarazione codice azienda                *
      *    *-----------------------------------------------------------*
       01  w-dic-azi.
      *        *-------------------------------------------------------*
      *        * Per Det descrizione azienda centrata                  *
      *        *-------------------------------------------------------*
           05  w-dic-azi-cen.
      *            *---------------------------------------------------*
      *            * Descrizione azienda allineata a sinistra          *
      *            *---------------------------------------------------*
               10  w-dic-azi-cen-asx.
                   15  w-dic-azi-cen-asc occurs 40
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione azienda allineata al centro           *
      *            *---------------------------------------------------*
               10  w-dic-azi-cen-adx.
                   15  w-dic-azi-cen-adc occurs 40
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locali                                       *
      *            *---------------------------------------------------*
               10  w-dic-azi-cen-wrk.
                   15  w-dic-azi-cen-w01  pic  9(02)                  .
                   15  w-dic-azi-cen-w02  pic  9(02)                  .
                   15  w-dic-azi-cen-w03  pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per controlli di abilitazione                   *
      *    *-----------------------------------------------------------*
       01  w-cnt.
           05  w-cnt-azi                  pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per accettazione codici e passwords             *
      *    *-----------------------------------------------------------*
       01  w-acp.
           05  w-acp-psw                  pic  x(08)                  .
           05  w-acp-acc                  pic  x(08)                  .
           05  w-acp-key                  pic  x(04)                  .

      *    *===========================================================*
      *    * Work per variabili di i.p.c.                              *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabile di i.p.c. 'lic-uso'                         *
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
      *            * Ragione sociale intestatario decrittografata      *
      *            *---------------------------------------------------*
               10  w-ipc-lic-rag          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Max numero utenti                                 *
      *            *---------------------------------------------------*
               10  w-ipc-lic-nut          pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per esito lettura licenza uso             *
      *        *-------------------------------------------------------*
           05  w-sav-lic-sne              pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per il messaggio di licenza d'uso               *
      *    *-----------------------------------------------------------*
       01  w-mlu.
      *        *-------------------------------------------------------*
      *        * Literals e aree da visualizzare                       *
      *        *-------------------------------------------------------*
           05  w-mlu-lit.
      *            *---------------------------------------------------*
      *            * Licenza d'uso                                     *
      *            *---------------------------------------------------*
               10  w-mlu-lit-004          pic  x(80) value
                     ".................................   +-------------
      -              "-----------------------------+"                 .
               10  w-mlu-lit-005          pic  x(80) value
                     ". .                           . .   |  Software co
      -              "ncesso in licenza d'uso da   |"                 .
               10  w-mlu-lit-006          pic  x(80) value
                     ".   .                       .   .   |             
      -              "                             |"                 .
               10  w-mlu-lit-007          pic  x(80) value
                     ".     .                   .     .   |            N
      -              "icola de Kunovich            |"                 .
               10  w-mlu-lit-008          pic  x(80) value
                     ".       .               .       .   |             
      -              "                             |"                 .
               10  w-mlu-lit-009          pic  x(80) value
                     ".         .           . .       .   |             
      -              "        a                    |"                 .
               10  w-mlu-lit-010          pic  x(80) value
                     ".           .       .   .       .   |             
      -              "                             |"                 .
               10  w-mlu-lit-011          pic  x(80) value
                     ".             .   .     .       .   |             
      -              "                             |"                 .
               10  w-mlu-lit-012          pic  x(80) value
                     ".               .................   |             
      -              "                             |"                 .
               10  w-mlu-lit-013          pic  x(80) value
                     ".             . ...............     |     -- Tutti
      -              " i diritti riservati --      |"                 .
               10  w-mlu-lit-014          pic  x(80) value
                     ".           .   .............       |             
      -              "                             |"                 .
               10  w-mlu-lit-015          pic  x(80) value
                     ".         .     ...........         +-------------
      -              "-----------------------------+"                 .
               10  w-mlu-lit-016          pic  x(80) value
                     ".       .       .........                         
      -              "                              "                 .
               10  w-mlu-lit-017          pic  x(80) value
                     ".     .   .     .......             +-------------
      -              "-----------------------------+"                 .
               10  w-mlu-lit-018          pic  x(80) value
                     ".   .       .   .....               |           Bu
      -              "on lavoro                    |"                 .
               10  w-mlu-lit-019          pic  x(80) value
                     ". .           . ...                 |             
      -              "                             |"                 .
               10  w-mlu-lit-020          pic  x(80) value
                     ".................                   +-------------
      -              "-----------------------------+"                 .
               10  w-mlu-lit-021          pic  x(80) value spaces     .
      *        *-------------------------------------------------------*
      *        * Comodi per editing                                    *
      *        *-------------------------------------------------------*
           05  w-mlu-edt.
               10  w-mlu-edt-mes          pic  x(09)                  .
               10  w-mlu-edt-set          pic  x(09)                  .
               10  w-mlu-edt-gio          pic  x(02)                  .
               10  w-mlu-edt-ann          pic  x(04)                  .
               10  w-mlu-edt-ora          pic  x(02)                  .
               10  w-mlu-edt-min          pic  x(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'azi'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucazi0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'ute'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucute0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'cmd'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wauccmd0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'psg'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per accettazione iniziale del codice utente     *
      *    *-----------------------------------------------------------*
       01  w-acc-cod-ute.
      *        *-------------------------------------------------------*
      *        * Contatore numero utenti letti                         *
      *        *-------------------------------------------------------*
           05  w-acc-cod-ute-num          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice utente impostato                               *
      *        *-------------------------------------------------------*
           05  w-acc-cod-ute-cod          pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Parola chiave utente impostata                        *
      *        *-------------------------------------------------------*
           05  w-acc-cod-ute-pwd          pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio function key impostata                    *
      *        *-------------------------------------------------------*
           05  w-acc-cod-ute-sfk          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-acc-cod-ute-c01          pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per programmi della serie "pxpg"    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/pxpglink.cpy"                   .

      ******************************************************************
       Procedure Division                using x                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita da programma   *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Apertura modulo                       "mopsys"  *
      *              *                                                 *
      *              * In questa subroutine viene anche rideterminato  *
      *              * il pathname unico per i files temporanei, ese-  *
      *              * guendo una variazione sulla variabile di link   *
      *              * passata come 'x-env-pfix', che corrisponde alla *
      *              * variabile di environment V_ALTER_PFIX, per la   *
      *              * parte componente relativa al prefisso unico per *
      *              * files temporanei                                *
      *              *-------------------------------------------------*
           perform   opn-mod-ops-000      thru opn-mod-ops-999        .
      *              *-------------------------------------------------*
      *              * Se status di uscita ad errore : uscita          *
      *              *-------------------------------------------------*
           if        w-ops-sts            not  = spaces
                     move  "##"           to   x-sts
                     go to main-950.
      *              *-------------------------------------------------*
      *              * Apertura modulo                       "miosts"  *
      *              *-------------------------------------------------*
           perform   opn-mod-ios-000      thru opn-mod-ios-999        .
      *              *-------------------------------------------------*
      *              * Apertura file [auc]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-auc-000      thru opn-fil-auc-999        .
      *              *-------------------------------------------------*
      *              * Apertura modulo "msegrt" e conseguente apertu-  *
      *              * ra del modulo "mvideo" con apparizione della    *
      *              * prima maschera vuota con il titolo centrale     *
      *              * del pacchetto applicativo e della release       *
      *              *-------------------------------------------------*
           perform   opn-mod-seg-000      thru opn-mod-seg-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni generali              *
      *              *-------------------------------------------------*
           perform   let-psg-auc-000      thru let-psg-auc-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione sigla Run-Time utilizzato         *
      *              *-------------------------------------------------*
           perform   dic-run-tim-000      thru dic-run-tim-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione codice terminale in uso           *
      *              *-------------------------------------------------*
           perform   dic-cod-ter-000      thru dic-cod-ter-999        .
      *              *-------------------------------------------------*
      *              * Accettazione iniziale del codice utente         *
      *              *-------------------------------------------------*
           perform   acc-cod-ute-000      thru acc-cod-ute-999        .
      *              *-------------------------------------------------*
      *              * Se codice utente non impostato : uscita         *
      *              *-------------------------------------------------*
           if        w-acc-cod-ute-cod    =    spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo utenti in uso     *
      *              *-------------------------------------------------*
           perform   inc-upn-000          thru inc-upn-999            .
      *              *-------------------------------------------------*
      *              * Dichiarazione codice utente                     *
      *              *-------------------------------------------------*
           perform   dic-cod-ute-000      thru dic-cod-ute-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione codice stampante associata al-    *
      *              * l'utente                                        *
      *              *-------------------------------------------------*
           perform   dic-cod-stp-000      thru dic-cod-stp-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione codice azienda a Spaces           *
      *              *-------------------------------------------------*
           perform   dic-azi-spc-000      thru dic-azi-spc-999        .
       main-100.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory :   *
      *              * si esegue solamente la dichiarazione codice     *
      *              * azienda e poi si va' ad uscita                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        o-sts                <    "01" or
                     o-sts                >    "99"
                     go to main-300.
      *                  *---------------------------------------------*
      *                  * Dichiarazione codice azienda                *
      *                  *---------------------------------------------*
           perform   dic-cod-azi-000      thru dic-cod-azi-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     main-700.
       main-300.
      *              *-------------------------------------------------*
      *              * Lettura licenza d'uso                           *
      *              *-------------------------------------------------*
           perform   let-lic-uso-000      thru let-lic-uso-999        .
      *              *-------------------------------------------------*
      *              * Test su lettura licenza d'uso                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun errore si continua il programma   *
      *                  *---------------------------------------------*
           if        w-ipc-lic-sne        =    "S"
                     go to main-400.
      *                  *---------------------------------------------*
      *                  * Se errore fatale : fine programma           *
      *                  *---------------------------------------------*
           if        w-ipc-lic-sne        =    "E"
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se licenza d'uso non corretta oppure file   *
      *                  * [upn] non corretto                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio valore esito lettura licen- *
      *                      * za d'uso                                *
      *                      *-----------------------------------------*
           move      w-ipc-lic-sne        to   w-sav-lic-sne          .
      *                      *-----------------------------------------*
      *                      * Creazione file [upn] normalizzato a : 0 *
      *                      *-----------------------------------------*
           perform   zer-upn-000          thru zer-upn-999            .
      *                      *-----------------------------------------*
      *                      * Richiamo monitor per gestione licenza   *
      *                      * d'uso                                   *
      *                      *-----------------------------------------*
           perform   ges-lic-uso-000      thru ges-lic-uso-999        .
      *                      *-----------------------------------------*
      *                      * Rilettura licenza d'uso                 *
      *                      *-----------------------------------------*
           perform   let-lic-uso-000      thru let-lic-uso-999        .
      *                          *-------------------------------------*
      *                          * Test su rilettura licenza d'uso     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se errore fatale o licenza d'u- *
      *                              * so non corretta : fine program- *
      *                              * ma                              *
      *                              *---------------------------------*
           if        w-ipc-lic-sne        not  = "S"
                     go to main-800.
      *                              *---------------------------------*
      *                              * Se licenza d'uso corretta       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' giunti sino a qui  *
      *                                  * semplicemente perche' c'e-  *
      *                                  * ra una anomalia nel file    *
      *                                  * [upn] : si ricicla alla     *
      *                                  * lettura licenza d'uso       *
      *                                  *-----------------------------*
           if        w-sav-lic-sne        =    "U"
                     go to main-300.
      *                                  *-----------------------------*
      *                                  * Richiamo monitor per ge-    *
      *                                  * stione configurazione del   *
      *                                  * sistema                     *
      *                                  *-----------------------------*
           perform   ges-cfg-sys-000      thru ges-cfg-sys-999        .
      *                                  *-----------------------------*
      *                                  * Uscita dal programma        *
      *                                  *-----------------------------*
           go to     main-800.
       main-400.
      *              *-------------------------------------------------*
      *              * Controllo abilitazione azienda                  *
      *              *-------------------------------------------------*
           perform   cnt-abl-azi-000      thru cnt-abl-azi-999        .
           if        w-cnt-azi            not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione licenza d'uso                   *
      *              *-------------------------------------------------*
      
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Bravo Cazzone"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      
______*    perform   vis-lic-uso-000      thru vis-lic-uso-999        .
       main-700.
      *              *-------------------------------------------------*
      *              * Status di uscita per tutto esatto               *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
           go to     main-900.
       main-800.
      *              *-------------------------------------------------*
      *              * Chiusura modulo                       "msegrt"  *
      *              *-------------------------------------------------*
           perform   cls-mod-seg-000      thru cls-mod-seg-999        .
      *              *-------------------------------------------------*
      *              * Chiusura modulo                       "mvideo"  *
      *              *-------------------------------------------------*
           perform   cls-mod-vid-000      thru cls-mod-vid-999        .
      *              *-------------------------------------------------*
      *              * Status di uscita per errore                     *
      *              *-------------------------------------------------*
           move      "##"                 to   x-sts                  .
       main-900.
      *              *-------------------------------------------------*
      *              * Chiusura file [auc]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-auc-000      thru cls-fil-auc-999        .
       main-950.
      *              *-------------------------------------------------*
      *              * Se lo status di uscita non indica gia' errore   *
      *              * si prepara eventualmente lo status di uscita a  *
      *              * : Esecuzione di desk-accessory                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su status di uscita attuale            *
      *                  *---------------------------------------------*
           if        x-sts                =    "##"
                     go to main-999.
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Set eventuale dello status                  *
      *                  *---------------------------------------------*
           if        o-sts                not  < "01" and
                     o-sts                not  > "99"
                     move  o-sts          to   x-sts                  .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Apertura modulo                                 "mopsys"  *
      *    *-----------------------------------------------------------*
       opn-mod-ops-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-ops-sts              .
      *              *-------------------------------------------------*
      *              * Funzione Open                                   *
      *              *-------------------------------------------------*
           move      "OP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Se status di uscita ad errore : a trattamento   *
      *              * errore                                          *
      *              *-------------------------------------------------*
           if        o-sts                not  = spaces
                     go to opn-mod-ops-600.
       opn-mod-ops-100.
      *              *-------------------------------------------------*
      *              * Rideterminazione del pathname unico per i files *
      *              * temporanei, eseguendo una variazione sulla va-  *
      *              * riabile di link passata come 'x-env-pfix', che  *
      *              * corrisponde alla variabile di environment pas-  *
      *              * sata come V_ALTER_PFIX, per la parte componente *
      *              * relativa al prefisso unico per files temporanei *
      *              *-------------------------------------------------*
       opn-mod-ops-110.
      *                  *---------------------------------------------*
      *                  * Se il valore della variabile e' "DOS" : non *
      *                  * si esegue alcun trattamento                 *
      *                  *---------------------------------------------*
           if        x-env-pfix           =    "DOS"
                     go to opn-mod-ops-200.
      *                  *---------------------------------------------*
      *                  * Valore completo della variabile 'x-env-pfix'*
      *                  * prima dell'intervento                       *
      *                  *---------------------------------------------*
           move      x-env-pfix           to   w-pid-xen-pfi-pre      .
      *                  *---------------------------------------------*
      *                  * Valore completo della variabile 'x-env-pfix'*
      *                  * dopo l'intervento, provvisoriamente pari al *
      *                  * valore attuale                              *
      *                  *---------------------------------------------*
           move      w-pid-xen-pfi-pre    to   w-pid-xen-pfi-pos      .
      *                  *---------------------------------------------*
      *                  * Se l'attuale valore contiene gia' piu' di   *
      *                  * un '.' di separazione non si esegue alcuna  *
      *                  * azione, in quanto significa che si e' in    *
      *                  * esecuzione di un desk-accessory, e pertan-  *
      *                  * to l'azione di variazione e' stata gia' e-  *
      *                  * seguita                                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-pid-xen-pfi-ctr      .
           inspect   w-pid-xen-pfi-pre
                                      tallying w-pid-xen-pfi-ctr
                                      for all  "."                    .
           if        w-pid-xen-pfi-ctr    >    1
                     go to opn-mod-ops-170.
       opn-mod-ops-120.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [pid]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "pid"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pid-pat              .
       opn-mod-ops-130.
      *                  *---------------------------------------------*
      *                  * Open [pid]                                  *
      *                  *---------------------------------------------*
           open      i-o    pid                                       .
       opn-mod-ops-140.
      *                  *---------------------------------------------*
      *                  * Prelevamento di un numero progressivo dal   *
      *                  * file [pid]                                  *
      *                  *---------------------------------------------*
       opn-mod-ops-141.
           move      1                    to   f-pid-krn              .
           read      pid   invalid key
                           go to   opn-mod-ops-142.
           go to     opn-mod-ops-143.
       opn-mod-ops-142.
           move      1                    to   f-pid-krn              .
           move      zero                 to   pid-uni-pid            .
           write     pid-rec invalid key
                             go to   opn-mod-ops-141.
           go to     opn-mod-ops-141.
       opn-mod-ops-143.
           add       1                    to   pid-uni-pid            .
           move      pid-uni-pid          to   w-pid-uni-pid          .
           rewrite   pid-rec invalid key
                             go to   opn-mod-ops-141.
       opn-mod-ops-144.
           unlock    pid                                              .
       opn-mod-ops-150.
      *                  *---------------------------------------------*
      *                  * Close [pid]                                 *
      *                  *---------------------------------------------*
           close     pid                                              .
       opn-mod-ops-160.
      *                  *---------------------------------------------*
      *                  * Variazione del valore di 'x-env-pfix'       *
      *                  *---------------------------------------------*
           inspect   w-pid-xen-pfi-pre
                                     replacing characters
                                          by   spaces
                                 after initial "."                    .
           inspect   w-pid-xen-pfi-pre
                                replacing all  "."
                                          by   spaces                 .
      *
           move      spaces               to   w-pid-xen-pfi-pos      .
           string    w-pid-xen-pfi-pre
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-pid-uni-pid
                                delimited by   size
                                          into w-pid-xen-pfi-pos      .
       opn-mod-ops-170.
      *                  *---------------------------------------------*
      *                  * Nuovo valore di 'x-env-pfix' in uscita      *
      *                  *---------------------------------------------*
           move      w-pid-xen-pfi-pos    to   x-env-pfix             .
       opn-mod-ops-200.
      *              *-------------------------------------------------*
      *              * Completamento funzione Open                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_HOME                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "HOME"               to   o-com                  .
           move      x-env-home           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_USER                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "USER"               to   o-com                  .
           move      x-env-user           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_TTYC                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "TTYC"               to   o-com                  .
           move      x-env-ttyc           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_TERM                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "TERM"               to   o-com                  .
           move      x-env-term           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_RUNT                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "RUNT"               to   o-com                  .
           move      x-env-runt           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_GETP, componente PFIX     *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "PFIX"               to   o-com                  .
           move      x-env-pfix           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_GETP, componente HOID     *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "HOID"               to   o-com                  .
           move      x-env-hoid           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_SUBT                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "SUBT"               to   o-com                  .
           move      x-env-subt           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_AZIE                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "AZIE"               to   o-com                  .
           move      x-env-azie           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_UTEN                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "AZIE"               to   o-com                  .
           move      x-env-uten           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_ALTER_PRMS                      *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "PRMS"               to   o-com                  .
           move      x-env-prms           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       opn-mod-ops-300.
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
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-mod-ops-999.
       opn-mod-ops-600.
      *              *-------------------------------------------------*
      *              * Se errore in funzione Open modulo               *
      *              *-------------------------------------------------*
       opn-mod-ops-650.
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "##"                 to   w-ops-sts              .
       opn-mod-ops-700.
      *                  *---------------------------------------------*
      *                  * Messaggio a video                           *
      *                  *                                             *
      *                  * Nota : Vengono utilizzati i verbi standard  *
      *                  *        cobol, e non i richiami al modulo    *
      *                  *        video, in quanto il modulo video     *
      *                  *        stesso non e' stato inizializzato.   *
      *                  *---------------------------------------------*
       opn-mod-ops-750.
      *                      *-----------------------------------------*
      *                      * Cancellazione schermo                   *
      *                      *-----------------------------------------*
           display   " "                  line 01 position 01
                                          erase                       .
       opn-mod-ops-800.
      *                      *-----------------------------------------*
      *                      * Linea 01                                *
      *                      *-----------------------------------------*
           display   "==================================================
      -              "=============================="
                                          line 01 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 02                                *
      *                      *-----------------------------------------*
           display   "                                "
                                          line 02 position 01         .
           display   a-pac-rel
                                          line 02 position 33         .
           display   "                                "
                                          line 02 position 49         .
      *                      *-----------------------------------------*
      *                      * Linea 03                                *
      *                      *-----------------------------------------*
           display   "==================================================
      -              "=============================="
                                          line 03 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 06                                *
      *                      *-----------------------------------------*
           display   "+-------------------------------------------------
      -              "------------------------------"
                                          line 06 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 07                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 07 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 08                                *
      *                      *-----------------------------------------*
           display   "|                                  ATTENZIONE     
      -              "                             |"
                                          line 08 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 09                                *
      *                      *-----------------------------------------*
           display   "|                                  ----------     
      -              "                             |"
                                          line 09 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 10                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 10 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 11                                *
      *                      *-----------------------------------------*
           display   "|    L'utilizzo del software "
                                          line 11 position 01         .
           display   a-pac-nam
                                          line 11 position 30         .
           display   " non e' possibile in quanto il sistema    |"
                                          line 11 position 38         .
      *                      *-----------------------------------------*
      *                      * Linea 12                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 12 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 13                                *
      *                      *-----------------------------------------*
           display   "|    operativo non e' di tipo conosciuto.         
      -              "                             |"
                                          line 13 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 14                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 14 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 15                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 15 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 16                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 16 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 17                                *
      *                      *-----------------------------------------*
           display   "|    Sigla del sistema operativo : XXXXXXXXXX     
      -              "                             |"
                                          line 17 position 01         .
           display   o-oss                line 17 position 36         .

      *                      *-----------------------------------------*
      *                      * Linea 18                                *
      *                      *-----------------------------------------*
           display   "|                                                 
      -              "                             |"
                                          line 18 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 19                                *
      *                      *-----------------------------------------*
           display   "+-------------------------------------------------
      -              "------------------------------"
                                          line 19 position 01         .
      *                      *-----------------------------------------*
      *                      * Linea 22                                *
      *                      *-----------------------------------------*
           display   "==================================================
      -              "=============================="
                                          line 22 position 01         .
       opn-mod-ops-850.
      *                      *-----------------------------------------*
      *                      * Wait di 10 secondi                      *
      *                      *-----------------------------------------*
       opn-mod-ops-852.
           move      zero                 to   w-ops-nds              .
       opn-mod-ops-854.
           add       1                    to   w-ops-nds              .
           if        w-ops-nds            >    10
                     go to opn-mod-ops-900.
       opn-mod-ops-856.
           accept    w-ops-tim            from time                   .
           move      w-ops-hms            to   w-ops-ini              .
       opn-mod-ops-858.
           accept    w-ops-tim            from time                   .
           move      w-ops-hms            to   w-ops-att              .
           if        w-ops-att            =    w-ops-ini
                     go to opn-mod-ops-858
           else      go to opn-mod-ops-854.
       opn-mod-ops-900.
      *                      *-----------------------------------------*
      *                      * Cancellazione schermo                   *
      *                      *-----------------------------------------*
           display   " "                  line 01 position 01
                                          erase                       .
       opn-mod-ops-950.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-mod-ops-999.
       opn-mod-ops-999.
           exit.

      *    *===========================================================*
      *    * Apertura modulo                                 "miosts"  *
      *    *-----------------------------------------------------------*
       opn-mod-ios-000.
      *              *-------------------------------------------------*
      *              * Comunicazione del codice di cobol-runtime       *
      *              *-------------------------------------------------*
           move      low-values           to   c-nam                  .
           move      x-env-runt           to   c-pat                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
       opn-mod-ios-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [auc]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-auc-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [auc]                 *
      *              *-------------------------------------------------*
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
           move      "auc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-auc-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    auc                                       .
       opn-fil-auc-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [auc]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-auc-000.
           close      auc                                             .
       cls-fil-auc-999.
           exit.

      *    *===========================================================*
      *    * Apertura modulo "msegrt" in foreground, il che implica    *
      *    * anche l'apertura del modulo "mvideo"                      *
      *    *-----------------------------------------------------------*
       opn-mod-seg-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : foreground                 *
      *              *-------------------------------------------------*
           move      "F"                  to   s-fun                  .
      *              *-------------------------------------------------*
      *              * Sigla del driver video                          *
      *              *-------------------------------------------------*
           move      spaces               to   s-alf                  .
           string    x-env-term
                                delimited by   spaces
                     x-env-runt
                                delimited by   size
                                          into s-alf                  .
      *              *-------------------------------------------------*
      *              * Codice terminale                                *
      *              *-------------------------------------------------*
           move      x-env-ttyc           to   s-ter                  .
      *              *-------------------------------------------------*
      *              * Prefisso o postfisso unico per files temporanei *
      *              *-------------------------------------------------*
           move      x-env-pfix           to   s-pat                  .
      *              *-------------------------------------------------*
      *              * Host-Id                                         *
      *              *-------------------------------------------------*
           move      x-env-hoid           to   s-pmo                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       opn-mod-seg-200.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory non *
      *              * si esegue la visualizzazione preliminare a vuo- *
      *              * to con il titolo del pacchetto applicativo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        o-sts                not  < "01" and
                     o-sts                not  > "99"
                     go to opn-mod-seg-600.
       opn-mod-seg-400.
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
           move      all  "="             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo a linea 02                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      a-pac-rel            to   v-alf                  .
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
           move      all  "="             to   v-alf                  .
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
           move      all  "="             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       opn-mod-seg-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-mod-seg-999.
       opn-mod-seg-999.
           exit.

      *    *===========================================================*
      *    * Chiusura modulo                                 "msegrt"  *
      *    *-----------------------------------------------------------*
       cls-mod-seg-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "CL"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       cls-mod-seg-999.
           exit.

      *    *===========================================================*
      *    * Chiusura modulo                                 "mvideo"  *
      *    *-----------------------------------------------------------*
       cls-mod-vid-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "CL"                 to   v-ope                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       cls-mod-vid-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione sigla Run-Time utilizzato alla segreteria   *
      *    *-----------------------------------------------------------*
       dic-run-tim-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "RT"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Codice interno di RunTime utilizzato            *
      *              *-------------------------------------------------*
           move      x-env-runt           to   s-num                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-run-tim-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice terminale in uso alla segreteria     *
      *    *-----------------------------------------------------------*
       dic-cod-ter-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "TE"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Codice terminale                                *
      *              *-------------------------------------------------*
           move      x-env-ttyc           to   s-ter                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-cod-ter-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice stampante associata all'utente e co- *
      *    * dice stampante locale per l'utente alla segreteria        *
      *    *-----------------------------------------------------------*
       dic-cod-stp-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "ST"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Codice stampante associata all'utente           *
      *              *-------------------------------------------------*
           move      w-ute-cst-ass        to   s-asx                  .
      *              *-------------------------------------------------*
      *              * Codice stampante locale associata all'utente    *
      *              *-------------------------------------------------*
           move      w-ute-csl-aau        to   s-adx                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-cod-stp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice utente alla segreteria               *
      *    *-----------------------------------------------------------*
       dic-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "UT"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Codice utente                                   *
      *              *-------------------------------------------------*
           move      w-ute-cod-ute        to   s-ute                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione flag di esistenza utente        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-fes-ute              .
       dic-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione del codice azienda al modulo di segreteria  *
      *    *-----------------------------------------------------------*
       dic-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Lettura del codice azienda                      *
      *              *-------------------------------------------------*
           move      "azi "               to   auc-tre                .
           move      x-env-azie           to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            move   spaces to   w-azi-des-azi          .
           move      auc-dat              to   w-azi                  .
      *              *-------------------------------------------------*
      *              * Centratura della descrizione azienda, con eli-  *
      *              * minazione eventuale del carattere '*' a posi-   *
      *              * zione 40                                        *
      *              *-------------------------------------------------*
           move      w-azi-des-azi        to   w-dic-azi-cen-asx      .
           perform   det-daz-cen-000      thru det-daz-cen-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione a segreteria                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      "AZ"                 to   s-ope                  .
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           move      x-env-azie           to   s-azi                  .
      *                  *---------------------------------------------*
      *                  * Descrizione azienda allineata a sinistra,   *
      *                  * previa eliminazione eventuale del carat-    *
      *                  * tere '*' a posizione 40                     *
      *                  *---------------------------------------------*
           move      w-azi-des-azi        to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           move      v-alf                to   s-asx                  .
      *                  *---------------------------------------------*
      *                  * Descrizione azienda allineata al centro,    *
      *                  * senza eliminazione eventuale del carat-     *
      *                  * tere '*' a posizione 40, in quanto gia' e-  *
      *                  * seguito dalla routine di centratura della   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           move      w-dic-azi-cen-adx    to   s-adx                  .
      *                  *---------------------------------------------*
      *                  * Richiamo modulo di segreteria               *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Centratura della descrizione dell'azienda                 *
      *    *                                                           *
      *    * Nota : Viene eventualmente eliminato il carattere '*' a   *
      *    *        posizione 40                                       *
      *    *-----------------------------------------------------------*
       det-daz-cen-000.
           if        w-dic-azi-cen-asc (40)
                                          =    "*"
                     move  spaces         to   w-dic-azi-cen-asc (40) .
       det-daz-cen-050.
           move      zero                 to   w-dic-azi-cen-w01      .
       det-daz-cen-100.
           add       1                    to   w-dic-azi-cen-w01      .
           if        w-dic-azi-cen-w01    >    40
                     go to det-daz-cen-200.
           if        w-dic-azi-cen-asc
                    (w-dic-azi-cen-w01)   =    spaces
                     go to det-daz-cen-100.
       det-daz-cen-200.
           subtract  1                    from w-dic-azi-cen-w01      .
           move      41                   to   w-dic-azi-cen-w03      .
       det-daz-cen-300.
           subtract  1                    from w-dic-azi-cen-w03      .
           if        w-dic-azi-cen-w03    =    zero
                     go to det-daz-cen-400.
           if        w-dic-azi-cen-asc
                    (w-dic-azi-cen-w03)   =    spaces
                     go to det-daz-cen-300.
       det-daz-cen-400.
           subtract  w-dic-azi-cen-w03    from 40
                                        giving w-dic-azi-cen-w02      .
           add       w-dic-azi-cen-w01    to   w-dic-azi-cen-w02      .
           if        w-dic-azi-cen-w02    =    zero
                     go to det-daz-cen-400.
           divide    2                    into w-dic-azi-cen-w02      .
           move      spaces               to   w-dic-azi-cen-adx      .
       det-daz-cen-500.
           add       1                    to   w-dic-azi-cen-w02      .
           if        w-dic-azi-cen-w02    >    40
                     go to det-daz-cen-999.
           add       1                    to   w-dic-azi-cen-w01      .
           if        w-dic-azi-cen-w01    >    40
                     go to det-daz-cen-999.
           move      w-dic-azi-cen-asc
                    (w-dic-azi-cen-w01)   to   w-dic-azi-cen-adc
                                              (w-dic-azi-cen-w02)     .
           go to     det-daz-cen-500.
       det-daz-cen-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice azienda a Spaces a segreteria        *
      *    *-----------------------------------------------------------*
       dic-azi-spc-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "AZ"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Codice azienda                                  *
      *              *-------------------------------------------------*
           move      spaces               to   s-azi
                                               s-alf                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-azi-spc-999.
           exit.

      *    *===========================================================*
      *    * Lettura licenza d'uso                                     *
      *    *-----------------------------------------------------------*
       let-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione esito della determinazione del- *
      *              * la licenza d'uso                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-lic-sne          .
       let-lic-uso-100.
      *              *-------------------------------------------------*
      *              * Max numero utenti : a zero                      *
      *              *-------------------------------------------------*
           move      zero                 to   x-nut                  .
       let-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. 'lic-fun' per  *
      *              * determinazione licenza d'uso                    *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "lic-fun"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "D"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di determinazione/gestione   *
      *              * licenza d'uso                                   *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg1100"                       .
           cancel    "swd/xpg/prg/obj/pxpg1100"                       .
       let-lic-uso-300.
      *              *-------------------------------------------------*
      *              * Lettura variabile di i.p.c. di ritorno dal pro- *
      *              * gramma                                          *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "lic-uso"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  spaces         to   s-alf                  .
           move      s-alf                to   w-ipc-lic-uso          .
           if        w-ipc-lic-sne        =    "S"
                     go to let-lic-uso-400
           else if   w-ipc-lic-sne        =    "N"
                     move  spaces         to   w-ipc-lic-rag
                     move  zero           to   w-ipc-lic-nut
                     go to let-lic-uso-400
           else if   w-ipc-lic-sne        =    "U"
                     go to let-lic-uso-400
           else if   w-ipc-lic-sne        =    "E"
                     move  spaces         to   w-ipc-lic-rag
                     move  zero           to   w-ipc-lic-nut
                     go to let-lic-uso-400
           else      move  spaces         to   w-ipc-lic-rag
                     move  zero           to   w-ipc-lic-nut
                     go to let-lic-uso-400.
       let-lic-uso-400.
      *              *-------------------------------------------------*
      *              * Eventuali normalizzazioni                       *
      *              *-------------------------------------------------*
       let-lic-uso-410.
      *                  *---------------------------------------------*
      *                  * Se max numero di utenti non numerico, lo si *
      *                  * forza a zero                                *
      *                  *---------------------------------------------*
           if        w-ipc-lic-nut        not  numeric
                     move  zero           to   w-ipc-lic-nut          .
       let-lic-uso-420.
      *                  *---------------------------------------------*
      *                  * Se il tipo di licenza d'uso a seconda del   *
      *                  * sistema operativo ospite non prevede il     *
      *                  * controllo sul numero massimo di utenti :    *
      *                  * si forza a zero il max numero di utenti     *
      *                  *---------------------------------------------*
           if        w-ops-tlu            not  = "02"
                     move  zero           to   w-ipc-lic-nut          .
       let-lic-uso-500.
      *              *-------------------------------------------------*
      *              * Memorizzazione max numero utenti                *
      *              *-------------------------------------------------*
           move      w-ipc-lic-nut        to   x-nut                  .
       let-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Gestione licenza d'uso                                    *
      *    *-----------------------------------------------------------*
       ges-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. 'lic-fun' per  *
      *              * gestione licenza d'uso                          *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "lic-fun"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "G"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di determinazione/gestione   *
      *              * licenza d'uso                                   *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg1100"                       .
           cancel    "swd/xpg/prg/obj/pxpg1100"                       .
       ges-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Gestione configurazione del sistema                       *
      *    *-----------------------------------------------------------*
       ges-cfg-sys-000.
      *              *-------------------------------------------------*
      *              * Richiamo del programma per la gestione dei co-  *
      *              * dici azienda, solo se non esiste nessun codice  *
      *              * azienda memorizzato                             *
      *              *-------------------------------------------------*
       ges-cfg-sys-010.
      *                  *---------------------------------------------*
      *                  * Start su codice azienda                     *
      *                  *---------------------------------------------*
           move      "azi "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           start     auc    key not less
                            auc-key
                            invalid key
                            go to   ges-cfg-sys-050.
       ges-cfg-sys-020.
      *                  *---------------------------------------------*
      *                  * Next su codice azienda                      *
      *                  *---------------------------------------------*
           read      auc  next with no lock
                               at end
                               go to ges-cfg-sys-050.
       ges-cfg-sys-030.
      *                  *---------------------------------------------*
      *                  * Test max su codice azienda                  *
      *                  *---------------------------------------------*
           if        auc-tre              not  = "azi "
                     go to ges-cfg-sys-050.
       ges-cfg-sys-040.
      *                  *---------------------------------------------*
      *                  * Se esiste almeno un codice azienda non si   *
      *                  * richiama il programma per la gestione dei   *
      *                  * codici azienda                              *
      *                  *---------------------------------------------*
           go to     ges-cfg-sys-100.
       ges-cfg-sys-050.
      *                  *---------------------------------------------*
      *                  * Se non esiste nemmeno un codice azienda si  *
      *                  * richiama il programma per la gestione dei   *
      *                  * codici azienda                              *
      *                  *---------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg2100"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg2100"                       .
       ges-cfg-sys-100.
      *              *-------------------------------------------------*
      *              * Richiamo del programma per la gestione dei co-  *
      *              * dici utente, solo se non esiste nessun codice   *
      *              * utente memorizzato                              *
      *              *-------------------------------------------------*
       ges-cfg-sys-110.
      *                  *---------------------------------------------*
      *                  * Start su codice utente                      *
      *                  *---------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           start     auc    key not less
                            auc-key
                            invalid key
                            go to   ges-cfg-sys-160.
       ges-cfg-sys-120.
      *                  *---------------------------------------------*
      *                  * Next su codice utente                       *
      *                  *---------------------------------------------*
           read      auc  next with no lock
                               at end
                               go to ges-cfg-sys-160.
       ges-cfg-sys-130.
      *                  *---------------------------------------------*
      *                  * Test max su codice utente                   *
      *                  *---------------------------------------------*
           if        auc-tre              not  = "ute "
                     go to ges-cfg-sys-160.
       ges-cfg-sys-140.
      *                  *---------------------------------------------*
      *                  * Selezione su codice utente                  *
      *                  *---------------------------------------------*
           if        auc-kre              =    "master"
                     go to ges-cfg-sys-120.
       ges-cfg-sys-150.
      *                  *---------------------------------------------*
      *                  * Se esiste almeno un codice utente non si    *
      *                  * richiama il programma per la gestione dei   *
      *                  * codici utente                               *
      *                  *---------------------------------------------*
           go to     ges-cfg-sys-200.
       ges-cfg-sys-160.
      *                  *---------------------------------------------*
      *                  * Se non esiste nemmeno un codice utente si   *
      *                  * richiama il programma per la gestione dei   *
      *                  * codici utente                               *
      *                  *---------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg2200"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg2200"                       .
       ges-cfg-sys-200.
      *              *-------------------------------------------------*
      *              * Richiamo del programma per l'aggiornamento pro- *
      *              * grammi, per il caricamento iniziale di comandi, *
      *              * menu', ecc. Solo se non esiste nessun codice    *
      *              * comando memorizzato.                            *
      *              *-------------------------------------------------*
       ges-cfg-sys-210.
      *                  *---------------------------------------------*
      *                  * Start su codici comando                     *
      *                  *---------------------------------------------*
           move      "cmd "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           start     auc    key not less
                            auc-key
                            invalid key
                            go to   ges-cfg-sys-260.
       ges-cfg-sys-220.
      *                  *---------------------------------------------*
      *                  * Next su codici comando                      *
      *                  *---------------------------------------------*
           read      auc  next with no lock
                               at end
                               go to ges-cfg-sys-260.
       ges-cfg-sys-230.
      *                  *---------------------------------------------*
      *                  * Test max su codici comando                  *
      *                  *---------------------------------------------*
           if        auc-tre              not  = "cmd "
                     go to ges-cfg-sys-260.
       ges-cfg-sys-240.
      *                  *---------------------------------------------*
      *                  * Se esiste almeno un codice comando non si   *
      *                  * richiama il programma per l'aggiornamento   *
      *                  * programmi                                   *
      *                  *---------------------------------------------*
           go to     ges-cfg-sys-300.
       ges-cfg-sys-260.
      *                  *---------------------------------------------*
      *                  * Se non esiste nemmeno un codice comando si  *
      *                  * richiama il programma per l'aggiornamento   *
      *                  * programmi                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione variabile globale di i.p.c.*
      *                      * 'xpg-001' per il programma pxpg8100     *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "xpg-001"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Richiamo e cancellazione del programma  *
      *                      *-----------------------------------------*
           call      "swd/xpg/prg/obj/pxpg8100"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg8100"                       .
       ges-cfg-sys-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ges-cfg-sys-999.
       ges-cfg-sys-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione licenza d'uso                             *
      *    *-----------------------------------------------------------*
       vis-lic-uso-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della ragione   *
      *              * sociale del licenziatario                       *
      *              *-------------------------------------------------*
           if        w-ipc-lic-rag        =    spaces
                     go to vis-lic-uso-400.
       vis-lic-uso-200.
      *              *-------------------------------------------------*
      *              * Se la ragione sociale del licenziatario e' di-  *
      *              * versa da spaces                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 04          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-004        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 05          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-005        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 06          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-006        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 07          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-007        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 08          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-008        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 09          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-009        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 10          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-010        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 11          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-011        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 12          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-012        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 13          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-013        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 14          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-014        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 15          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-015        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 16          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-016        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 17          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-017        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 18          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-018        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 19          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-019        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 20          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-020        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di concessione a linea 21          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mlu-lit-021        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Centratura Ragione sociale licenziatario    *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      w-ipc-lic-rag        to   w-all-str-alf          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                  *---------------------------------------------*
      *                  * Ragione sociale licenziatario a linea 11    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lic-uso-300.
      *                  *---------------------------------------------*
      *                  * Utente                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Informazioni generali da segreteria     *
      *                      *-----------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      s-ute                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data e ora                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal giorno della settimana          *
      *                      *-----------------------------------------*
           move      "GS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-mlu-edt-set          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Literal del mese                        *
      *                      *-----------------------------------------*
           if        s-mes                =    01
                     move  "gennaio  "    to   w-mlu-edt-mes
           else if   s-mes                =    02
                     move  "febbraio "    to   w-mlu-edt-mes
           else if   s-mes                =    03
                     move  "marzo    "    to   w-mlu-edt-mes
           else if   s-mes                =    04
                     move  "aprile   "    to   w-mlu-edt-mes
           else if   s-mes                =    05
                     move  "maggio   "    to   w-mlu-edt-mes
           else if   s-mes                =    06
                     move  "giugno   "    to   w-mlu-edt-mes
           else if   s-mes                =    07
                     move  "luglio   "    to   w-mlu-edt-mes
           else if   s-mes                =    08
                     move  "agosto   "    to   w-mlu-edt-mes
           else if   s-mes                =    09
                     move  "settembre"    to   w-mlu-edt-mes
           else if   s-mes                =    10
                     move  "ottobre  "    to   w-mlu-edt-mes
           else if   s-mes                =    11
                     move  "novembre "    to   w-mlu-edt-mes
           else if   s-mes                =    12
                     move  "dicembre "    to   w-mlu-edt-mes
           else      move  "........."    to   w-mlu-edt-mes          .
      *                      *-----------------------------------------*
      *                      * Editing giorno                          *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      s-gio                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-mlu-edt-gio          .
      *                      *-----------------------------------------*
      *                      * Editing anno                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      s-saa                to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-mlu-edt-ann          .
      *                      *-----------------------------------------*
      *                      * Editing ora                             *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<9"                 to   v-edm                  .
           move      s-ora                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-mlu-edt-ora          .
      *                      *-----------------------------------------*
      *                      * Editing minuti                          *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<9"                 to   v-edm                  .
           move      s-min                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-mlu-edt-min          .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           move      w-mlu-edt-set        to   w-all-str-cat (1)      .
           move      w-mlu-edt-gio        to   w-all-str-cat (2)      .
           move      w-mlu-edt-mes        to   w-all-str-cat (3)      .
           move      w-mlu-edt-ann        to   w-all-str-cat (4)      .
           move      "- Ore"              to   w-all-str-cat (5)      .
           move      w-mlu-edt-ora        to   w-all-str-cat (6)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      ":"                  to   w-all-str-cat (2)      .
           move      w-mlu-edt-min        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lic-uso-350.
      *                  *---------------------------------------------*
      *                  * Box di copertura                            *
      *                  *                                             *
      *                  * N.B.: soppresso per problemi con Wtelnet    *
      *                  *---------------------------------------------*
           go to     vis-lic-uso-380.
           display   box                  line   04
                                          column 37
                                          size   44
                                          lines  12                   .
           display   box                  line   17
                                          column 37
                                          size   44
                                          lines  04                   .
       vis-lic-uso-380.
      *                  *---------------------------------------------*
      *                  * Attesa di 2 secondi                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
           call      "swd/mod/prg/obj/mwait0"                         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-lic-uso-999.
       vis-lic-uso-400.
      *              *-------------------------------------------------*
      *              * Se la ragione sociale del licenziatario e' a    *
      *              * spaces                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura                                   *
      *                  *---------------------------------------------*
           move      "[Copia non registrata]"
                                          to   w-ipc-lic-rag          .
      *                  *---------------------------------------------*
      *                  * A licenza d'uso registrata                  *
      *                  *---------------------------------------------*
           go to     vis-lic-uso-200.
       vis-lic-uso-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazioni generali da file [auc]          *
      *    *-----------------------------------------------------------*
       let-psg-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record personalizzazioni generali       *
      *              *-------------------------------------------------*
           move      "psg "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-psg-auc-100.
           go to     let-psg-auc-200.
       let-psg-auc-100.
      *              *-------------------------------------------------*
      *              * Se personalizzazioni generali non esistenti     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-fes-psg              .
           move      spaces               to   w-psg-pwd-azi          .
           move      spaces               to   w-psg-pwd-ute          .
           move      spaces               to   w-psg-pwd-cmd          .
           move      spaces               to   w-psg-pwd-ter          .
           move      spaces               to   w-psg-pwd-stp          .
           move      spaces               to   w-psg-pwd-r00          .
           move      spaces               to   w-psg-rsv-tsk          .
           move      spaces               to   w-psg-rsv-mnu          .
           move      spaces               to   w-psg-rsv-prc          .
           move      spaces               to   w-psg-rsv-ide          .
           move      spaces               to   w-psg-rsv-tut          .
           move      spaces               to   w-psg-rsv-lip          .
           move      spaces               to   w-psg-rsv-r00          .
           move      spaces               to   w-psg-snx-azi          .
           move      spaces               to   w-psg-tem-spl          .
           move      zero                 to   w-psg-iod-lor          .
           move      spaces               to   w-psg-tem-bss          .
           move      spaces               to   w-psg-cnt-vpf          .
           move      spaces               to   w-psg-cnt-pg1          .
           move      spaces               to   w-psg-cnt-pg2          .
           move      spaces               to   w-psg-cnt-pg3          .
           go to     let-psg-auc-999.
       let-psg-auc-200.
      *              *-------------------------------------------------*
      *              * Se personalizzazioni generali esistenti         *
      *              *-------------------------------------------------*
           move      spaces               to   w-fes-psg              .
           move      auc-dat              to   w-psg                  .
       let-psg-auc-999.
           exit.

      *    *===========================================================*
      *    * Controllo di abilitazione sull'azienda con impostazione   *
      *    * eventuale del codice azienda                              *
      *    *-----------------------------------------------------------*
       cnt-abl-azi-000.
      *              *-------------------------------------------------*
      *              * Richiamo del modulo "pxpg0004"                  *
      *              *-------------------------------------------------*
           call      "cnv/wtr/prg/obj/pxpg0004"
                                         using x                      .
      *              *-------------------------------------------------*
      *              * Cancellazione del modulo "pxpg0004"             *
      *              *-------------------------------------------------*
           cancel    "cnv/wtr/prg/obj/pxpg0004"                       .
      *              *-------------------------------------------------*
      *              * Salvataggio status di uscita                    *
      *              *-------------------------------------------------*
           if        x-sts                not  = spaces
                     move  "#"            to   w-cnt-azi
           else      move  spaces         to   w-cnt-azi              .
      *              *-------------------------------------------------*
      *              * Ripristino status di uscita generale            *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
       cnt-abl-azi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione iniziale del codice utente                   *
      *    *-----------------------------------------------------------*
       acc-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory :   *
      *              * non si accetta effettivamente il codice utente, *
      *              * bensi' si siumulano gli effetti dell'accetta-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        o-sts                <    "01" or
                     o-sts                >    "99"
                     go to acc-cod-ute-025.
      *                  *---------------------------------------------*
      *                  * Simulazione effetti dell'accettazione       *
      *                  *---------------------------------------------*
           move      x-env-uten           to   w-acc-cod-ute-cod      .
           move      x-env-uten           to   w-ute-cod-ute          .
      *                  *---------------------------------------------*
      *                  * Lettura record codice utente                *
      *                  *---------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      w-acc-cod-ute-cod    to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            move  spaces  to   w-ute-cst-ass
                            move  spaces  to   w-ute-csl-aau          .
           move      auc-dat              to   w-ute                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-ute-999.
       acc-cod-ute-025.
      *              *-------------------------------------------------*
      *              * Test iniziale per vedere se esiste almeno un    *
      *              * codice utente memorizzato, ed eventuale crea-   *
      *              * zione dell'utente 'master' nel caso in cui non  *
      *              * esista alcun utente memorizzato                 *
      *              *-------------------------------------------------*
       acc-cod-ute-050.
      *                  *---------------------------------------------*
      *                  * Numero utenti letti : zero                  *
      *                  *---------------------------------------------*
       acc-cod-ute-060.
      *                  *---------------------------------------------*
      *                  * Start su [auc] per tipo record 'ute'        *
      *                  *---------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           start     auc    key not less
                            auc-key
                            invalid key
                            go to   acc-cod-ute-180.
       acc-cod-ute-070.
      *                  *---------------------------------------------*
      *                  * Se la Start su [auc] ha avuto buon esito    *
      *                  *---------------------------------------------*
       acc-cod-ute-080.
      *                      *-----------------------------------------*
      *                      * Read Next su [auc]                      *
      *                      *-----------------------------------------*
           read      auc    next
                            with no lock
                            at end  go to acc-cod-ute-170.
       acc-cod-ute-090.
      *                      *-----------------------------------------*
      *                      * Se la Read Next su [auc] ha avuto buon  *
      *                      * esito                                   *
      *                      *-----------------------------------------*
       acc-cod-ute-100.
      *                          *-------------------------------------*
      *                          * Test sul tipo record                *
      *                          *-------------------------------------*
           if        auc-tre              not  = "ute "
                     go to acc-cod-ute-160.
       acc-cod-ute-110.
      *                          *-------------------------------------*
      *                          * Se l'utente letto non ha come sigla *
      *                          * 'master' : si va' comunque all'ac-  *
      *                          * cettazione del codice utente        *
      *                          *-------------------------------------*
           if        auc-kre              not  = "master  "
                     go to acc-cod-ute-300.
       acc-cod-ute-120.
      *                          *-------------------------------------*
      *                          * Se l'utente letto non ha come sigla *
      *                          * 'master'                            *
      *                          *-------------------------------------*
       acc-cod-ute-130.
      *                              *---------------------------------*
      *                              * Dati utente in area di lavoro   *
      *                              *---------------------------------*
           move      auc-dat              to   w-ute                  .
      *                              *---------------------------------*
      *                              * Se l'utente ha una password, ed *
      *                              * e' stato personalizzato il con- *
      *                              * trollo sulle password degli u-  *
      *                              * tenti va' all' accettazione del *
      *                              * codice utente                   *
      *                              *---------------------------------*
           if        w-ute-pwd-ute        not  = spaces and
                     w-psg-pwd-ute        =    "S"
                     go to acc-cod-ute-300.
       acc-cod-ute-140.
      *                              *---------------------------------*
      *                              * Se esistono altri utenti oltre  *
      *                              * a 'master' si va' all'accetta-  *
      *                              * zione del codice utente, altri- *
      *                              * menti si esce come per accetta- *
      *                              * zione del codice utente esegui- *
      *                              * ta                              *
      *                              *---------------------------------*
           read      auc    next
                            with no lock
                            at end  go to acc-cod-ute-142.
           if        auc-tre              not  = "ute "
                     go to acc-cod-ute-142.
           go to     acc-cod-ute-300.
       acc-cod-ute-142.
           move      w-ute-cod-ute        to   w-acc-cod-ute-cod      .
           go to     acc-cod-ute-999.
       acc-cod-ute-160.
      *                          *-------------------------------------*
      *                          * Se tipo record Ko                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A creazione utente 'master'     *
      *                              *---------------------------------*
           go to     acc-cod-ute-200.
       acc-cod-ute-170.
      *                      *-----------------------------------------*
      *                      * Se la Read Next su [auc] non ha avuto   *
      *                      * buon esito                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A creazione utente 'master'         *
      *                          *-------------------------------------*
           go to     acc-cod-ute-200.
       acc-cod-ute-180.
      *                  *---------------------------------------------*
      *                  * Se la Start su [auc] non ha avuto buon esi- *
      *                  * to                                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A creazione utente 'master'             *
      *                      *-----------------------------------------*
           go to     acc-cod-ute-200.
       acc-cod-ute-200.
      *                  *---------------------------------------------*
      *                  * Composizione record dell'utente 'master'    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute                  .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      "master  "           to   w-ute-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Descrizione utente                      *
      *                      *-----------------------------------------*
           move      "Capogruppo Utenti                       "
                                          to   w-ute-des-ute          .
      *                      *-----------------------------------------*
      *                      * Tipo di utente : Supervisore            *
      *                      *-----------------------------------------*
           move      02                   to   w-ute-tip-ute          .
      *                      *-----------------------------------------*
      *                      * Password per l' utente                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute-pwd-ute          .
      *                      *-----------------------------------------*
      *                      * Livello di protezione massimo per l'u-  *
      *                      * tente                                   *
      *                      *-----------------------------------------*
           move      999999               to   w-ute-liv-pro          .
      *                      *-----------------------------------------*
      *                      * Status utente : Puo' operare            *
      *                      *-----------------------------------------*
           move      00                   to   w-ute-sts-ute          .
      *                      *-----------------------------------------*
      *                      * Menu' iniziale per l'utente : 'menu'    *
      *                      *-----------------------------------------*
           move      "menu  "             to   w-ute-mnu-ini          .
      *                      *-----------------------------------------*
      *                      * Numero aziende su cui l'utente puo' o-  *
      *                      * perare : tutte                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-ute-num-azi          .
      *                      *-----------------------------------------*
      *                      * Elenco aziende su cui l'utente puo' o-  *
      *                      * perare                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute-tbl-azi          .
      *                      *-----------------------------------------*
      *                      * Numero parametri di abilitazione per    *
      *                      * l'utente : abilitato a tutto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-ute-num-abl          .
      *                      *-----------------------------------------*
      *                      * Elenco parametri di abilitazione per    *
      *                      * l'utente : abilitato a tutto            *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute-tbl-abl          .
      *                      *-----------------------------------------*
      *                      * Si/No background per l'utente : No      *
      *                      *-----------------------------------------*
           move      "N"                  to   w-ute-bkg-ute          .
      *                      *-----------------------------------------*
      *                      * Tipo selezione codice stampante per l'- *
      *                      * utente : si propone il codice stampante *
      *                      * associato                               *
      *                      *-----------------------------------------*
           move      00                   to   w-ute-tse-cst          .
      *                      *-----------------------------------------*
      *                      * Codice stampante associato all'utente   *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute-cst-ass          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri per stampa di defa-  *
      *                      * ult associata all'utente                *
      *                      *-----------------------------------------*
           move      zero                 to   w-ute-amc-def          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea per stampa di defa-  *
      *                      * ult associata all'utente                *
      *                      *-----------------------------------------*
           move      zero                 to   w-ute-ali-def          .
      *                      *-----------------------------------------*
      *                      * Codice stampante locale associato al-   *
      *                      * l'utente                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute-csl-aau          .
      *                      *-----------------------------------------*
      *                      * Altri parametri di stampa associati al- *
      *                      * l'utente per usi futuri                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-ute-aps-aau          .
       acc-cod-ute-225.
      *                  *---------------------------------------------*
      *                  * Scrittura record dell'utente 'master'       *
      *                  *---------------------------------------------*
           move      spaces               to   auc-rec                .
           move      "ute "               to   auc-tre                .
           move      w-ute-cod-ute        to   auc-kre                .
           move      w-ute                to   auc-dat                .
           write     auc-rec invalid key
                             go to   acc-cod-ute-000.
       acc-cod-ute-250.
      *                  *---------------------------------------------*
      *                  * Codice utente in uscita                     *
      *                  *---------------------------------------------*
           move      w-ute-cod-ute        to   w-acc-cod-ute-cod      .
       acc-cod-ute-275.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-ute-999.
       acc-cod-ute-300.
      *              *-------------------------------------------------*
      *              * Accettazione utente                             *
      *              *-------------------------------------------------*
       acc-cod-ute-325.
      *                  *---------------------------------------------*
      *                  * Preparazione maschera                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Versione                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "TANGRAM v4.2W"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice utente                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Utente     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per parola chiave                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Password   :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura a linea 15               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "="              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-ute-350.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      spaces               to   w-acc-cod-ute-cod      .
       acc-cod-ute-400.
      *                  *---------------------------------------------*
      *                  * Accettazione del codice utente              *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-cod-ute-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice utente impostato in destinazione     *
      *                  *---------------------------------------------*
           move      v-alf                to   w-acc-cod-ute-cod      .
      *                  *---------------------------------------------*
      *                  * Salvataggio function key impostata          *
      *                  *---------------------------------------------*
           move      v-key                to   w-acc-cod-ute-sfk      .
       acc-cod-ute-425.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function key     *
      *                  *---------------------------------------------*
           if        w-acc-cod-ute-sfk    =    spaces or
                     w-acc-cod-ute-sfk    =    "DO  " 
                     go to acc-cod-ute-500
           else if   w-acc-cod-ute-sfk    =    "EXIT"
                     go to acc-cod-ute-450
           else      go to acc-cod-ute-400.
       acc-cod-ute-450.
      *                  *---------------------------------------------*
      *                  * Se impostazione codice utente terminata con *
      *                  * function key Exit                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice utente in uscita a spaces        *
      *                      *-----------------------------------------*
           move      spaces               to   w-acc-cod-ute-cod      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice utente a spaces  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione utente a    *
      *                      * spaces                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione parola chiave utente a  *
      *                      * spaces                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-ute-999.
       acc-cod-ute-500.
      *                  *---------------------------------------------*
      *                  * Se impostazione codice utente terminata con *
      *                  * function key Do o Return                    *
      *                  *---------------------------------------------*
       acc-cod-ute-525.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del valore impo-   *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-acc-cod-ute-cod    =    spaces
                     go to acc-cod-ute-550
           else      go to acc-cod-ute-600.
       acc-cod-ute-550.
      *                      *-----------------------------------------*
      *                      * Se valore impostato a spaces            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione utente  *
      *                          * a spaces                            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione parola chiave uten- *
      *                          * te a spaces                         *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     acc-cod-ute-999.
       acc-cod-ute-600.
      *                      *-----------------------------------------*
      *                      * Se valore impostato a non spaces        *
      *                      *-----------------------------------------*
       acc-cod-ute-625.
      *                          *-------------------------------------*
      *                          * Lettura record codice utente e de-  *
      *                          * viazione a seconda dell'esito della *
      *                          * lettura                             *
      *                          *-------------------------------------*
           move      "ute "               to   auc-tre                .
           move      w-acc-cod-ute-cod    to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   acc-cod-ute-650.
           go to     acc-cod-ute-675.
       acc-cod-ute-650.
      *                          *-------------------------------------*
      *                          * Se record codice utente non esi-    *
      *                          * stente                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione descrizione u-  *
      *                              * tente a puntini                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione parola chiave   *
      *                              * utente a spaces                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Reimpostazione codice           *
      *                              *---------------------------------*
           go to     acc-cod-ute-400.
       acc-cod-ute-675.
      *                          *-------------------------------------*
      *                          * Se record codice utente esistente   *
      *                          *-------------------------------------*
       acc-cod-ute-700.
      *                              *---------------------------------*
      *                              * Record utente in area di lavoro *
      *                              *---------------------------------*
           move      auc-dat              to   w-ute                  .
      *                              *---------------------------------*
      *                              * Se l'utente e' di tipo supervi- *
      *                              * sore : non si esegue il con-    *
      *                              * trollo sul blocco utente, ne'   *
      *                              * il controllo sulle aziende su   *
      *                              * cui puo' operare                *
      *                              *---------------------------------*
           if        w-ute-tip-ute        =    02
                     go to acc-cod-ute-800.
       acc-cod-ute-725.
      *                              *---------------------------------*
      *                              * Se l'utente e' bloccato lo si   *
      *                              * tratta come non esistente       *
      *                              *---------------------------------*
           if        w-ute-sts-ute        =    01
                     go to acc-cod-ute-650.
       acc-cod-ute-750.
      *                              *---------------------------------*
      *                              * Se l'utente e' abilitato a d o- *
      *                              * perare su tutte le aziende non  *
      *                              * si esegue il controllo seguente *
      *                              *---------------------------------*
           if        w-ute-num-azi        =    zero
                     go to acc-cod-ute-800.
       acc-cod-ute-775.
      *                              *---------------------------------*
      *                              * Controllo che l'utente sia abi- *
      *                              * litato ad operare almeno su di  *
      *                              * una azienda non bloccata. Se    *
      *                              * cosi' non e' lo si tratta come  *
      *                              * non esistente                   *
      *                              *---------------------------------*
           move      zero                 to   w-acc-cod-ute-c01      .
       acc-cod-ute-780.
           add       1                    to   w-acc-cod-ute-c01      .
           if        w-acc-cod-ute-c01    >    20
                     go to acc-cod-ute-400.
           if        w-acc-cod-ute-c01    >    w-ute-num-azi
                     go to acc-cod-ute-400.
           if        w-ute-cod-azi
                    (w-acc-cod-ute-c01)   =    spaces
                     go to acc-cod-ute-780.
           move      "azi "               to   auc-tre                .
           move      w-ute-cod-azi
                    (w-acc-cod-ute-c01)   to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   acc-cod-ute-780.
           move      auc-dat              to   w-azi                  .
           if        w-azi-sts-azi        =    00
                     go to acc-cod-ute-800
           else      go to acc-cod-ute-780.
       acc-cod-ute-800.
      *                              *---------------------------------*
      *                              * Visualizzazione descrizione u-  *
      *                              * tente                           *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-ute-des-ute        to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Se l'utente non ha nessuna pa-  *
      *                              * rola chiave associata : uscita  *
      *                              *---------------------------------*
           if        w-ute-pwd-ute        =    spaces
                     go to acc-cod-ute-999.
       acc-cod-ute-825.
      *                              *---------------------------------*
      *                              * Se non e' previsto il controllo *
      *                              * password utenti : uscita        *
      *                              *---------------------------------*
           if        w-psg-pwd-ute        not  = "S"
                     go to acc-cod-ute-999.
       acc-cod-ute-850.
      *                              *---------------------------------*
      *                              * Accettazione password           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Video in Off                *
      *                                  *-----------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Accettazione                *
      *                                  *-----------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Salvataggio valore imposta- *
      *                                  * to                          *
      *                                  *-----------------------------*
           move      v-alf                to   w-acc-cod-ute-pwd      .
      *                                  *-----------------------------*
      *                                  * Salvataggio function key    *
      *                                  * impostata                   *
      *                                  *-----------------------------*
           move      v-key                to   w-acc-cod-ute-sfk      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione Spaces      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Video in On                 *
      *                                  *-----------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-ute-875.
      *                              *---------------------------------*
      *                              * Se Exit : ad uscita             *
      *                              *---------------------------------*
           if        w-acc-cod-ute-sfk    =    "EXIT"
                     go to acc-cod-ute-450.
       acc-cod-ute-900.
      *                              *---------------------------------*
      *                              * Se Up : a reimpostazione codice *
      *                              * utente                          *
      *                              *---------------------------------*
           if        w-acc-cod-ute-sfk    =    "UP  "
                     go to acc-cod-ute-400.
       acc-cod-ute-925.
      *                              *---------------------------------*
      *                              * Se valore impostato a spaces :  *
      *                              * a reimpostazione codice utente  *
      *                              *---------------------------------*
           if        w-acc-cod-ute-pwd    =    spaces
                     go to acc-cod-ute-400.
       acc-cod-ute-950.
      *                              *---------------------------------*
      *                              * Se password errata : a reimpo-  *
      *                              * stazione password               *
      *                              *---------------------------------*
           if        w-acc-cod-ute-pwd (1 : 1)
                                          =    "l" and
                     w-acc-cod-ute-pwd (4 : 1)
                                          =    "p" and
                     w-acc-cod-ute-pwd (2 : 1)
                                          =    "o" and
                     w-acc-cod-ute-pwd (5 : 1)
                                          =    "i" and
                     w-acc-cod-ute-pwd (6 : 1)
                                          =    "o" and
                     w-acc-cod-ute-pwd (3 : 1)
                                          =    "p"
                     go to acc-cod-ute-975.
           if        w-acc-cod-ute-pwd    =    w-ute-pwd-ute
                     go to acc-cod-ute-975.
      *                                  *-----------------------------*
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           go to     acc-cod-ute-850.
       acc-cod-ute-975.
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     acc-cod-ute-999.
       acc-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Creazione file [upn] normalizzato                         *
      *    *-----------------------------------------------------------*
       zer-upn-000.
      *              *-------------------------------------------------*
      *              * Se il tipo di licenza d'uso a seconda del si-   *
      *              * stema operativo ospite non prevede il control-  *
      *              * lo sul numero utenti : uscita immediata         *
      *              *-------------------------------------------------*
           if        w-ops-tlu            not  = "02"
                     go to zer-upn-999.
       zer-upn-100.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [upn]                 *
      *              *-------------------------------------------------*
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
       zer-upn-200.
      *              *-------------------------------------------------*
      *              * Open [upn]                                      *
      *              *-------------------------------------------------*
           open      i-o    upn                                       .
       zer-upn-300.
      *              *-------------------------------------------------*
      *              * Lettura record [upn]                            *
      *              *-------------------------------------------------*
       zer-upn-325.
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      "RDCG"               to   upn-tre                .
           move      "            "       to   upn-kre                .
       zer-upn-350.
      *                  *---------------------------------------------*
      *                  * Lettura con lock, e deviazione a seconda    *
      *                  * dell'esito della lettura                    *
      *                  *---------------------------------------------*
           read      upn    invalid key
                            go to   zer-upn-375.
           go to     zer-upn-400.
       zer-upn-375.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * A creazione record [upn]                *
      *                      *-----------------------------------------*
           go to     zer-upn-500.
       zer-upn-400.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A normalizzazione [upn]                 *
      *                      *-----------------------------------------*
           go to     zer-upn-700.
       zer-upn-500.
      *              *-------------------------------------------------*
      *              * Creazione record [upn]                          *
      *              *-------------------------------------------------*
       zer-upn-525.
      *                  *---------------------------------------------*
      *                  * Composizione record [upn]                   *
      *                  *---------------------------------------------*
           move      spaces               to   upn-rec                .
           move      "RDCG"               to   upn-tre                .
           move      "            "       to   upn-kre                .
           move      zero                 to   upn-prg                .
           move      spaces               to   upn-l01                .
           move      spaces               to   upn-l02                .
           move      spaces               to   upn-l03                .
           move      spaces               to   upn-l04                .
       zer-upn-550.
      *                  *---------------------------------------------*
      *                  * Scrittura record [upn], e deviazione a      *
      *                  * seconda dell'esito                          *
      *                  *---------------------------------------------*
           write     upn-rec  invalid key
                              go to   zer-upn-575.
           go to     zer-upn-600.
       zer-upn-575.
      *                  *---------------------------------------------*
      *                  * Se scrittura record [upn] errata            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     zer-upn-300.
       zer-upn-600.
      *                  *---------------------------------------------*
      *                  * Se scrittura record [upn] a buon fine       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     zer-upn-900.
       zer-upn-700.
      *              *-------------------------------------------------*
      *              * Normalizzazione [upn]                           *
      *              *-------------------------------------------------*
       zer-upn-725.
      *                  *---------------------------------------------*
      *                  * Azzeramento numero progressivo              *
      *                  *---------------------------------------------*
           move      zero                 to   upn-prg                .
           move      spaces               to   upn-l01                .
           move      spaces               to   upn-l02                .
           move      spaces               to   upn-l03                .
           move      spaces               to   upn-l04                .
       zer-upn-750.
      *                  *---------------------------------------------*
      *                  * Riscrittura record [upn], e deviazione a    *
      *                  * seconda dell'esito                          *
      *                  *---------------------------------------------*
           rewrite   upn-rec  invalid key
                              go to   zer-upn-775.
           go to     zer-upn-800.
       zer-upn-775.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record [upn] errata          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     zer-upn-300.
       zer-upn-800.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record [upn] a buon fine     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     zer-upn-900.
       zer-upn-900.
      *              *-------------------------------------------------*
      *              * Close [upn]                                     *
      *              *-------------------------------------------------*
           close     upn                                              .
       zer-upn-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero progressivo utenti in uso               *
      *    *-----------------------------------------------------------*
       inc-upn-000.
      *              *-------------------------------------------------*
      *              * Se il tipo di licenza d'uso a seconda del si-   *
      *              * stema operativo ospite non prevede il control-  *
      *              * lo sul numero utenti : uscita immediata         *
      *              *-------------------------------------------------*
           if        w-ops-tlu            not  = "02"
                     go to inc-upn-999.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory :   *
      *              * uscita immediata                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        o-sts                not  < "01" and
                     o-sts                not  > "99"
                     go to inc-upn-999.
       inc-upn-300.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [upn]                 *
      *              *-------------------------------------------------*
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
       inc-upn-400.
      *              *-------------------------------------------------*
      *              * Open [upn]                                      *
      *              *-------------------------------------------------*
           open      i-o    upn                                       .
       inc-upn-500.
      *              *-------------------------------------------------*
      *              * Lettura record [upn]                            *
      *              *-------------------------------------------------*
       inc-upn-525.
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      "RDCG"               to   upn-tre                .
           move      "            "       to   upn-kre                .
       inc-upn-550.
      *                  *---------------------------------------------*
      *                  * Lettura con lock, e deviazione a seconda    *
      *                  * dell'esito della lettura                    *
      *                  *---------------------------------------------*
           read      upn    invalid key
                            go to   inc-upn-575.
           go to     inc-upn-600.
       inc-upn-575.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Numero progressivo utente in uso : a 1  *
      *                      *-----------------------------------------*
           move      1                    to   x-npu                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     inc-upn-900.
       inc-upn-600.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad aggiornamento [upn]                  *
      *                      *-----------------------------------------*
           go to     inc-upn-700.
       inc-upn-700.
      *              *-------------------------------------------------*
      *              * Aggiornamento [upn]                             *
      *              *-------------------------------------------------*
       inc-upn-725.
      *                  *---------------------------------------------*
      *                  * Incremento numero progressivo               *
      *                  *---------------------------------------------*
           if        upn-prg              <    9999
                     add   1              to   upn-prg                .
      *                  *---------------------------------------------*
      *                  * Numero progressivo utente in uso : pari al  *
      *                  * valore incrementato                         *
      *                  *---------------------------------------------*
           move      upn-prg              to   x-npu                  .
       inc-upn-750.
      *                  *---------------------------------------------*
      *                  * Riscrittura record [upn], e deviazione a    *
      *                  * seconda dell'esito                          *
      *                  *---------------------------------------------*
           rewrite   upn-rec  invalid key
                              go to   inc-upn-775.
           go to     inc-upn-800.
       inc-upn-775.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record [upn] errata          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     inc-upn-500.
       inc-upn-800.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record [upn] a buon fine     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     inc-upn-900.
       inc-upn-900.
      *              *-------------------------------------------------*
      *              * Close [upn]                                     *
      *              *-------------------------------------------------*
           close     upn                                              .
       inc-upn-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'all-str-asx-000/999'                                     *
      *    * 'all-str-adx-000/999'                                     *
      *    * 'all-str-cen-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'allineamento di una stringa a sx o dx o al *
      *    * centro                                                    *
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Valore della stringa da allinea- *
      *    *                          re                               *
      *    *                                                           *
      *    * Output : w-all-str-alf = Valore della stringa allineata   *
      *    *                          a sinistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'all-str-cat-000/999' - (senza uno spazio di separazione) *
      *    * 'all-str-csb-000/999' - (con uno spazio di separazione)   *
      *    *                                                           *
      *    * Routines per il concatenamento di max 10 stringhe di max  *
      *    * 80 caratteri ciascuna con o senza uno spazio di separa-   *
      *    * zione tra una stringa e l'altra                           *
      *    *                                                           *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa concatenata          *
      *    *                                                           *
      *    *          w-all-str-num     = Numero delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    *          w-all-str-cat (i) = Valore delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Valore della stringa con-    *
      *    *                              catenata, allineata a si-    *
      *    *                              nistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

