       Identification Division.
       Program-Id.                                 mops04             .
      *================================================================*
      *                                                                *
      * MODULO OBSOLETO NON PIU' UTILIZZATO !!!                        *
      *                                                                *
      *================================================================*
      *                                                                *
      * Modulo di interfacciamento con sistema operativo ospite        *
      *                                                                *
      * Interfaccia per : MS-DOS                                       *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti : Vedere modulo 'mopsys'              *
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
      *    * File Control [bat]                                        *
      *    *-----------------------------------------------------------*
           select            bat   assign to print          z-bat-pat
                             file  status is                z-bat-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [bat]                                    *
      *    *-----------------------------------------------------------*
       fd  bat  label record omitted.
       01  bat-rec.
           05  bat-chr        occurs  220 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Informazioni mantenute internamente dal programma         *
      *    *-----------------------------------------------------------*
       01  z.
      *        *-------------------------------------------------------*
      *        * Unita' disco su cui si trova la directory \abd, cosi' *
      *        * come letto dalla variabile di ambiente %DSKABD%       *
      *        *                                                       *
      *        * Puo' assumere il valore "C:" o "D:" ecc. ecc.         *
      *        *-------------------------------------------------------*
           05  z-dsk-abd                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Unita' disco su cui si trova il sistema e i comandi,  *
      *        * cosi' come letto dalla variabile di ambiente %DSKSYS% *
      *        *                                                       *
      *        * Puo' assumere il valore "C:" o "D:" ecc. ecc.         *
      *        *                                                       *
      *        * Se non definita assume il valore di %DSKABD%          *
      *        *-------------------------------------------------------*
           05  z-dsk-sys                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Area ausiliaria per controllo i-o su [bat]            *
      *        *-------------------------------------------------------*
           05  z-bat.
      *            *---------------------------------------------------*
      *            * File name                                         *
      *            *---------------------------------------------------*
               10  z-bat-nam              pic  x(04) value "bat "     .
      *            *---------------------------------------------------*
      *            * File pathname                                     *
      *            *---------------------------------------------------*
               10  z-bat-pat              pic  x(60) value spaces     .
      *            *---------------------------------------------------*
      *            * File status                                       *
      *            *---------------------------------------------------*
               10  z-bat-sts              pic  x(02) value "00"       .
      *        *-------------------------------------------------------*
      *        * Valori relativi all'environment                       *
      *        *-------------------------------------------------------*
           05  z-var-env.
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_HOME                            *
      *            *---------------------------------------------------*
               10  z-var-env-hom          pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_USER                            *
      *            *---------------------------------------------------*
               10  z-var-env-use          pic  x(08) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_TTYC                            *
      *            *---------------------------------------------------*
               10  z-var-env-tty          pic  x(08) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_TERM                            *
      *            *---------------------------------------------------*
               10  z-var-env-ter          pic  x(08) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_RUNT                            *
      *            *---------------------------------------------------*
               10  z-var-env-run          pic  x(02) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_GETP, componente PFIX relativa  *
      *            * al prefisso unico per files temporanei            *
      *            *---------------------------------------------------*
               10  z-var-env-pfi          pic  x(20) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_GETP, componente HOID relativa  *
      *            * all' Host-Id                                      *
      *            *---------------------------------------------------*
               10  z-var-env-hoi          pic  x(20) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_SUBT                            *
      *            *---------------------------------------------------*
               10  z-var-env-sub          pic  x(04) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_AZIE                            *
      *            *---------------------------------------------------*
               10  z-var-env-azi          pic  x(04) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_UTEN                            *
      *            *---------------------------------------------------*
               10  z-var-env-ute          pic  x(08) value spaces     .
      *            *---------------------------------------------------*
      *            * Variabile V_ALTER_PRMS                            *
      *            *---------------------------------------------------*
               10  z-var-env-prm          pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Livello di indentazione desk-accessory            *
      *            *---------------------------------------------------*
               10  z-var-env-lid          pic  x(02) value spaces     .
      *        *-------------------------------------------------------*
      *        * Valori passati dal modulo di segreteria               *
      *        *-------------------------------------------------------*
           05  z-mod-seg.
      *            *---------------------------------------------------*
      *            * Codice azienda in uso                             *
      *            *---------------------------------------------------*
               10  z-mod-seg-azi          pic  x(04) value spaces     .
      *            *---------------------------------------------------*
      *            * Codice utente in uso                              *
      *            *---------------------------------------------------*
               10  z-mod-seg-ute          pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Work per composizione pathname in \abd                *
      *        *-------------------------------------------------------*
           05  z-pat-abd.
      *            *---------------------------------------------------*
      *            * Numero componenti del pathname                    *
      *            *---------------------------------------------------*
               10  z-pat-abd-nco          pic  9(02) value zero       .
      *            *---------------------------------------------------*
      *            * Tabella componenti del pathname                   *
      *            *---------------------------------------------------*
               10  z-pat-abd-tbl.
                   15  z-pat-abd-ele occurs 10
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Comodo per composizione pathname                  *
      *            *---------------------------------------------------*
               10  z-pat-abd-pat          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatori, indici, puntatori                      *
      *            *---------------------------------------------------*
               10  z-pat-abd-i01          pic  9(02)                  .
               10  z-pat-abd-c01          pic  9(02)                  .
               10  z-pat-abd-p01          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per trasformazioni in uppercase                  *
      *        *-------------------------------------------------------*
           05  z-upp-cas.
      *            *---------------------------------------------------*
      *            * Valore da rendere in uppercase                    *
      *            *---------------------------------------------------*
               10  z-upp-cas-val.
                   15  z-upp-cas-chr
                              occurs  40  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  z-upp-cas-c01          pic  9(02)                  .
               10  z-upp-cas-c02          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tabella caratteri in lowercase                    *
      *            *---------------------------------------------------*
               10  z-upp-cas-low          pic  x(26)       value
                     "abcdefghijklmnopqrstuvwxyz"                     .
      *            *---------------------------------------------------*
      *            * Tabella caratteri in uppercase                    *
      *            *---------------------------------------------------*
               10  z-upp-cas-upp          pic  x(26)       value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
      *        *-------------------------------------------------------*
      *        * Work per chiamate alla shell di sistema               *
      *        *-------------------------------------------------------*
           05  z-sys-shl.
      *            *---------------------------------------------------*
      *            * Pathname unico temporaneo per esecuzione dei fi-  *
      *            * les .bat automatici per le chiamate al sistema    *
      *            *---------------------------------------------------*
               10  z-sys-shl-put-bau      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Comodo per la chiamata al sistema senza parametri *
      *            * e per la costruzione del comando 'change directo- *
      *            * ri di ripristino al rientro di una chiamata al    *
      *            * sistema                                           *
      *            *---------------------------------------------------*
               10  z-sys-shl-com-001      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Parametro da passare alla chiamata di sistema     *
      *            *---------------------------------------------------*
               10  z-sys-shl-par.
      *                *-----------------------------------------------*
      *                * Comando che deve essere eseguito dalla shell  *
      *                * di sistema                                    *
      *                *-----------------------------------------------*
                   15  z-sys-shl-par-cds.
                       20  filler  occurs 220
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag indicatore se la chiamata al sistema esegue  *
      *            * oppure no input-output sul terminale              *
      *            *  - Spaces : No                                    *
      *            *  - S      : Si                                    *
      *            *---------------------------------------------------*
               10  z-sys-shl-iot          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Parametro indicatore per no i-o su terminale      *
      *            *---------------------------------------------------*
               10  z-sys-shl-nio          pic  x(05) value "NO-IO"    .
      *            *---------------------------------------------------*
      *            * Contatori, indici, pointers locali                *
      *            *---------------------------------------------------*
               10  z-sys-shl-c01          pic  9(03)                  .
               10  z-sys-shl-p01          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggi                                   *
      *        *-------------------------------------------------------*
           05  z-sav.
      *            *---------------------------------------------------*
      *            * Salvataggio per nome procedura batch              *
      *            *---------------------------------------------------*
               10  z-sav-npb              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per ispezione pathname                           *
      *        *-------------------------------------------------------*
           05  z-pth.
      *            *---------------------------------------------------*
      *            * Pathname da ispezionare                           *
      *            *---------------------------------------------------*
               10  z-pth-pat-nam.
                   15  z-pth-pat-chr
                               occurs 40  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Base name                                         *
      *            *---------------------------------------------------*
               10  z-pth-bas-nam          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * File name                                         *
      *            *---------------------------------------------------*
               10  z-pth-fil-nam          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatori, indici, pointers locali                *
      *            *---------------------------------------------------*
               10  z-pth-pth-i01          pic  9(02)                  .
               10  z-pth-pth-i02          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per controllo di tasti funzione asincroni        *
      *        *-------------------------------------------------------*
           05  z-afk.
      *            *---------------------------------------------------*
      *            * Numero di tasti funzione                          *
      *            *---------------------------------------------------*
               10  z-afk-num-fnk          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tabella dei tasti funzione                        *
      *            *---------------------------------------------------*
               10  z-afk-tbl-fnk.
      *                *-----------------------------------------------*
      *                * Elementi della tabelle                        *
      *                *-----------------------------------------------*
                   15  z-afk-ele-fnk
                               occurs 10  pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Tabella delle exceptions relative ai tasti        *
      *            *---------------------------------------------------*
               10  z-afk-tbl-exc.
      *                *-----------------------------------------------*
      *                * Elementi della tabelle                        *
      *                *-----------------------------------------------*
                   15  z-afk-ele-exc
                               occurs 10  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Indice sulle tabelle                              *
      *            *---------------------------------------------------*
               10  z-afk-inx-fnk          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Carattere da funzione di accettazione             *
      *            *---------------------------------------------------*
               10  z-afk-chr-acc          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Exception code da funzione di accettazione        *
      *            *---------------------------------------------------*
               10  z-afk-exc-acc          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Risultato del Test Input Status                   *
      *            *---------------------------------------------------*
               10  z-afk-ris-tis          pic  9(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * System information                                        *
      *    *-----------------------------------------------------------*
       01  w-sys-inf.
      *        *-------------------------------------------------------*
      *        * Operating system                                      *
      *        * -    Unix-V                                           *
      *        * -    Unix-4                                           *
      *        * -    UNOS                                             *
      *        * -    VAX/VMS                                          *
      *        * -    MS-DOS                                           *
      *        * -    WINDOWS                                          *
      *        * -    OS/2                                             *
      *        * -    AOS/VS                                           *
      *        *-------------------------------------------------------*
           05  w-sys-inf-ope-sys          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * User-Id                                               *
      *        *-------------------------------------------------------*
           05  w-sys-inf-usr-ide          pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Station-Id                                            *
      *        *-------------------------------------------------------*
           05  w-sys-inf-sta-ide          pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Has indexed read previous                             *
      *        *-------------------------------------------------------*
           05  w-sys-inf-has-irp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Has relative read previous                            *
      *        *-------------------------------------------------------*
           05  w-sys-inf-has-rrp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Can test input status                                 *
      *        *-------------------------------------------------------*
           05  w-sys-inf-can-tis          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Is multi-tasking                                      *
      *        *-------------------------------------------------------*
           05  w-sys-inf-mul-tsk          pic  x(01)                  .

      ******************************************************************
       Procedure Division                using o
                                               w-sys-inf              .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Esecuzione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Componente per composizione pathname        *
      *                  *---------------------------------------------*
           if        o-ope                =    "A5"
                     perform fun-xa5-000  thru fun-xa5-999
      *                  *---------------------------------------------*
      *                  * Inizializzazione composizione pathname      *
      *                  *---------------------------------------------*
           else if   o-ope                =    "A0"
                     perform fun-xa0-000  thru fun-xa0-999
      *                  *---------------------------------------------*
      *                  * Ottenimento pathname completo sotto \abd    *
      *                  *---------------------------------------------*
           else if   o-ope                =    "A9"
                     perform fun-xa9-000  thru fun-xa9-999
      *                  *---------------------------------------------*
      *                  * Concatenamento componenti per ottenimento   *
      *                  * di un pathname                              *
      *                  *---------------------------------------------*
           else if   o-ope                =    "C9"
                     perform fun-xc9-000  thru fun-xc9-999
      *                  *---------------------------------------------*
      *                  * Richiesta di test se richiesta interruzione *
      *                  * di stampa                                   *
      *                  *---------------------------------------------*
           else if   o-ope                =    "<>"
                     perform tst-ids-000  thru tst-ids-999
      *                  *---------------------------------------------*
      *                  * Richiesta di test se pressione asincrona    *
      *                  * multipla di tasti funzione da operatore     *
      *                  *---------------------------------------------*
           else if   o-ope                =    "+K"
                     perform tsq-afk-000  thru tsq-afk-999
      *                  *---------------------------------------------*
      *                  * Richiesta di test se pressione asincrona    *
      *                  * singola di tasto funzione da operatore      *
      *                  *---------------------------------------------*
           else if   o-ope                =    "=K"
                     perform tps-afk-000  thru tps-afk-999
      *                  *---------------------------------------------*
      *                  * Rimozione di tutti i files contenuti in una *
      *                  * subdirectory di \abd                        *
      *                  *---------------------------------------------*
           else if   o-ope                =    "R9"
                     perform fun-ras-000  thru fun-ras-999
      *                  *---------------------------------------------*
      *                  * Richiesta di ammissibilita' background      *
      *                  *---------------------------------------------*
           else if   o-ope                =    "BF"
                     perform snx-bkg-000  thru snx-bkg-999
      *                  *---------------------------------------------*
      *                  * Richiesta di ammissibilita' stampa in spool *
      *                  *---------------------------------------------*
           else if   o-ope                =    "SS"
                     perform snx-ssp-000  thru snx-ssp-999
      *                  *---------------------------------------------*
      *                  * Richiesta di esecuzione in background       *
      *                  *---------------------------------------------*
           else if   o-ope                =    "BK"
                     perform exe-bkg-000  thru exe-bkg-999
      *                  *---------------------------------------------*
      *                  * Richiesta di esecuzione desk-accessories    *
      *                  *---------------------------------------------*
           else if   o-ope                =    "DA"
                     perform exe-dac-000  thru exe-dac-999
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           else if   o-ope                =    "LD"
                     perform liv-dac-000  thru liv-dac-999
      *                  *---------------------------------------------*
      *                  * Richiesta tipo di licenza d'uso             *
      *                  *---------------------------------------------*
           else if   o-ope                =    "LU"
                     perform tip-lus-000  thru tip-lus-999
      *                  *---------------------------------------------*
      *                  * Separazione tra basename e filename         *
      *                  *---------------------------------------------*
           else if   o-ope                =    "BN"
                     perform bna-fna-000  thru bna-fna-999
      *                  *---------------------------------------------*
      *                  * Riunione basename e filename in pathname    *
      *                  *---------------------------------------------*
           else if   o-ope                =    "PN"
                     perform pat-nam-000  thru pat-nam-999
      *                  *---------------------------------------------*
      *                  * Copy File by Pathname                       *
      *                  *---------------------------------------------*
           else if   o-ope                =    "CP"
                     perform cpy-pth-000  thru cpy-pth-999
      *                  *---------------------------------------------*
      *                  * Delete File by Pathname                     *
      *                  *---------------------------------------------*
           else if   o-ope                =    "DP"
                     perform del-pth-000  thru del-pth-999
      *                  *---------------------------------------------*
      *                  * Rename File by Pathname                     *
      *                  *---------------------------------------------*
           else if   o-ope                =    "RP"
                     perform ren-pth-000  thru ren-pth-999
      *                  *---------------------------------------------*
      *                  * Informazioni su filename ed estensione      *
      *                  *---------------------------------------------*
           else if   o-ope                =    "EX"
                     perform inf-fxt-000  thru inf-fxt-999
      *                  *---------------------------------------------*
      *                  * Tipi di shell richiamabili                  *
      *                  *---------------------------------------------*
           else if   o-ope                =    "TS"
                     perform tip-she-000  thru tip-she-999
      *                  *---------------------------------------------*
      *                  * Richiesta di esecuzione di una shell inte-  *
      *                  * rattiva, per poi rientrare in programma     *
      *                  *---------------------------------------------*
           else if   o-ope                =    "SH"
                     perform she-int-000  thru she-int-999
      *                  *---------------------------------------------*
      *                  * Richiesta di esecuzione di una procedura    *
      *                  * batch standard                              *
      *                  *---------------------------------------------*
           else if   o-ope                =    "XB"
                     perform bat-std-000  thru bat-std-999
      *                  *---------------------------------------------*
      *                  * Richiesta di esecuzione di un comando con-  *
      *                  * tenuto in \abd                              *
      *                  *---------------------------------------------*
           else if   o-ope                =    "X9"
                     perform com-abd-000  thru com-abd-999
      *                  *---------------------------------------------*
      *                  * Richiesta di una 'Call Unix', ovvero di una *
      *                  * chiamata diretta al sistema, senza filtri   *
      *                  * da parte del modulo di interfaccia          *
      *                  *---------------------------------------------*
           else if   o-ope                =    "CU"
                     perform cll-unx-000  thru cll-unx-999
      *                  *---------------------------------------------*
      *                  * Dichiarazione codice azienda                *
      *                  *---------------------------------------------*
           else if   o-ope                =    "AZ"
                     perform dic-azi-000  thru dic-azi-999
      *                  *---------------------------------------------*
      *                  * Dichiarazione codice utente                 *
      *                  *---------------------------------------------*
           else if   o-ope                =    "UT"
                     perform dic-ute-000  thru dic-ute-999
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           else if   o-ope                =    "OP"
                     perform opn-ops-000  thru opn-ops-999
      *                  *---------------------------------------------*
      *                  * Completamento open modulo                   *
      *                  *---------------------------------------------*
           else if   o-ope                =    "O2"
                     perform op2-ops-000  thru op2-ops-999
      *                  *---------------------------------------------*
      *                  * Richiesta valore parametro di completamento *
      *                  *---------------------------------------------*
           else if   o-ope                =    "I2"
                     perform ri2-ops-000  thru ri2-ops-999
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           else if   o-ope                =    "CL"
                     perform cls-ops-000  thru cls-ops-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *----------------------------------------------------------------*

      *    *===========================================================*
      *    * Open modulo                                               *
      *    *-----------------------------------------------------------*
       opn-ops-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella function keys asincrone *
      *              *-------------------------------------------------*
           move      zero                 to   z-afk-num-fnk          .
           move      spaces               to   z-afk-tbl-fnk          .
           move      all   "0"            to   z-afk-tbl-exc          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di ambiente %DSKABD%    *
      *              * per l'unita' disco su cui si trova la directory *
      *              * \abd, e memorizzazione del valore               *
      *              *-------------------------------------------------*
           accept    z-dsk-abd            from environment "DSKABD"   .
           if        z-dsk-abd            =    spaces
                     move  "c:"           to   z-dsk-abd              .
       opn-ops-100.
      *              *-------------------------------------------------*
      *              * Lettura della variabile di ambiente %DSKSYS%    *
      *              * per l'unita' disco su cui si trova il sistema   *
      *              * e i comandi, e memorizzazione del valore        *
      *              *-------------------------------------------------*
           accept    z-dsk-sys            from environment "DSKSYS"   .
           if        z-dsk-sys            =    spaces
                     move  z-dsk-abd      to   z-dsk-sys              .
       opn-ops-200.
      *              *-------------------------------------------------*
      *              * Preparazione di un pathname unico temporaneo    *
      *              * per l'esecuzione dei files .bat automatici per  *
      *              * le chiamate al sistema                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Simulazione funzione A0                     *
      *                  *---------------------------------------------*
           perform   fun-xa0-000          thru fun-xa0-999            .
      *                  *---------------------------------------------*
      *                  * Simulazione funzione A5 per 'tmp'           *
      *                  *---------------------------------------------*
           move      "tmp"                to   o-com                  .
           perform   fun-xa5-000          thru fun-xa5-999            .
      *                  *---------------------------------------------*
      *                  * Simulazione funzione A5 per il nome del fi- *
      *                  * le temporaneo con suffisso '.bat'           *
      *                  *---------------------------------------------*
           move      "batchsys.bat"       to   o-com                  .
           perform   fun-xa5-000          thru fun-xa5-999            .
      *                  *---------------------------------------------*
      *                  * Simulazione funzione A9                     *
      *                  *---------------------------------------------*
           perform   fun-xa9-000          thru fun-xa9-999            .
      *                  *---------------------------------------------*
      *                  * Memorizzazione pathname                     *
      *                  *---------------------------------------------*
           move      o-pat                to   z-sys-shl-put-bau      .
       opn-ops-999.
           exit.

      *    *===========================================================*
      *    * Completamento open modulo                                 *
      *    *-----------------------------------------------------------*
       op2-ops-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione del valore della variabile di    *
      *              * environment passata                             *
      *              *-------------------------------------------------*
           if        o-com                =    "HOME"
                     move  o-pat          to   z-var-env-hom
           else if   o-com                =    "USER"
                     move  o-pat          to   z-var-env-use
           else if   o-com                =    "TTYC"
                     move  o-pat          to   z-var-env-tty
           else if   o-com                =    "TERM"
                     move  o-pat          to   z-var-env-ter
           else if   o-com                =    "RUNT"
                     move  o-pat          to   z-var-env-run
           else if   o-com                =    "PFIX"
                     move  o-pat          to   z-var-env-pfi
                     move  "00"           to   z-var-env-lid
           else if   o-com                =    "HOID"
                     move  o-pat          to   z-var-env-hoi
           else if   o-com                =    "SUBT"
                     move  o-pat          to   z-var-env-sub
           else if   o-com                =    "AZIE"
                     move  o-pat          to   z-var-env-azi
           else if   o-com                =    "UTEN"
                     move  o-pat          to   z-var-env-ute
           else if   o-com                =    "PRMS"
                     move  o-pat          to   z-var-env-prm          .
       op2-ops-999.
           exit.

      *    *===========================================================*
      *    * Richiesta valore parametro di completamento open modulo   *
      *    *-----------------------------------------------------------*
       ri2-ops-000.
      *              *-------------------------------------------------*
      *              * Estrazione del valore della variabile di envi-  *
      *              * ronment passata al momento della "O2"           *
      *              *-------------------------------------------------*
           if        o-com                =    "HOME"
                     move  z-var-env-hom  to   o-pat
           else if   o-com                =    "USER"
                     move  z-var-env-use  to   o-pat
           else if   o-com                =    "TTYC"
                     move  z-var-env-tty  to   o-pat
           else if   o-com                =    "TERM"
                     move  z-var-env-ter  to   o-pat
           else if   o-com                =    "RUNT"
                     move  z-var-env-run  to   o-pat
           else if   o-com                =    "PFIX"
                     move  z-var-env-pfi  to   o-pat
           else if   o-com                =    "HOID"
                     move  z-var-env-hoi  to   o-pat
           else if   o-com                =    "SUBT"
                     move  z-var-env-sub  to   o-pat
           else if   o-com                =    "AZIE"
                     move  z-var-env-azi  to   o-pat
           else if   o-com                =    "UTEN"
                     move  z-var-env-ute  to   o-pat
           else if   o-com                =    "PRMS"
                     move  z-var-env-prm  to   o-pat
           else      move  spaces         to   o-pat                  .
       ri2-ops-999.
           exit.

      *    *===========================================================*
      *    * Close modulo                                              *
      *    *-----------------------------------------------------------*
       cls-ops-000.
       cls-ops-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice azienda                              *
      *    *-----------------------------------------------------------*
       dic-azi-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione codice azienda                   *
      *              *-------------------------------------------------*
           move      o-com                to   z-mod-seg-azi          .
       dic-azi-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice utente                               *
      *    *-----------------------------------------------------------*
       dic-ute-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione codice utente                    *
      *              *-------------------------------------------------*
           move      o-com                to   z-mod-seg-ute          .
       dic-ute-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione composizione pathname di \abd            *
      *    *-----------------------------------------------------------*
       fun-xa0-000.
      *              *-------------------------------------------------*
      *              * Numero componenti a zero                        *
      *              *-------------------------------------------------*
           move      zero                 to   z-pat-abd-nco          .
      *              *-------------------------------------------------*
      *              * Tabella componenti del pathname a spaces        *
      *              *-------------------------------------------------*
           move      spaces               to   z-pat-abd-tbl          .
       fun-xa0-999.
           exit.

      *    *===========================================================*
      *    * Componente per composizione pathname di \abd              *
      *    *-----------------------------------------------------------*
       fun-xa5-000.
      *              *-------------------------------------------------*
      *              * Se gia' memorizzato il massimo numero di compo- *
      *              * nenti : uscita                                  *
      *              *-------------------------------------------------*
           if        z-pat-abd-nco        not  < 10
                     go to fun-xa5-999.
      *              *-------------------------------------------------*
      *              * Se componente a spaces : uscita                 *
      *              *-------------------------------------------------*
           if        o-com                =    spaces
                     go to fun-xa5-999.
      *              *-------------------------------------------------*
      *              * Incremento numero componenti                    *
      *              *-------------------------------------------------*
           add       1                    to   z-pat-abd-nco          .
      *              *-------------------------------------------------*
      *              * Memorizzazione componente in tabella            *
      *              *-------------------------------------------------*
           move      o-com                to   z-pat-abd-ele
                                              (z-pat-abd-nco)         .
       fun-xa5-999.
           exit.

      *    *===========================================================*
      *    * Ottenimento pathname completo con locazione sotto \abd    *
      *    *-----------------------------------------------------------*
       fun-xa9-000.
      *              *-------------------------------------------------*
      *              * Se numero componenti memorizzati pari a zero :  *
      *              * uscita con valore a spaces                      *
      *              *-------------------------------------------------*
           if        z-pat-abd-nco        =    zero
                     move  spaces         to   o-pat
                     go to fun-xa9-999.
       fun-xa9-100.
      *              *-------------------------------------------------*
      *              * Indice su componente da comporre a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   z-pat-abd-i01          .
      *              *-------------------------------------------------*
      *              * Pathname risultante pari al prefisso            *
      *              *-------------------------------------------------*
           move      spaces               to   z-pat-abd-pat          .
           string    z-dsk-abd  delimited by   size
                     "\abd"     delimited by size
                                          into z-pat-abd-pat          .
       fun-xa9-200.
      *              *-------------------------------------------------*
      *              * Incremento indice su componente da comporre     *
      *              *-------------------------------------------------*
           add       1                    to   z-pat-abd-i01          .
      *              *-------------------------------------------------*
      *              * Se maggiore del numero di componenti memorizza- *
      *              * ti : fine composizione                          *
      *              *-------------------------------------------------*
           if        z-pat-abd-i01        >    z-pat-abd-nco
                     go to fun-xa9-300.
      *              *-------------------------------------------------*
      *              * Determinazione del valore del pointer sul ca-   *
      *              * rattere successivo all'ultimo carattere si-     *
      *              * gnificativo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   z-pat-abd-c01          .
           inspect   z-pat-abd-pat    tallying z-pat-abd-c01
                                  for trailing spaces                 .
           subtract  z-pat-abd-c01        from 40
                                        giving z-pat-abd-p01          .
           add       1                    to   z-pat-abd-p01          .
      *              *-------------------------------------------------*
      *              * Se pointer non minore del massimo : fine com-   *
      *              * posizione. Cio' lascia spazio sicuramente per   *
      *              * un carattere di separazione e per almeno un     *
      *              * carattere del componente successivo             *
      *              *-------------------------------------------------*
           if        z-pat-abd-p01        not  < 40
                     go to fun-xa9-300.
      *              *-------------------------------------------------*
      *              * Composizione del carattere di separazione       *
      *              *-------------------------------------------------*
           string    "\"        delimited by   size
                                          into z-pat-abd-pat
                                  with pointer z-pat-abd-p01          .
      *              *-------------------------------------------------*
      *              * Composizione del componente                     *
      *              *-------------------------------------------------*
           string    z-pat-abd-ele
                    (z-pat-abd-i01)
                                delimited by   size
                                          into z-pat-abd-pat
                                  with pointer z-pat-abd-p01          .
      *              *-------------------------------------------------*
      *              * Riciclo su componente successivo                *
      *              *-------------------------------------------------*
           go to     fun-xa9-200.
       fun-xa9-300.
      *              *-------------------------------------------------*
      *              * Uppercase del pathname risultante               *
      *              *                                                 *
      *              * Nota : Funzione inibita                         *
      *              *-------------------------------------------------*
      *    move      z-pat-abd-pat        to   z-upp-cas-val          .
      *    perform   tra-val-upp-000      thru tra-val-upp-999        .
      *    move      z-upp-cas-val        to   z-pat-abd-pat          .
      *              *-------------------------------------------------*
      *              * Pathname risultante in uscita                   *
      *              *-------------------------------------------------*
           move      z-pat-abd-pat        to   o-pat                  .
       fun-xa9-999.
           exit.

      *    *===========================================================*
      *    * Concatenamento componenti pathname                        *
      *    *-----------------------------------------------------------*
       fun-xc9-000.
      *              *-------------------------------------------------*
      *              * Se numero componenti memorizzati pari a zero :  *
      *              * uscita con valore a spaces                      *
      *              *-------------------------------------------------*
           if        z-pat-abd-nco        =    zero
                     move  spaces         to   o-pat
                     go to fun-xc9-999.
       fun-xc9-100.
      *              *-------------------------------------------------*
      *              * Pathname risultante pari al valore del primo    *
      *              * componente                                      *
      *              *-------------------------------------------------*
           move      z-pat-abd-ele (1)    to   z-pat-abd-pat          .
      *              *-------------------------------------------------*
      *              * Indice su componente da comporre a 1            *
      *              *-------------------------------------------------*
           move      1                    to   z-pat-abd-i01          .
       fun-xc9-200.
      *              *-------------------------------------------------*
      *              * Incremento indice su componente da comporre     *
      *              *-------------------------------------------------*
           add       1                    to   z-pat-abd-i01          .
      *              *-------------------------------------------------*
      *              * Se maggiore del numero di componenti memorizza- *
      *              * ti : fine composizione                          *
      *              *-------------------------------------------------*
           if        z-pat-abd-i01        >    z-pat-abd-nco
                     go to fun-xc9-300.
      *              *-------------------------------------------------*
      *              * Determinazione del valore del pointer sul ca-   *
      *              * rattere successivo all'ultimo carattere si-     *
      *              * gnificativo                                     *
      *              *-------------------------------------------------*
           move      zero                 to   z-pat-abd-c01          .
           inspect   z-pat-abd-pat    tallying z-pat-abd-c01
                                  for trailing spaces                 .
           subtract  z-pat-abd-c01        from 40
                                        giving z-pat-abd-p01          .
           add       1                    to   z-pat-abd-p01          .
      *              *-------------------------------------------------*
      *              * Se pointer non minore del massimo : fine com-   *
      *              * posizione. Cio' lascia spazio sicuramente per   *
      *              * un carattere di separazione e per almeno un     *
      *              * carattere del componente successivo             *
      *              *-------------------------------------------------*
           if        z-pat-abd-p01        not  < 40
                     go to fun-xc9-300.
      *              *-------------------------------------------------*
      *              * Composizione del carattere di separazione       *
      *              *-------------------------------------------------*
           string    "\"        delimited by   size
                                          into z-pat-abd-pat
                                  with pointer z-pat-abd-p01          .
      *              *-------------------------------------------------*
      *              * Composizione del componente                     *
      *              *-------------------------------------------------*
           string    z-pat-abd-ele
                    (z-pat-abd-i01)
                                delimited by   size
                                          into z-pat-abd-pat
                                  with pointer z-pat-abd-p01          .
      *              *-------------------------------------------------*
      *              * Riciclo su componente successivo                *
      *              *-------------------------------------------------*
           go to     fun-xc9-200.
       fun-xc9-300.
      *              *-------------------------------------------------*
      *              * Uppercase del pathname risultante               *
      *              *                                                 *
      *              * Nota : Funzione inibita                         *
      *              *-------------------------------------------------*
      *    move      z-pat-abd-pat        to   z-upp-cas-val          .
      *    perform   tra-val-upp-000      thru tra-val-upp-999        .
      *    move      z-upp-cas-val        to   z-pat-abd-pat          .
      *              *-------------------------------------------------*
      *              * Pathname risultante in uscita                   *
      *              *-------------------------------------------------*
           move      z-pat-abd-pat        to   o-pat                  .
       fun-xc9-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di test se richiesta interruzione di stampa     *
      *    *-----------------------------------------------------------*
       tst-ids-000.
      *              *-------------------------------------------------*
      *              * Preparazione richiamo routine generica di test  *
      *              * su sequenza di tasti funzione asincroni         *
      *              *-------------------------------------------------*
           move      spaces               to   o-com                  .
           string    "[1] "     delimited by   size
                     "[2] "     delimited by   size
                     "[3] "     delimited by   size
                     "[4] "     delimited by   size
                                          into o-com                  .
      *              *-------------------------------------------------*
      *              * Richiamo della routine                          *
      *              *-------------------------------------------------*
           perform   tsq-afk-000          thru tsq-afk-999            .
       tst-ids-999.
           exit.

      *    *===========================================================*
      *    * Test su sequenza di tasti funzione asincroni              *
      *    *-----------------------------------------------------------*
       tsq-afk-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a No interruzione              *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
      *              *-------------------------------------------------*
      *              * Se il sistema operativo non puo' testare l'in-  *
      *              * put status : uscita                             *
      *              *-------------------------------------------------*
           if        w-sys-inf-can-tis    not  = "Y"
                     go to tsq-afk-999.
       tsq-afk-100.
      *              *-------------------------------------------------*
      *              * Preparazione della tabella dei tasti funzione e *
      *              * delle relative eccezioni                        *
      *              *-------------------------------------------------*
           perform   pxc-afk-000          thru pxc-afk-999            .
      *              *-------------------------------------------------*
      *              * Se non e' stato passato nessun tasto funzione   *
      *              * di tipo riconosciuto : uscita                   *
      *              *-------------------------------------------------*
           if        z-afk-num-fnk        =    zero
                     go to tsq-afk-999.
       tsq-afk-200.
      *              *-------------------------------------------------*
      *              * Indice su tabella tasti funzione a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   z-afk-inx-fnk          .
       tsq-afk-300.
      *              *-------------------------------------------------*
      *              * Test Input Status                               *
      *              *-------------------------------------------------*
           accept    z-afk-ris-tis        from input status           .
      *              *-------------------------------------------------*
      *              * Se non ci sono caratteri in pendenza nel buffer *
      *              * della tastiera : uscita                         *
      *              *-------------------------------------------------*
           if        z-afk-ris-tis        not  = 1
                     go to tsq-afk-999.
       tsq-afk-310.
      *              *-------------------------------------------------*
      *              * Accettazione di un carattere da tastiera, con   *
      *              * le seguenti caratteristiche :                   *
      *              *  - Video in off                                 *
      *              *  - No beep                                      *
      *              *  - Controllo della function key digitata        *
      *              *-------------------------------------------------*
           if        o-mxf                =    zero
                     move  23             to   o-mxf                  .
           if        o-mxe                =    zero
                     move  01             to   o-mxe                  .
           accept    z-afk-chr-acc        line o-mxf
                                      position o-mxe
                                          off
                                          no beep
                                          on exception z-afk-exc-acc
                     go to tsq-afk-400.
      *              *-------------------------------------------------*
      *              * Se non c'e' exception si ricomincia da capo     *
      *              *-------------------------------------------------*
           go to     tsq-afk-200.
       tsq-afk-400.
      *              *-------------------------------------------------*
      *              * Incremento indice su tabella                    *
      *              *-------------------------------------------------*
           add       1                    to   z-afk-inx-fnk          .
      *              *-------------------------------------------------*
      *              * Se l'exception code digitato non corrisponde    *
      *              * a quanto specificato in tabella si ricomincia   *
      *              * da capo                                         *
      *              *-------------------------------------------------*
           if        z-afk-exc-acc        not  = z-afk-ele-exc
                                                (z-afk-inx-fnk)
                     go to tsq-afk-200.
      *              *-------------------------------------------------*
      *              * Se si e' sull'ultima function key da esaminare  *
      *              * si esce con status di uscita a Si interruzione  *
      *              *-------------------------------------------------*
           if        z-afk-inx-fnk        =    z-afk-num-fnk
                     move  "<>"           to   o-sts
                     go to tsq-afk-999.
      *              *-------------------------------------------------*
      *              * Test Input Status                               *
      *              *-------------------------------------------------*
           accept    z-afk-ris-tis        from input status           .
      *              *-------------------------------------------------*
      *              * Se ci sono caratteri in pendenza nel buffer     *
      *              * della tastiera : riciclo immediato              *
      *              *-------------------------------------------------*
           if        z-afk-ris-tis        =    1
                     go to tsq-afk-310.
      *              *-------------------------------------------------*
      *              * Attesa di circa 3 secondi per dar tempo all'o-  *
      *              * peratore di digitare il tasto funzione succes-  *
      *              * sivo                                            *
      *              *-------------------------------------------------*
           call      "C$SLEEP"           using "3"                    .
      *              *-------------------------------------------------*
      *              * Si ricicla per la function key successiva       *
      *              *-------------------------------------------------*
           go to     tsq-afk-300.
       tsq-afk-999.
           exit.

      *    *===========================================================*
      *    * Test su singolo tasto funzione asincrono                  *
      *    *-----------------------------------------------------------*
       tps-afk-000.
      *              *-------------------------------------------------*
      *              * Se il sistema operativo non puo' testare l'in-  *
      *              * put status : uscita con esito negativo          *
      *              *-------------------------------------------------*
           if        w-sys-inf-can-tis    not  = "Y"
                     move  spaces         to   o-com
                     go to tps-afk-999.
      *              *-------------------------------------------------*
      *              * Preparazione della tabella dei tasti funzione e *
      *              * delle relative eccezioni                        *
      *              *-------------------------------------------------*
           perform   pxc-afk-000          thru pxc-afk-999            .
      *              *-------------------------------------------------*
      *              * Se non e' stato passato nessun tasto funzione   *
      *              * di tipo riconosciuto : uscita con esito negati- *
      *              * vo                                              *
      *              *-------------------------------------------------*
           if        z-afk-num-fnk        =    zero
                     move  spaces         to   o-com
                     go to tps-afk-999.
      *              *-------------------------------------------------*
      *              * Test Input Status                               *
      *              *-------------------------------------------------*
           accept    z-afk-ris-tis        from input status           .
      *              *-------------------------------------------------*
      *              * Se non ci sono caratteri in pendenza nel buffer *
      *              * della tastiera : uscita con esito negativo      *
      *              *-------------------------------------------------*
           if        z-afk-ris-tis        not  = 1
                     move  spaces         to   o-com
                     go to tps-afk-999.
      *              *-------------------------------------------------*
      *              * Accettazione di un carattere da tastiera, con   *
      *              * le seguenti caratteristiche :                   *
      *              *  - Video in off                                 *
      *              *  - No beep                                      *
      *              *  - Controllo della function key digitata        *
      *              *-------------------------------------------------*
           if        o-mxf                =    zero
                     move  23             to   o-mxf                  .
           if        o-mxe                =    zero
                     move  01             to   o-mxe                  .
           accept    z-afk-chr-acc        line o-mxf
                                      position o-mxe
                                          off
                                          no beep
                                          on exception z-afk-exc-acc
                     go to tps-afk-200.
      *              *-------------------------------------------------*
      *              * Se non c'e' exception : uscita con esito nega-  *
      *              * tivo                                            *
      *              *-------------------------------------------------*
           move      spaces               to   o-com                  .
           go to     tps-afk-999.
       tps-afk-200.
      *              *-------------------------------------------------*
      *              * Indice su tabella tasti funzione a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   z-afk-inx-fnk          .
       tps-afk-300.
      *              *-------------------------------------------------*
      *              * Incremento indice su tabella                    *
      *              *-------------------------------------------------*
           add       1                    to   z-afk-inx-fnk          .
      *              *-------------------------------------------------*
      *              * Se oltre l'ultimo elemento memorizzato : uscita *
      *              * con esito negativo                              *
      *              *-------------------------------------------------*
           if        z-afk-inx-fnk        >    z-afk-num-fnk
                     move  spaces         to   o-com
                     go to tps-afk-999.
      *              *-------------------------------------------------*
      *              * Se l'exception code digitato non corrisponde    *
      *              * a quanto specificato in tabella si riprova con  *
      *              * l'elemento memorizzato successivo               *
      *              *-------------------------------------------------*
           if        z-afk-exc-acc        not  = z-afk-ele-exc
                                                (z-afk-inx-fnk)
                     go to tps-afk-300.
      *              *-------------------------------------------------*
      *              * In caso contrario si esce con la function key   *
      *              * trovata                                         *
      *              *-------------------------------------------------*
           move      z-afk-ele-fnk
                    (z-afk-inx-fnk)       to   o-com                  .
       tps-afk-999.
           exit.

      *    *===========================================================*
      *    * Preparazione exception codes in tabella tasti funzione    *
      *    *-----------------------------------------------------------*
       pxc-afk-000.
      *              *-------------------------------------------------*
      *              * Se la tabella passata come parametro e' pari    *
      *              * all'attuale area di ridefinizione : uscita im-  *
      *              * mediata senza ulteriore azione                  *
      *              *-------------------------------------------------*
           if        o-com                =    z-afk-tbl-fnk
                     go to pxc-afk-999.
      *              *-------------------------------------------------*
      *              * Tabella passata come parametro in area per la   *
      *              * ridefinizione                                   *
      *              *-------------------------------------------------*
           move      o-com                to   z-afk-tbl-fnk          .
       pxc-afk-100.
      *              *-------------------------------------------------*
      *              * Numero di tasti funzione presenti : a zero      *
      *              *-------------------------------------------------*
           move      zero                 to   z-afk-num-fnk          .
      *              *-------------------------------------------------*
      *              * Indice scansione tabella : a zero               *
      *              *-------------------------------------------------*
           move      zero                 to   z-afk-inx-fnk          .
       pxc-afk-200.
      *              *-------------------------------------------------*
      *              * Incremento indice scansione tabella             *
      *              *-------------------------------------------------*
           add       1                    to   z-afk-inx-fnk          .
      *              *-------------------------------------------------*
      *              * Se superato il max numero elementi: si termina  *
      *              * la scansione                                    *
      *              *-------------------------------------------------*
           if        z-afk-inx-fnk        >    10
                     go to pxc-afk-999.
      *              *-------------------------------------------------*
      *              * Se il tasto funzione in esame e' a spaces : si  *
      *              * termina la scansione                            *
      *              *-------------------------------------------------*
           if        z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    spaces
                     go to pxc-afk-999.
      *              *-------------------------------------------------*
      *              * Se il tasto funzione in esame e' di tipo rico-  *
      *              * nosciuto si incrementa il numero di tasti fun-  *
      *              * zione presenti e si memorizza il codice di ec-  *
      *              * cezione corrispondente, in caso contrario si    *
      *              * termina la scansione                            *
      *              *-------------------------------------------------*
           if        z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "[1] "
                     move  01             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "[2] "
                     move  02             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "[3] "
                     move  03             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "[4] "
                     move  04             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "RFSH"
                     move  06             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "COPY"
                     move  07             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "PAST"
                     move  08             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "EXPD"
                     move  09             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "APND"
                     move  10             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "ICHR"
                     move  11             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "DCHR"
                     move  12             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "BACK"
                     move  14             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "HELP"
                     move  15             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "DO  "
                     move  16             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "DELT"
                     move  17             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "PRNT"
                     move  18             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "EXIT"
                     move  20             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "FIND"
                     move  41             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "INSR"
                     move  42             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "REMV"
                     move  43             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "SLCT"
                     move  44             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "PRSC"
                     move  45             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "NXSC"
                     move  46             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "UP  "
                     move  51             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "DOWN"
                     move  52             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "RGHT"
                     move  53             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "LEFT"
                     move  54             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "RTRN"
                     move  61             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "TAB "
                     move  62             to   z-afk-exc-acc
           else if   z-afk-ele-fnk
                    (z-afk-inx-fnk)       =    "SHCP"
                     move  65             to   z-afk-exc-acc
           else      go to pxc-afk-999.
           add       1                    to   z-afk-num-fnk          .
           move      z-afk-exc-acc        to   z-afk-ele-exc
                                              (z-afk-num-fnk)         .
           go to     pxc-afk-200.
       pxc-afk-999.
           exit.

      *    *===========================================================*
      *    * Rimozione di tutti i files contenuti in una subdirectory  *
      *    * di \abd                                                   *
      *    *-----------------------------------------------------------*
       fun-ras-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione eventuale prefisso in tabella    *
      *              * componenti pathname                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero componenti                *
      *                  *---------------------------------------------*
           add       1                    to   z-pat-abd-nco          .
      *                  *---------------------------------------------*
      *                  * Memorizzazione componente in tabella        *
      *                  *---------------------------------------------*
           move      o-com                to   z-pat-abd-ele
                                              (z-pat-abd-nco)         .
       fun-ras-100.
      *              *-------------------------------------------------*
      *              * Completamento dell'eventuale prefisso in ta-    *
      *              * bella componenti pathname                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del pointer                  *
      *                  *---------------------------------------------*
           move      zero                 to   z-pat-abd-c01          .
           inspect   z-pat-abd-ele
                    (z-pat-abd-nco)   tallying z-pat-abd-c01
                                  for trailing spaces                 .
           subtract  z-pat-abd-c01        from 40
                                        giving z-pat-abd-p01          .
           add       1                    to   z-pat-abd-p01          .
      *                  *---------------------------------------------*
      *                  * Completamento                               *
      *                  *---------------------------------------------*
           string    "*.*"      delimited by   size
                                          into z-pat-abd-ele
                                              (z-pat-abd-nco)
                                  with pointer z-pat-abd-p01          .
       fun-ras-200.
      *              *-------------------------------------------------*
      *              * Richiamo della routine per l'ottenimento in     *
      *              * o-pat del pathname completo con locazione sotto *
      *              * \abd a seconda dei richiami gia' avvenuti di    *
      *              * A0 a A5, arricchiti del suffisso                *
      *              *-------------------------------------------------*
           perform   fun-xa9-000          thru fun-xa9-999            .
       fun-ras-300.
      *              *-------------------------------------------------*
      *              * Composizione del comando da sottoporre alla     *
      *              * chiamata della shell di sistema                 *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-par-cds      .
           string    "DEL "
                                delimited by   size
                     o-pat
                                delimited by   size
                                          into z-sys-shl-par-cds      .
       fun-ras-400.
      *              *-------------------------------------------------*
      *              * Null-redirection per standard output            *
      *              *-------------------------------------------------*
           perform   out-nul-dev-000      thru out-nul-dev-999        .
      *              *-------------------------------------------------*
      *              * Null-redirection per standard error             *
      *              *-------------------------------------------------*
           perform   err-nul-dev-000      thru err-nul-dev-999        .
      *              *-------------------------------------------------*
      *              * Flag indicatore di No i-o su terminale          *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-iot          .
       fun-ras-500.
      *              *-------------------------------------------------*
      *              * Preparazione del pathname unico temporaneo per  *
      *              * l'esecuzione dei files .bat automatici per le   *
      *              * chiamate al sistema                             *
      *              *-------------------------------------------------*
           move      z-sys-shl-put-bau    to   z-bat-pat              .
      *              *-------------------------------------------------*
      *              * Richiamo della shell di sistema                 *
      *              *-------------------------------------------------*
           perform   cll-sys-shl-000      thru cll-sys-shl-999        .
       fun-ras-999.
           exit.

      *    *===========================================================*
      *    * Richiesta sui tipi di shell richiamabili                  *
      *    *-----------------------------------------------------------*
       tip-she-000.
      *              *-------------------------------------------------*
      *              * Sono richiamabili tutti i tipi di shell         *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       tip-she-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di esecuzione di una shell interattiva, per poi *
      *    * rientrare in programma                                    *
      *    *-----------------------------------------------------------*
       she-int-000.
      *              *-------------------------------------------------*
      *              * Flag indicatore di Si i-o su terminale          *
      *              *-------------------------------------------------*
           move      "S"                  to   z-sys-shl-iot          .
       she-int-100.
      *              *-------------------------------------------------*
      *              * Se e' stato passato un comando si richiama la   *
      *              * chiamata normale al sistema, passando quel co-  *
      *              * mando. In caso contrario significhera' che e'   *
      *              * stato passato il comando a spaces, che indica   *
      *              * la richiesta di una shell manuale               *
      *              *-------------------------------------------------*
       she-int-200.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        o-shs                =    spaces
                     go to she-int-300.
      *                  *---------------------------------------------*
      *                  * Preparazione dell'area per il comando       *
      *                  *---------------------------------------------*
           move      o-shs                to   z-sys-shl-par-cds      .
      *                  *---------------------------------------------*
      *                  * Preparazione del pathname unico temporaneo  *
      *                  * per l'esecuzione dei files .bat automatici  *
      *                  * per le chiamate al sistema                  *
      *                  *---------------------------------------------*
           move      z-sys-shl-put-bau    to   z-bat-pat              .
      *                  *---------------------------------------------*
      *                  * Richiamo della shell di sistema             *
      *                  *---------------------------------------------*
           perform   cll-sys-shl-000      thru cll-sys-shl-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     she-int-999.
       she-int-300.
      *              *-------------------------------------------------*
      *              * Preparazione dell'area per il comando a spaces  *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-par-cds      .
      *              *-------------------------------------------------*
      *              * Richiamo della shell di sistema                 *
      *              *-------------------------------------------------*
           perform   cll-sys-shl-000      thru cll-sys-shl-999        .
       she-int-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di esecuzione di una procedura batch standard,  *
      *    * all'interno della directory \abd\bat                      *
      *    *-----------------------------------------------------------*
       bat-std-000.
      *              *-------------------------------------------------*
      *              * Salvataggio nome della procedura batch          *
      *              *-------------------------------------------------*
           move      o-com                to   z-sav-npb              .
       bat-std-100.
      *              *-------------------------------------------------*
      *              * Preparazione del nome del comando               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione composizione pathname      *
      *                  *---------------------------------------------*
           perform   fun-xa0-000          thru fun-xa0-999            .
      *                  *---------------------------------------------*
      *                  * 1. componente per composizione pathname     *
      *                  *---------------------------------------------*
           move      "bat"                to   o-com                  .
           perform   fun-xa5-000          thru fun-xa5-999            .
      *                  *---------------------------------------------*
      *                  * 2. componente per composizione pathname     *
      *                  *---------------------------------------------*
           move      z-sav-npb            to   o-com                  .
           perform   fun-xa5-000          thru fun-xa5-999            .
      *                  *---------------------------------------------*
      *                  * Ottenimento pathname completo sotto \abd in *
      *                  * o-pat                                       *
      *                  *---------------------------------------------*
           perform   fun-xa9-000          thru fun-xa9-999            .
       bat-std-200.
      *              *-------------------------------------------------*
      *              * Preparazione del pathname (non unico e non tem- *
      *              * poraneo, in quanto gia' esistente) per l'esecu- *
      *              * zione del file .bat (non automatico) per la     *
      *              * chiamata al sistema                             *
      *              *-------------------------------------------------*
           move      o-pat                to   z-bat-pat              .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema con Si i-o                  *
      *              *-------------------------------------------------*
           perform   cll-sys-tem-sio-000  thru cll-sys-tem-sio-999    .
       bat-std-300.
      *              *-------------------------------------------------*
      *              * Status di uscita a : eseguito                   *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       bat-std-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di esecuzione di un comando contenuto in \abd   *
      *    *-----------------------------------------------------------*
       com-abd-000.
      *              *-------------------------------------------------*
      *              * Completamento preparazione pathname completo    *
      *              * con l'ultimo componente, ovvero il nome stesso  *
      *              * del comando                                     *
      *              *-------------------------------------------------*
           perform   fun-xa5-000          thru fun-xa5-999            .
       com-abd-100.
      *              *-------------------------------------------------*
      *              * Richiamo della routine per l'ottenimento in     *
      *              * o-pat del pathname completo con locazione sotto *
      *              * \abd a seconda dei richiami gia' avvenuti di    *
      *              * A0 a A5, arricchiti del nome del comando        *
      *              *-------------------------------------------------*
           perform   fun-xa9-000          thru fun-xa9-999            .
       com-abd-200.
      *              *-------------------------------------------------*
      *              * Preparazione del pathname (non unico e non tem- *
      *              * poraneo, in quanto gia' esistente) per l'esecu- *
      *              * zione del file .bat (non automatico) per la     *
      *              * chiamata al sistema, con aggiunta dei parametri *
      *              * passati al comando                              *
      *              *-------------------------------------------------*
           move      spaces               to   z-bat-pat              .
           string    o-pat      delimited by   spaces
                     " "        delimited by   size
                     o-shs      delimited by   size
                                          into z-bat-pat              .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema con Si i-o                  *
      *              *-------------------------------------------------*
           perform   cll-sys-tem-sio-000  thru cll-sys-tem-sio-999    .
       com-abd-300.
      *              *-------------------------------------------------*
      *              * Status di uscita a : eseguito                   *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       com-abd-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di una 'Call Unix', ovvero di una chiamata di-  *
      *    * retta al sistema, 
      *    *-----------------------------------------------------------*
       cll-unx-000.
      *              *-------------------------------------------------*
      *              * Composizione del comando completo da sottoporre *
      *              * alla chiamata della shell di sistema, cosi' co- *
      *              * me passato dal chiamante                        *
      *              *-------------------------------------------------*
           move      o-shs                to   z-bat-pat              .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema con Si i-o                  *
      *              *-------------------------------------------------*
           perform   cll-sys-tem-sio-000  thru cll-sys-tem-sio-999    .
      *              *-------------------------------------------------*
      *              * Status di uscita a : eseguito                   *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       cll-unx-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di ammissibilita' background                    *
      *    *-----------------------------------------------------------*
       snx-bkg-000.
      *              *-------------------------------------------------*
      *              * Ammissibilita' background : No                  *
      *              *-------------------------------------------------*
           move      "##"                 to   o-sts                  .
       snx-bkg-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di ammissibilita' stampa in spool               *
      *    *-----------------------------------------------------------*
       snx-ssp-000.
      *              *-------------------------------------------------*
      *              * Ammissibilita' stampa in spool : Si             *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       snx-ssp-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di esecuzione in background                     *
      *    *-----------------------------------------------------------*
       exe-bkg-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a : non eseguito               *
      *              *-------------------------------------------------*
           move      "##"                 to   o-sts                  .
       exe-bkg-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di esecuzione di desk-accessories               *
      *    *-----------------------------------------------------------*
       exe-dac-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a : non eseguito               *
      *              *-------------------------------------------------*
           move      "##"                 to   o-sts                  .
       exe-dac-999.
           exit.

      *    *===========================================================*
      *    * Richiesta del livello di esecuzione di desk-accessories   *
      *    *-----------------------------------------------------------*
       liv-dac-000.
      *              *-------------------------------------------------*
      *              * Status di uscita pari al livello                *
      *              *-------------------------------------------------*
           move      z-var-env-lid        to   o-sts                  .
      *              *-------------------------------------------------*
      *              * Tipo di desk-accessory, solo se in esecuzione   *
      *              * di un desk-accessory                            *
      *              *-------------------------------------------------*
           if        z-var-env-lid        not  < "01" and
                     z-var-env-lid        not  > "99"
                     move  z-var-env-sub  to   o-pat
           else      move  spaces         to   o-pat                  .
       liv-dac-999.
           exit.

      *    *===========================================================*
      *    * Tipo di controllo su licenza d'uso                        *
      *    *-----------------------------------------------------------*
       tip-lus-000.
      *              *-------------------------------------------------*
      *              * Solo su licenziatario                           *
      *              *-------------------------------------------------*
           move      "01"                 to   o-sts                  .
       tip-lus-999.
           exit.

      *    *===========================================================*
      *    * Separazione di un pathname in basename e filename         *
      *    *-----------------------------------------------------------*
       bna-fna-000.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine di ispezione e sepa-  *
      *              * razione di un pathname                          *
      *              *-------------------------------------------------*
           move      o-pat                to   z-pth-pat-nam          .
           perform   isp-pat-nam-000      thru isp-pat-nam-999        .
      *              *-------------------------------------------------*
      *              * Risultati in uscita                             *
      *              *-------------------------------------------------*
           move      z-pth-bas-nam        to   o-pat                  .
           move      z-pth-fil-nam        to   o-com                  .
       bna-fna-999.
           exit.

      *    *===========================================================*
      *    * Composizione di un pathname da basename e filename        *
      *    *-----------------------------------------------------------*
       pat-nam-000.
      *              *-------------------------------------------------*
      *              * Valori in input in aree di comodo               *
      *              *-------------------------------------------------*
           move      o-pat                to   z-pth-bas-nam          .
           move      o-com                to   z-pth-fil-nam          .
      *              *-------------------------------------------------*
      *              * Composizione del pathname                       *
      *              *-------------------------------------------------*
           move      spaces               to   z-pth-pat-nam          .
           if        z-pth-bas-nam        =    spaces and
                     z-pth-fil-nam        =    spaces
                     go to pat-nam-900.
           if        z-pth-bas-nam        =    spaces
                     move  z-pth-fil-nam  to   z-pth-pat-nam
                     go to pat-nam-900.
           if        z-pth-fil-nam        =    spaces
                     move  z-pth-bas-nam  to   z-pth-pat-nam
                     go to pat-nam-900.
           string    z-pth-bas-nam
                                delimited by   spaces
                     z-pth-fil-nam
                                delimited by   spaces
                                          into z-pth-pat-nam          .
       pat-nam-900.
      *              *-------------------------------------------------*
      *              * Risultato in uscita                             *
      *              *-------------------------------------------------*
           move      z-pth-pat-nam        to   o-pat                  .
       pat-nam-999.
           exit.

      *    *===========================================================*
      *    * Copy File by Pathname                                     *
      *    *-----------------------------------------------------------*
       cpy-pth-000.
      *              *-------------------------------------------------*
      *              * Se i pathnames di origine e di destinazione so- *
      *              * no uguali : nessuna azione                      *
      *              *-------------------------------------------------*
           if        z-pat-abd-ele (1)    =    z-pat-abd-ele (2)
                     go to cpy-pth-900.
      *              *-------------------------------------------------*
      *              * Se anche uno solo dei due pathnames risulta a   *
      *              * spaces : nessuna azione                         *
      *              *-------------------------------------------------*
           if        z-pat-abd-ele (1)    =    spaces or
                     z-pat-abd-ele (2)    =    spaces
                     go to cpy-pth-900.
       cpy-pth-100.
      *              *-------------------------------------------------*
      *              * Composizione del comando da sottoporre alla     *
      *              * chiamata della shell di sistema.                *
      *              *                                                 *
      *              * Si presume che preventivamente al richiamo di   *
      *              * questa funzione:                                *
      *              *                                                 *
      *              * [] Sia stata richiamata la funzione A0          *
      *              *                                                 *
      *              * [] Sia stata richiamata la funzione A5 per co-  *
      *              *    municare il pathname del file di origine     *
      *              *                                                 *
      *              * [] Sia stata richiamata la funzione A5 per co-  *
      *              *    municare il pathname del file di destina-    *
      *              *    zione                                        *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-par-cds      .
           string    "COPY "
                                delimited by   size
                     z-pat-abd-ele (1)
                                delimited by   spaces
                     " /B "
                                delimited by   size
                     z-pat-abd-ele (2)
                                delimited by   spaces
                     " /B "
                                delimited by   size
                                          into z-sys-shl-par-cds      .
      *              *-------------------------------------------------*
      *              * Null-redirection per standard output            *
      *              *-------------------------------------------------*
           perform   out-nul-dev-000      thru out-nul-dev-999        .
      *              *-------------------------------------------------*
      *              * Null-redirection per standard error             *
      *              *-------------------------------------------------*
           perform   err-nul-dev-000      thru err-nul-dev-999        .
      *              *-------------------------------------------------*
      *              * Flag indicatore di No i-o su terminale          *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-iot          .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname unico temporaneo per  *
      *              * l'esecuzione dei files .bat automatici per le   *
      *              * chiamate al sistema                             *
      *              *-------------------------------------------------*
           move      z-sys-shl-put-bau    to   z-bat-pat              .
      *              *-------------------------------------------------*
      *              * Richiamo della shell di sistema                 *
      *              *-------------------------------------------------*
           perform   cll-sys-shl-000      thru cll-sys-shl-999        .
       cpy-pth-900.
      *              *-------------------------------------------------*
      *              * Status di uscita a : eseguito                   *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       cpy-pth-999.
           exit.

      *    *===========================================================*
      *    * Delete File by Pathname                                   *
      *    *-----------------------------------------------------------*
       del-pth-000.
      *              *-------------------------------------------------*
      *              * Se il pathname del file da cancellare risulta a *
      *              * spaces : nessuna azione                         *
      *              *-------------------------------------------------*
           if        z-pat-abd-ele (1)    =    spaces
                     go to del-pth-900.
       del-pth-100.
      *              *-------------------------------------------------*
      *              * Composizione del comando da sottoporre alla     *
      *              * chiamata della shell di sistema.                *
      *              *                                                 *
      *              * Si presume che preventivamente al richiamo di   *
      *              * questa funzione:                                *
      *              *                                                 *
      *              * [] Sia stata richiamata la funzione A0          *
      *              *                                                 *
      *              * [] Sia stata richiamata la funzione A5 per co-  *
      *              *    municare il pathname del file da cancellare  *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-par-cds      .
           string    "DEL "
                                delimited by   size
                     z-pat-abd-ele (1)
                                delimited by   spaces
                                          into z-sys-shl-par-cds      .
      *              *-------------------------------------------------*
      *              * Null-redirection per standard output            *
      *              *-------------------------------------------------*
           perform   out-nul-dev-000      thru out-nul-dev-999        .
      *              *-------------------------------------------------*
      *              * Null-redirection per standard error             *
      *              *-------------------------------------------------*
           perform   err-nul-dev-000      thru err-nul-dev-999        .
      *              *-------------------------------------------------*
      *              * Flag indicatore di No i-o su terminale          *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-iot          .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname unico temporaneo per  *
      *              * l'esecuzione dei files .bat automatici per le   *
      *              * chiamate al sistema                             *
      *              *-------------------------------------------------*
           move      z-sys-shl-put-bau    to   z-bat-pat              .
      *              *-------------------------------------------------*
      *              * Richiamo della shell di sistema                 *
      *              *-------------------------------------------------*
           perform   cll-sys-shl-000      thru cll-sys-shl-999        .
       del-pth-900.
      *              *-------------------------------------------------*
      *              * Status di uscita a : eseguito                   *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       del-pth-999.
           exit.

      *    *===========================================================*
      *    * Rename File by Pathname                                   *
      *    *-----------------------------------------------------------*
       ren-pth-000.
      *              *-------------------------------------------------*
      *              * Se i pathnames di origine e di destinazione so- *
      *              * no uguali : nessuna azione                      *
      *              *-------------------------------------------------*
           if        z-pat-abd-ele (1)    =    z-pat-abd-ele (2)
                     go to ren-pth-900.
      *              *-------------------------------------------------*
      *              * Se anche uno solo dei due pathnames risulta a   *
      *              * spaces : nessuna azione                         *
      *              *-------------------------------------------------*
           if        z-pat-abd-ele (1)    =    spaces or
                     z-pat-abd-ele (2)    =    spaces
                     go to ren-pth-900.
       ren-pth-100.
      *              *-------------------------------------------------*
      *              * Suddivisione del pathname di origine nelle sue  *
      *              * componenti Base Name e File Name, e salvatag-   *
      *              * gio dei valori distinti                         *
      *              *-------------------------------------------------*
           move      z-pat-abd-ele (1)    to   z-pth-pat-nam          .
           perform   isp-pat-nam-000      thru isp-pat-nam-999        .
           move      z-pth-bas-nam        to   z-pat-abd-ele (5)      .
           move      z-pth-fil-nam        to   z-pat-abd-ele (6)      .
       ren-pth-200.
      *              *-------------------------------------------------*
      *              * Suddivisione del pathname di destinazione nelle *
      *              * sue componenti Base Name e File Name, e salva-  *
      *              * taggio dei valori distinti                      *
      *              *-------------------------------------------------*
           move      z-pat-abd-ele (2)    to   z-pth-pat-nam          .
           perform   isp-pat-nam-000      thru isp-pat-nam-999        .
           move      z-pth-bas-nam        to   z-pat-abd-ele (7)      .
           move      z-pth-fil-nam        to   z-pat-abd-ele (8)      .
       ren-pth-300.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del confronto tra i due    *
      *              * Base Names                                      *
      *              *-------------------------------------------------*
           if        z-pat-abd-ele (5)    =    z-pat-abd-ele (7)
                     go to ren-pth-400
           else      go to ren-pth-500.
       ren-pth-400.
      *              *-------------------------------------------------*
      *              * Se i due Base Names sono uguali                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione del comando da sottoporre alla *
      *                  * chiamata della shell di sistema, per l'o-   *
      *                  * perazione di ridenominazione vera e propria *
      *                  *                                             *
      *                  * Si presume che preventivamente al richiamo  *
      *                  * di questa funzione:                         *
      *                  *                                             *
      *                  * [] Sia stata richiamata la funzione A0      *
      *                  *                                             *
      *                  * [] Sia stata richiamata la funzione A5 per  *
      *                  *    comunicare il pathname del file di ori-  *
      *                  *    gine                                     *
      *                  *                                             *
      *                  * [] Sia stata richiamata la funzione A5 per  *
      *                  *    comunicare il pathname del file rideno-  *
      *                  *    minato                                   *
      *                  *---------------------------------------------*
           move      spaces               to   z-sys-shl-par-cds      .
           string    "REN "
                                delimited by   size
                     z-pat-abd-ele (1)
                                delimited by   spaces
                     " "
                                delimited by   size
                     z-pat-abd-ele (8)
                                delimited by   spaces
                                          into z-sys-shl-par-cds      .
      *                  *---------------------------------------------*
      *                  * Null-redirection per standard output        *
      *                  *---------------------------------------------*
           perform   out-nul-dev-000      thru out-nul-dev-999        .
      *                  *---------------------------------------------*
      *                  * Null-redirection per standard error         *
      *                  *---------------------------------------------*
           perform   err-nul-dev-000      thru err-nul-dev-999        .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di No i-o su terminale      *
      *                  *---------------------------------------------*
           move      spaces               to   z-sys-shl-iot          .
      *                  *---------------------------------------------*
      *                  * Preparazione del pathname unico temporaneo  *
      *                  * per l'esecuzione dei files .bat automatici  *
      *                  * per le chiamate al sistema                  *
      *                  *---------------------------------------------*
           move      z-sys-shl-put-bau    to   z-bat-pat              .
      *                  *---------------------------------------------*
      *                  * Richiamo della shell di sistema             *
      *                  *---------------------------------------------*
           perform   cll-sys-shl-000      thru cll-sys-shl-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ren-pth-900.
       ren-pth-500.
      *              *-------------------------------------------------*
      *              * Se i due Base Names sono diversi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Copiatura del file originale sul file di    *
      *                  * destinazione                                *
      *                  *---------------------------------------------*
           perform   cpy-pth-000          thru cpy-pth-999            .
      *                  *---------------------------------------------*
      *                  * Cancellazione del file originale            *
      *                  *---------------------------------------------*
           perform   del-pth-000          thru del-pth-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ren-pth-900.
       ren-pth-900.
      *              *-------------------------------------------------*
      *              * Status di uscita a : eseguito                   *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
       ren-pth-999.
           exit.

      *    *===========================================================*
      *    * Informazioni su filename ed estensione                    *
      *    *-----------------------------------------------------------*
       inf-fxt-000.
      *              *-------------------------------------------------*
      *              * Massima dimensione per un filename              *
      *              *-------------------------------------------------*
           move      008                  to   o-mxf                  .
      *              *-------------------------------------------------*
      *              * Massima dimensione per una estensione           *
      *              *-------------------------------------------------*
           move      003                  to   o-mxe                  .
      *              *-------------------------------------------------*
      *              * Carattere di separazione per l'estensione       *
      *              *-------------------------------------------------*
           move      "."                  to   o-che                  .
       inf-fxt-999.
           exit.

      *================================================================*
      *       Subroutines                                              *
      *----------------------------------------------------------------*

      *    *===========================================================*
      *    * Subroutine per trasformazione in uppercase                *
      *    *-----------------------------------------------------------*
       tra-val-upp-000.
           move      zero                 to   z-upp-cas-c01          .
       tra-val-upp-100.
           add       1                    to   z-upp-cas-c01          .
           if        z-upp-cas-c01        >    40
                     go to  tra-val-upp-999.
           move      zero                 to   z-upp-cas-c02          .
           inspect   z-upp-cas-low    tallying z-upp-cas-c02
                     for characters     before
                                       initial z-upp-cas-chr
                                              (z-upp-cas-c01)         .
           if        z-upp-cas-c02        not  < 26
                     go to tra-val-upp-100.
           add       1                    to   z-upp-cas-c02          .
           move      z-upp-cas-upp
                    (z-upp-cas-c02: 1)    to   z-upp-cas-chr
                                              (z-upp-cas-c01)         .
           go to     tra-val-upp-100.
       tra-val-upp-999.
           exit.

      *    *===========================================================*
      *    * Completamento du un comando che deve essere eseguito dal- *
      *    * la shell di sistema per la redirezione dell'output su     *
      *    * 'null device'                                             *
      *    *-----------------------------------------------------------*
       out-nul-dev-000.
      *              *-------------------------------------------------*
      *              * Determinazione del puntatore per l'accodamento  *
      *              *-------------------------------------------------*
           move      zero                 to   z-sys-shl-c01          .
           inspect   z-sys-shl-par-cds
                                      tallying z-sys-shl-c01
                                  for trailing spaces                 .
           subtract  z-sys-shl-c01        from 220
                                        giving z-sys-shl-p01          .
           add       2                    to   z-sys-shl-p01          .
      *              *-------------------------------------------------*
      *              * Suffisso                                        *
      *              *-------------------------------------------------*
           string    "> NUL"
                                delimited by   size
                                          into z-sys-shl-par-cds
                                  with pointer z-sys-shl-p01          .
       out-nul-dev-999.
           exit.

      *    *===========================================================*
      *    * Completamento du un comando che deve essere eseguito dal- *
      *    * la shell di sistema per la redirezione degli eventuali    *
      *    * errori su 'null device'                                   *
      *    *-----------------------------------------------------------*
       err-nul-dev-000.
      *              *-------------------------------------------------*
      *              * Nessuna azione                                  *
      *              *-------------------------------------------------*
       err-nul-dev-999.
           exit.

      *    *===========================================================*
      *    * Chiamata alla shell di sistema                            *
      *    *-----------------------------------------------------------*
       cll-sys-shl-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore dell'area di    *
      *              * comando che deve essere eseguito                *
      *              *-------------------------------------------------*
           if        z-sys-shl-par-cds    =    spaces
                     go to cll-sys-shl-100
           else      go to cll-sys-shl-200.
       cll-sys-shl-100.
      *              *-------------------------------------------------*
      *              * Se area di comando a spaces, ovvero se e' stata *
      *              * richiesta una shell interattiva a comandi ma-   *
      *              * nuali                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiamata al sistema senza parametri         *
      *                  *---------------------------------------------*
           perform   cll-sys-tem-nop-000  thru cll-sys-tem-nop-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cll-sys-shl-999.
       cll-sys-shl-200.
      *              *-------------------------------------------------*
      *              * Se area di comando diversa da spaces, ovvero se *
      *              * e' stata richiesta l'esecuzione di un comando   *
      *              * vero e proprio                                  *
      *              *-------------------------------------------------*
       cll-sys-shl-300.
      *                  *---------------------------------------------*
      *                  * Composizione del file di appoggio [bat]     *
      *                  * contenente il batch di comandi              *
      *                  *---------------------------------------------*
       cll-sys-shl-310.
      *                      *-----------------------------------------*
      *                      * Open output [bat]                       *
      *                      *-----------------------------------------*
           open      output bat                                       .
       cll-sys-shl-320.
      *                      *-----------------------------------------*
      *                      * Composizione e scrittura della prima    *
      *                      * linea del batch per porre in Off la     *
      *                      * funzione di eco dei comandi eseguiti    *
      *                      *-----------------------------------------*
           move      "@ECHO OFF"          to   bat-rec                .
           write     bat-rec            before 1                      .
       cll-sys-shl-330.
      *                      *-----------------------------------------*
      *                      * Composizione e scrittura della seconda  *
      *                      * linea del batch contenente il comando   *
      *                      * vero e proprio che deve essere eseguito *
      *                      *-----------------------------------------*
           move      z-sys-shl-par-cds    to   bat-rec                .
           write     bat-rec            before 1                      .
       cll-sys-shl-340.
      *                      *-----------------------------------------*
      *                      * Close [bat]                             *
      *                      *-----------------------------------------*
           close     bat                                              .
       cll-sys-shl-400.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del flag indicatore di *
      *                  * Si/No i-o su terminale                      *
      *                  *---------------------------------------------*
           if        z-sys-shl-iot        not  = spaces
                     go to cll-sys-shl-600.
       cll-sys-shl-500.
      *                  *---------------------------------------------*
      *                  * Se non c'e' i-o su terminale                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema utilizzando il solo *
      *                      * pathname del file contenente il batch,  *
      *                      * e con il parametro di no i-o            *
      *                      *-----------------------------------------*
           perform   cll-sys-tem-nio-000  thru cll-sys-tem-nio-999    .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-shl-999.
       cll-sys-shl-600.
      *                  *---------------------------------------------*
      *                  * Se c'e' i-o su terminale                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema utilizzando il solo *
      *                      * pathname del file contenente il batch,  *
      *                      * ma senza il parametro di no i-o         *
      *                      *-----------------------------------------*
           perform   cll-sys-tem-sio-000  thru cll-sys-tem-sio-999    .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-shl-999.
       cll-sys-shl-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema senza parametri                       *
      *    *-----------------------------------------------------------*
       cll-sys-tem-nop-000.
      *              *-------------------------------------------------*
      *              * Chiamata                                        *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-com-001      .
           string    z-dsk-sys
                               delimited  by   size
                     "\COMMAND.COM"
                               delimited  by   size
                                          into z-sys-shl-com-001      .
           call      "SYSTEM"            using z-sys-shl-com-001      .
      *              *-------------------------------------------------*
      *              * Ripristino work directory                       *
      *              *-------------------------------------------------*
           move      spaces               to   z-sys-shl-com-001      .
           string    z-dsk-abd
                               delimited  by   size
                     "\abd\ogt"
                               delimited  by   size
                                          into z-sys-shl-com-001      .
           call      "C$CHDIR"           using z-sys-shl-com-001      .
       cll-sys-tem-nop-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema con No i-o                            *
      *    *-----------------------------------------------------------*
       cll-sys-tem-nio-000.
      *              *-------------------------------------------------*
      *              * Chiamata                                        *
      *              *-------------------------------------------------*
           call      "SYSTEM"            using z-bat-pat
                                               z-sys-shl-nio          .
       cll-sys-tem-nio-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema con Si i-o                            *
      *    *-----------------------------------------------------------*
       cll-sys-tem-sio-000.
      *              *-------------------------------------------------*
      *              * Chiamata                                        *
      *              *-------------------------------------------------*
           call      "SYSTEM"            using z-bat-pat              .
       cll-sys-tem-sio-999.
           exit.

      *    *===========================================================*
      *    * Ispezione di un pathname, contenuto in 'z-pth-pat-nam'    *
      *    *-----------------------------------------------------------*
       isp-pat-nam-000.
      *              *-------------------------------------------------*
      *              * Determinazione del numero di caratteri di sepa- *
      *              * razione elementi                                *
      *              *-------------------------------------------------*
           move      zero                 to   z-pth-pth-i01          .
           inspect   z-pth-pat-nam
                                      tallying z-pth-pth-i01
                                      for all  "\"                    .
           inspect   z-pth-pat-nam
                                      tallying z-pth-pth-i01
                                      for all  ":"                    .
      *              *-------------------------------------------------*
      *              * Se zero caratteri di separazione il pathname da *
      *              * ispezionare corrisponde ad un File Name senza   *
      *              * alcun Base Name                                 *
      *              *-------------------------------------------------*
           if        z-pth-pth-i01        =    zero
                     move  spaces         to   z-pth-bas-nam
                     move  z-pth-pat-nam  to   z-pth-fil-nam
                     go to isp-pat-nam-999.
       isp-pat-nam-100.
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice sull'ultimo caratte- *
      *              * re di separazione                               *
      *              *-------------------------------------------------*
       isp-pat-nam-120.
           move      40                   to   z-pth-pth-i01          .
       isp-pat-nam-140.
           if        z-pth-pat-chr
                    (z-pth-pth-i01)       =    "\"
                     go to isp-pat-nam-200.
           if        z-pth-pat-chr
                    (z-pth-pth-i01)       =    ":"
                     go to isp-pat-nam-200.
           subtract  1                    from z-pth-pth-i01          .
           go to     isp-pat-nam-140.
       isp-pat-nam-200.
      *              *-------------------------------------------------*
      *              * Determinazione del Base Name, comprendente il   *
      *              * carattere di separazione                        *
      *              *-------------------------------------------------*
           move      spaces               to   z-pth-bas-nam          .
           move      z-pth-pat-nam
                    (01: z-pth-pth-i01)   to   z-pth-bas-nam          .
       isp-pat-nam-300.
      *              *-------------------------------------------------*
      *              * Determinazione del File Name                    *
      *              *-------------------------------------------------*
           move      40                   to   z-pth-pth-i02          .
           subtract  z-pth-pth-i01        from z-pth-pth-i02          .
           add       1                    to   z-pth-pth-i01          .
           move      spaces               to   z-pth-fil-nam          .
           move      z-pth-pat-nam
                    (z-pth-pth-i01:
                     z-pth-pth-i02)       to   z-pth-fil-nam          .
       isp-pat-nam-999.
           exit.

