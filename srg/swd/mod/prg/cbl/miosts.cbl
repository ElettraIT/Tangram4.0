       Identification Division.
       Program-Id.                                 miosts             .
      *================================================================*
      *                                                                *
      *    Modulo per il reperimento dell' I-O status .                *
      *                                                                *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    N.B.: vedi note importanti con tag 'NdeK'                   *
      *                                                                *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Converte il file status passato dal runtime in un file sta- *
      *    tus convenzionale. Cio' consente di far fronte a cambiamen- *
      *    ti di codifica dei file status da parte dei cobol runtimes. *
      *                                                                *
      *    Inoltre, ma solo in caso di errore, memorizza il codice di  *
      *    errore espanso a 4 digits, solo se il tipo di runtime uti-  *
      *    lizzato supporta questo tipo di codice di errore espanso.   *
      *                                                                *
      *    Se la chiamata viene eseguita con il codice di errore pari  *
      *    a low values, il modulo ritorna nella'area di interfaccia   *
      *    rispettivamente :                                           *
      *    - il nome del file                                          *
      *    - il pathname del file                                      *
      *    - lo status di errore cobol reale                           *
      *    relativi all'ultima operazione di i-o terminata con status  *
      *    di errore diverso da e-not-err.                             *
      *                                                                *
      *    Se la chiamata viene eseguita con il codice di errore pari  *
      *    a high values, il modulo ritorna nella'area di interfaccia  *
      *    rispettivamente :                                           *
      *    - il nome del file                                          *
      *    - il pathname del file                                      *
      *    - l'estensione allo status di errore cobol reale            *
      *    relativi all'ultima operazione di i-o terminata con status  *
      *    di errore diverso da e-not-err.                             *
      *                                                                *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Il file status convenzionale e' codificato secondo lo sche- *
      *    ma seguente :                                               *
      *                                                                *
      *    Status  Mnemonico       Descrizione del file status         *
      *    ------  ---------  ---------------------------------------  *
      *                                                                *
      *     "00"   e-not-err  Nessun errore                            *
      *                                                                *
      *     "02"   e-not-lst  Il record appena letto sequenzialmente,  *
      *                       per mezzo di una chiave alternativa, e'  *
      *                       seguito da altri records aventi lo stes- *
      *                       so valore per la chiave letta, cioe' non *
      *                       e' l'ultimo di un set di duplicati       *
      *                                                                *
      *     "10"   e-end-fil  Raggiunta la condizione di 'at end'      *
      *                                                                *
      *     "21"   e-not-ord  Chiave non in ordine sequenziale in caso *
      *                       di write su di un file ad indici in ac-  *
      *                       cesso sequenziale                        *
      *                                                                *
      *     "22"   e-dup-key  Chiave gia' esistente in caso di write   *
      *                       su di un file ad indici in accesso ca-   *
      *                       suale                                    *
      *                                                                *
      *     "23"   e-not-fnd  Chiave non esistente in caso di read da  *
      *                       un file ad indici in accesso casuale     *
      *                                                                *
      *     "30"   e-hdw-err  Permanent error - hardware error         *
      *                                                                *
      *     "34"   e-dsk-exh  Permanent error                          *
      *                       - non piu' spazio fisico su disco        *
      *                       - tentativo di scrittura oltre i limiti  *
      *                         stabiliti esternamente per il file     *
      *                                                                *
      *     "91"   e-opn-err  Errore su open per uno dei seguenti mo-  *
      *                       tivi :                                   *
      *                       - il file non esiste                     *
      *                       - il nome del file non e' formulato cor- *
      *                         rettamente                             *
      *                       - il file e' gia' aperto nello stesso    *
      *                         programma                              *
      *                       - insufficienti canali di i-o disponibi- *
      *                         li                                     *
      *                       Errore su close per il seguente motivo : *
      *                       - il file non e' mai stato aperto nel    *
      *                         programma
      *                                                                *
      *     "92"   e-mod-err  Errore di modo per uno dei seguenti mo-  *
      *                       tivi :                                   *
      *                       - il file non e' mai stato aperto        *
      *                       - operazione di scrittura su file aperto *
      *                         in input                               *
      *                       - operazione di lettura su file aperto   *
      *                         in output                              *
      *                       - tentativo di apertura di un file chiu- *
      *                         so con lock                            *
      *                       - delete o rewrite su di un file in ac-  *
      *                         cesso sequenziale non preceduta da una *
      *                         read eseguita con successo             *
      *                       - write o rewrite di un record sequen-   *
      *                         ziale piu' grande della lunghezza mas- *
      *                         sima prevista per il record            *
      *                       - rewrite di un record sequenziale che   *
      *                         non ha la stessa lunghezza del record  *
      *                         da sostituire                          *
      *                       - il file non supporta il modo di aper-  *
      *                         tura richiesto                         *
      *                       - il file da deletare e' protetto        *
      *                       - il file da deletare e' stato chiuso    *
      *                         con lock                               *
      *                                                                *
      *     "94"   e-use-err  Errore di uso per uno dei seguenti moti- *
      *                       vi :                                     *
      *                       - file gia' in uso esclusivo da parte di *
      *                         altri                                  *
      *                       - record gia' in uso esclusivo da parte  *
      *                         di altri                               *
      *                                                                *
      *     "97"   e-lim-err  Errore di superamento limiti per uno dei *
      *                       seguenti motivi :                        *
      *                       - troppi record locked                   *
      *                       - troppi files indexed o relative aperti *
      *                                                                *
      *     "98"   e-noc-spc  Permanent error - no contiguous space    *
      *                                                                *
      *     "9A"   e-fil-inc  Incompatibilita' di file per uno dei se- *
      *                       guenti motivi :                          *
      *                       - tipo di file non corrispondente        *
      *                       - chiavi non corrispondenti              *
      *                       - lunghezza record non corrispondente    *
      *                                                                *
      *     "9B"   e-cor-inx  Area indici corrotta                     *
      *                                                                *
      *     "9C"   e-per-vio  Permission violation : mancano i permes- *
      *                       si necessari all'accesso                 *
      *                                                                *
      *     "9D"   e-nam-err  Il nome del file ha zero o troppi carat- *
      *                       teri                                     *
      *                                                                *
      *     "9E"   e-rec-len  Il record e' troppo lungo, ed eccede la  *
      *                       lunghezza massima consentita             *
      *                                                                *
      *     "9F"   e-pag-ful  Page full in line sequential file        *
      *                                                                *
      *     "**"   e-fat-err  Flag convenzionale di fatal error        *
      *                                                                *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Lo schema sopra riportato corrisponde all'area che viene    *
      *    inclusa tramite COPY in tutti i moduli di trattamento di    *
      *    input-output su file, cioe' swd/mod/int/e.                  *
      *                                                                *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Questo modulo utilizza in linkage l'area seguente :         *
      *                                                                *
      *    01  c.                                                      *
      *        05  c-nam   pic  x(04).                                 *
      *        05  c-pat   pic  x(40).                                 *
      *        05  c-sts   pic  x(02).                                 *
      *                                                                *
      *    dove :                                                      *
      *                                                                *
      *            c-nam : nome simbolico del file                     *
      *            c-pat : pathname effettivo del file                 *
      *            c-sts : i-o status del file                         *
      *                                                                *
      *    Questa area viene passata dal chiamante, che normalmente e' *
      *    un modulo di trattamento di input-output su di un file.     *
      *                                                                *
      *    Il modulo provvede a trasformare il valore di c-sts, che    *
      *    dipende dal cobol runtime utilizzato, nel valore conven-    *
      *    zionale descritto nello schema precedente.                  *
      *                                                                *
      *    Cio' consente un certo grado di indipendenza rispetto ai    *
      *    cambiamenti di codifica delle codifiche degli status nei    *
      *    cobol runtimes utilizzati.                                  *
      *                                                                *
      *    Se il valore di c-nam e' pari a low-values si intende la    *
      *    chiamata al modulo non come richiesta di traslazione, ma    *
      *    come comunicazione del tipo di runtime utilizzato. In tal   *
      *    caso si presume che il valore di c-pat rappresenti la si-   *
      *    gla del tipo di runtime utilizzato, secondo lo schema de-   *
      *    scritto di seguito per i tipi di runtime riconosciuti.      *
      *                                                                *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Attualmente il modulo e' in grado di trasformare corretta-  *
      *    mente gli status relativamente ai seguenti runtimes :       *
      *                                                                *
      *    Codice    Sigla            Descrizione del runtime          *
      *    ------  ----------  --------------------------------------  *
      *                                                                *
      *      00     ACU85      Acucobol 85                             *
      *                                                                *
      *      01     ACERMMCO   Austec RM/MASTER COBOL 74               *
      *                                                                *
      *      02     RM85       Ryan-McFarland RM/COBOL-85              *
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
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Indice su tipo di runtime utilizzato                  *
      *        *                                                       *
      *        * - 01 : Acucobol 85                                    *
      *        * - 02 : Austec RM-Master Cobol 74                      *
      *        * - 03 : Ryan Mc-Farland RM-Cobol 85                    *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-run-inx                  pic  9(02)       value zero .
      *        *-------------------------------------------------------*
      *        *                                                       *
      *        * Tabella di traslazione se il runtime utilizzato e' u- *
      *        * no dei seguenti :                                     *
      *        *                                                       *
      *        * Acucobol 85                                           *
      *        * Ryan-McFarland RM/COBOL-85                            *
      *        *                                                       *
      *        * ----------------------------------------------------- *
      *        *                                                       *
      *        * Nota : La codifica dei codici di errore ad uso inter- *
      *        *        no wip e' quella in origine relativa al Run-   *
      *        *        time dell'Austec RM-Master Cobol 74.           *
      *        *        Per ragioni storiche si e' pensato di tenere   *
      *        *        questa codifica come definitiva, in modo da    *
      *        *        poter costruire un manuale di interpretazione  *
      *        *        dei codici di errore costante nel tempo.       *
      *        *                                                       *
      *        *        Pertanto ogni altro tipo di Runtime verra' a-  *
      *        *        deguato a questa codifica per mezzo della tra- *
      *        *        slazione del codice di errore del Runtime nel  *
      *        *        codice di errore interno                       *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-tbl-085.
               10  w-tbl-085-val.
                   15  filler             pic  x(04) value "0000"     .
                   15  filler             pic  x(04) value "0202"     .
                   15  filler             pic  x(04) value "049A"     .
                   15  filler             pic  x(04) value "0500"     .
                   15  filler             pic  x(04) value "0700"     .
                   15  filler             pic  x(04) value "1010"     .
                   15  filler             pic  x(04) value "1410"     .
                   15  filler             pic  x(04) value "2121"     .
                   15  filler             pic  x(04) value "2222"     .
                   15  filler             pic  x(04) value "2323"     .
                   15  filler             pic  x(04) value "2434"     .
                   15  filler             pic  x(04) value "3030"     .
                   15  filler             pic  x(04) value "3434"     .
                   15  filler             pic  x(04) value "3591"     .
                   15  filler             pic  x(04) value "3792"     .
                   15  filler             pic  x(04) value "3892"     .
                   15  filler             pic  x(04) value "399A"     .
                   15  filler             pic  x(04) value "4191"     .
                   15  filler             pic  x(04) value "4292"     .
                   15  filler             pic  x(04) value "4392"     .
                   15  filler             pic  x(04) value "4492"     .
                   15  filler             pic  x(04) value "4610"     .
                   15  filler             pic  x(04) value "4792"     .
                   15  filler             pic  x(04) value "4892"     .
                   15  filler             pic  x(04) value "4992"     .
                   15  filler             pic  x(04) value "9394"     .
                   15  filler             pic  x(04) value "9497"     .
                   15  filler             pic  x(04) value "9792"     .
                   15  filler             pic  x(04) value "989A"     .
                   15  filler             pic  x(04) value "9994"     .
               10  w-tbl-085-var redefines
                   w-tbl-085-val.
                   15  w-tbl-085-ele
                                   occurs 30
                            ascending key is   w-tbl-085-key
                                  indexed by   w-tbl-085-inx          .
                       20  w-tbl-085-key  pic  x(02)                  .
                       20  w-tbl-085-tsz  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di salvataggio file-status reale con espansione al   *
      *    * codice di errore                                          *
      *    *-----------------------------------------------------------*
       01  s.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  s-nam                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  s-pat                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status completo                                  *
      *        *-------------------------------------------------------*
           05  s-fsc.
      *            *---------------------------------------------------*
      *            * File status reale cobol                           *
      *            *---------------------------------------------------*
               10  s-sts                  pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Estensione al file status reale cobol             *
      *            *---------------------------------------------------*
               10  s-ext                  pic  x(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

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

      ******************************************************************
       Procedure Division                using c                      .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di runtime uti- *
      *              * lizzato                                         *
      *              *-------------------------------------------------*
           go to     main-100
                     main-200
                     main-300
                     depending            on   w-run-inx              .
       main-020.
      *              *-------------------------------------------------*
      *              * Se non e' ancora stato specificato quale sia il *
      *              * tipo di runtime che si sta utilizzando          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la chiamata al modulo e' stata eseguita  *
      *                  * specificamente per comunicare il tipo di    *
      *                  * runtime utilizzato                          *
      *                  *---------------------------------------------*
           if        c-nam                not  = low-values
                     go to main-040.
      *                      *-----------------------------------------*
      *                      * Si determina il tipo di runtime utiliz- *
      *                      * zato in base alla sigla passata nel va- *
      *                      * lore di c-pat, default Acucobol 85      *
      *                      *-----------------------------------------*
           if        c-pat                =    "01"
                     move  02             to   w-run-inx
           else if   c-pat                =    "02"
                     move  03             to   w-run-inx
           else      move  01             to   w-run-inx              .
      *                      *-----------------------------------------*
      *                      * Quindi si esce                          *
      *                      *-----------------------------------------*
           go to     main-999.
       main-040.
      *                  *---------------------------------------------*
      *                  * Se la chiamata al modulo e' stata eseguita  *
      *                  * per ottenere la traslazione, ma non era an- *
      *                  * cora stato comunicato il tipo di runtime u- *
      *                  * tilizzato                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si forza il default per il tipo di run- *
      *                      * time utilizzato, ACU85                  *
      *                      *-----------------------------------------*
           move      01                   to   w-run-inx              .
      *                      *-----------------------------------------*
      *                      * Quindi si ricicla per ottenere la tra-  *
      *                      * slazione                                *
      *                      *-----------------------------------------*
           go to     main-000.
       main-100.
      *              *-------------------------------------------------*
      *              * Tipo di runtime : ACU85                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore del codice  *
      *                  * di errore                                   *
      *                  *---------------------------------------------*
           if        c-sts                =    low-values
                     go to main-120
           else if   c-sts                =    high-values
                     go to main-140
           else      go to main-160.
       main-120.
      *                  *---------------------------------------------*
      *                  * Se codice di errore a low-values            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In uscita :                             *
      *                      * - file name                             *
      *                      * - file pathname                         *
      *                      * - file status cobol reale               *
      *                      *-----------------------------------------*
           move      s-nam                to   c-nam                  .
           move      s-pat                to   c-pat                  .
           move      s-sts                to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-140.
      *                  *---------------------------------------------*
      *                  * Se codice di errore a high-values           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In uscita :                             *
      *                      * - file name                             *
      *                      * - file pathname                         *
      *                      * - estensione alfile status cobol reale  *
      *                      *-----------------------------------------*
           move      s-nam                to   c-nam                  .
           move      s-pat                to   c-pat                  .
           move      s-ext                to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-160.
      *                  *---------------------------------------------*
      *                  * Se codice di errore effettivo               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se 'no errore' : uscita                 *
      *                      *-----------------------------------------*
           if        c-sts                =    "00"
                     go to main-999.
      *                      *-----------------------------------------*
      *                      * Se errore                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Memorizzazione di :                 *
      *                          * - file name                         *
      *                          * - file pathname                     *
      *                          * relativi all'ultima operazione di   *
      *                          * i-o avvenuta con errori             *
      *                          *-------------------------------------*
           move      c-nam                to   s-nam                  .
           move      c-pat                to   s-pat                  .
      *                          *-------------------------------------*
      *                          * Determinazione dell'estensione al   *
      *                          * codice di errore, unitamente al     *
      *                          * codice di errore stesso             *
      *                          *-------------------------------------*
           call      "C$RERR"            using s-fsc                  .
      *                          *-------------------------------------*
      *                          * Traslazione in base alla tabella di *
      *                          * transcodifica                       *
      *                          *                                     *
      *                          * Attenzione : NON viene eseguita la  *
      *                          *              transcodifica, rimane  *
      *                          *              bensi' il codice ACU   *
      *                          *-------------------------------------*
           set       w-tbl-085-inx        to   1                      .
           search    w-tbl-085-ele
                     at end move  "**"    to   c-sts
                     when   w-tbl-085-key
                           (w-tbl-085-inx)
                                          =    c-sts
                            move  w-tbl-085-key
                                 (w-tbl-085-inx)
                                          to   c-sts                  .
      *                          *=====================================*
      *                          * MODIFICA DEL 25/07/96 by NdeK       *
      *                          *-------------------------------------*
      *                          * ATTENZIONE : Nella tabella dei co-  *
      *                          *              dici di errore relati- *
      *                          *   ! ! !      va al modulo 'e' si    *
      *                          *              considera l'errore '94'*
      *                          *              in caso di record o di *
      *                          *              file lockato da un al- *
      *                          *              tro utente. Il runtime *
      *                          *              Acucobol assegna a     *
      *                          *              questo tipo di errore  *
      *                          *              il codice '99' quindi  *
      *                          *              il confronto con la    *
      *                          *              variabile 'e-use-err'  *
      *                          *              (presente in tutti gli *
      *                          *              'iof' di Tangram)      *
      *                          *              provoca inevitabilmen- *
      *                          *              te un 'fatal-error'.   *
      *                          *              Per ovviare a questo   *
      *                          *              problema si deve tras- *
      *                          *              codificare l'errore    *
      *                          *              '99' con l'errore '94' *
      *                          *=====================================*
      *                          * MODIFICA DEL 02/12/99 by NdeK       *
      *                          *-------------------------------------*
      *                          * ATTENZIONE : Il codice errore '93', *
      *                          *              pur avendo il signifi- *
      *                          *   ! ! !      cato di file lockato   *
      *                          *              da un altro utente in  *
      *                          *              fase di open, si con-  *
      *                          *              verte comunque in er-  *
      *                          *              rore '94'.             *
      *                          *-------------------------------------*
           if        c-sts                =    "99" or
                     c-sts                =    "93"
                     move  "94"           to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-200.
      *              *-------------------------------------------------*
      *              * Tipo di runtime : ACERMMCO                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore del codice  *
      *                  * di errore                                   *
      *                  *---------------------------------------------*
           if        c-sts                =    low-values
                     go to main-220
           else if   c-sts                =    high-values
                     go to main-240
           else      go to main-260.
       main-220.
      *                  *---------------------------------------------*
      *                  * Se codice di errore a low-values            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In uscita :                             *
      *                      * - file name                             *
      *                      * - file pathname                         *
      *                      * - file status cobol reale               *
      *                      *-----------------------------------------*
           move      s-nam                to   c-nam                  .
           move      s-pat                to   c-pat                  .
           move      s-sts                to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-240.
      *                  *---------------------------------------------*
      *                  * Se codice di errore a high-values           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In uscita :                             *
      *                      * - file name                             *
      *                      * - file pathname                         *
      *                      * - estensione alfile status cobol reale  *
      *                      *-----------------------------------------*
           move      s-nam                to   c-nam                  .
           move      s-pat                to   c-pat                  .
           move      high-values          to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-260.
      *                  *---------------------------------------------*
      *                  * Se codice di errore effettivo               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se 'no errore' : uscita                 *
      *                      *-----------------------------------------*
           if        c-sts                =    "00"
                     go to main-999.
      *                          *-------------------------------------*
      *                          * Memorizzazione di :                 *
      *                          * - file name                         *
      *                          * - file pathname                     *
      *                          * relativi all'ultima operazione di   *
      *                          * i-o avvenuta con errori             *
      *                          *-------------------------------------*
           move      c-nam                to   s-nam                  .
           move      c-pat                to   s-pat                  .
      *                          *-------------------------------------*
      *                          * Memorizzazione di :                 *
      *                          * - codice di errore cobol            *
      *                          * - estensione pari a "00"            *
      *                          *-------------------------------------*
           move      c-sts                to   s-sts                  .
           move      "00"                 to   s-ext                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-300.
      *              *-------------------------------------------------*
      *              * Tipo di runtime : RM85                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore del codice  *
      *                  * di errore                                   *
      *                  *---------------------------------------------*
           if        c-sts                =    low-values
                     go to main-320
           else if   c-sts                =    high-values
                     go to main-340
           else      go to main-360.
       main-320.
      *                  *---------------------------------------------*
      *                  * Se codice di errore a low-values            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In uscita :                             *
      *                      * - file name                             *
      *                      * - file pathname                         *
      *                      * - file status cobol reale               *
      *                      *-----------------------------------------*
           move      s-nam                to   c-nam                  .
           move      s-pat                to   c-pat                  .
           move      s-sts                to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-340.
      *                  *---------------------------------------------*
      *                  * Se codice di errore a high-values           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In uscita :                             *
      *                      * - file name                             *
      *                      * - file pathname                         *
      *                      * - estensione alfile status cobol reale  *
      *                      *-----------------------------------------*
           move      s-nam                to   c-nam                  .
           move      s-pat                to   c-pat                  .
           move      s-ext                to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-360.
      *                  *---------------------------------------------*
      *                  * Se codice di errore effettivo               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se 'no errore' : uscita                 *
      *                      *-----------------------------------------*
           if        c-sts                =    "00"
                     go to main-999.
      *                      *-----------------------------------------*
      *                      * Se errore                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Memorizzazione di :                 *
      *                          * - file name                         *
      *                          * - file pathname                     *
      *                          * relativi all'ultima operazione di   *
      *                          * i-o avvenuta con errori             *
      *                          *-------------------------------------*
           move      c-nam                to   s-nam                  .
           move      c-pat                to   s-pat                  .
      *                          *-------------------------------------*
      *                          * Determinazione dell'estensione al   *
      *                          * codice di errore, unitamente al     *
      *                          * codice di errore stesso             *
      *                          *-------------------------------------*
           call      "C$RERR"            using s-fsc                  .
      *                          *-------------------------------------*
      *                          * Traslazione in base alla tabella di *
      *                          * transcodifica                       *
      *                          *-------------------------------------*
           set       w-tbl-085-inx        to   1                      .
           search    w-tbl-085-ele
                     at end move  "**"    to   c-sts
                     when   w-tbl-085-key
                           (w-tbl-085-inx)
                                          =    c-sts
                            move  w-tbl-085-tsz
                                 (w-tbl-085-inx)
                                          to   c-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     main-999.
       main-999.
           exit      program                                          .
