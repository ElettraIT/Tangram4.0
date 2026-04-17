       Identification Division.
       Program-Id.                                 mprint             .
      *================================================================*
      *                                                                *
      *                    Interfaccia gestione stampa                 *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/06/91    *
      *                       Ultima revisione:    NdK del 09/06/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *                                                                *
      *================================================================*
      *                                                                *
      * N.B.: il tool esterno 'pdftk' per proteggere i PDF dalla mo-   *
      *       difica, antepone il codice azienda al PDF risultante     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * N.B.: il programma contiene alcune componenti "sperimentali"   *
      *       da completare ...                                        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Begin       Dichiarazione inizio ciclo stampa.                 *
      *                                                                *
      *             Input  : p-ope = "BE"                              *
      *                                                                *
      *                      p-sel = area parametri stampa selezionati *
      *                                                                *
      *             Output : p-pag = numero pagina attuale : zero      *
      *                                                                *
      *                      p-lnr = numero linea  attuale : 1         *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "1" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *                      p-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error code xx      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * End         Dichiarazione fine ciclo stampa.                   *
      *                                                                *
      *             Input  : p-ope = "EN"                              *
      *                                                                *
      *             Output : p-rsc = Status di uscita di i-o           *
      *                              Spaces : OK                       *
      *                              xx     : i-o error code           *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "0" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *                      p-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error code xx      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Page        Avanzamento pagina in ciclo stampa.                *
      * Advance                                                        *
      *             Input  : p-ope = "PA"                              *
      *                                                                *
      *             Output : p-pag = numero pagina attuale : p-pag + 1 *
      *                                                                *
      *                      p-lnr = numero linea  attuale : 1         *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "1" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *                      p-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error code xx      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Eject       Espulsione forzata foglio fisico                   *
      *                                                                *
      *             Input  : p-ope = "EJ"                              *
      *                                                                *
      *             Output : p-pag = numero pagina attuale : invariato *
      *                                                                *
      *                      p-lnr = numero linea  attuale : 1         *
      *                                                                *
      *                      p-res = numero linee  residue : zero      *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "2" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *                      p-rsc = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - xx     : i-o error code xx      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Line Feed   Avanzamento linea in ciclo stampa.                 *
      *                                                                *
      *             Input  : p-ope = "LF"                              *
      *                                                                *
      *             Output : p-lnr = numero linea  attuale : p-lnr + 1 *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "0" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Line Feed   Avanzamento linee in ciclo stampa.                 *
      * Multiplo                                                       *
      *             Input  : p-ope = "L+"                              *
      *                                                                *
      *                      p-lin = numero linee di line-feed         *
      *                                                                *
      *             Output : p-lnr = numero linea  attuale : p-lnr +   *
      *                                                      p-lin     *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "0" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Vertical    Posizionamento a linea in ciclo stampa.            *
      * Positioning                                                    *
      *             Input  : p-ope = "VP"                              *
      *                                                                *
      *                      p-lin = numero linea a cui posizionarsi   *
      *                                                                *
      *             Output : p-lnr = numero linee attuale : p-lin      *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "1" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Vertical    Posizionamento a linea in ciclo stampa subordina-  *
      * Subordinate ta al fatto che p-vpm sia a "0"                    *
      *                                                                *
      *             Input  : p-ope = "VS"                              *
      *                                                                *
      *                      p-lin = numero linea a cui posizionarsi   *
      *                                                                *
      *             Output : p-lnr = numero linee attuale : p-lin      *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "1" *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Print       Editing singolo campo e suo inserimento a linea e  *
      * Field       posizione in ciclo di stampa                       *
      *                                                                *
      *             Input  : p-ope = "PF"                              *
      *                                                                *
      *                      p-tip = tipo campo  : A = alfanumerico    *
      *                                            U = alfa-uppercase  *
      *                                            L = alfa-lowercase  *
      *                                            N = numerico        *
      *                                            V = valore numerico *
      *                                                con decimali in *
      *                                                forma virtuale  *
      *                                            D = data            *
      *                                                                *
      *                      p-car = se A        : numero di caratteri *
      *                                            max 240             *
      *                              se N        : numero di interi    *
      *                                            max 13              *
      *                              se V        : numero di cifre,    *
      *                                            totali, max 13,     *
      *                                            comprensive dei     *
      *                                            decimali virtuali   *
      *                              se D        : non significativo   *
      *                                                                *
      *                      p-dec = se A        : non significativo   *
      *                              se N        : numero di decimali  *
      *                                            max 5               *
      *                              se V        : numero di decimali  *
      *                                            virtuali, max 3     *
      *                              se D        : non significativo   *
      *                                                                *
      *                      p-sgn = se A        : non significativo   *
      *                              se N        : segno algebrico     *
      *                                            S =  con   segno    *
      *                                            altrimenti senza    *
      *                              se V        : segno algebrico     *
      *                                            S =  con   segno    *
      *                                            altrimenti senza    *
      *                              se D        : non significativo   *
      *                                                                *
      *                      p-edm = se A        : non significativo   *
      *                              se N        : attributi editing   *
      *                                            G = raggruppa a tre *
      *                                                a tre i  carat- *
      *                                                teri prima del  *
      *                                                punto decimale  *
      *                                            9 = zeri in testa   *
      *                                            B = blank when zero *
      *                                            D = solo i decimali *
      *                                                significativi   *
      *                                            < = allineamento a  *
      *                                                sinistra        *
      *                                            M = secondo masche- *
      *                                                ra edit p-msk   *
      *                              se V        : come per numerico   *
      *                              se D        : non significativo   *
      *                                            (sempre 99/99/99    *
      *                                             blank when zero)   *
      *                                                                *
      *                      p-msk = se A        : non significativo   *
      *                              se N        : significativo solo  *
      *                                            se p-edm comprende  *
      *                                            il carattere 'M' .  *
      *                                            In questo caso ol-  *
      *                                            tre a 'M' e' ammes- *
      *                                            so solo 'B' .  La   *
      *                                            maschera deve ave-  *
      *                                            re una X per ogni   *
      *                                            intero significati- *
      *                                            va; ogni altro ca-  *
      *                                            rattere viene  in-  *
      *                                            serito brutalmente. *
      *                                            Per esempio con la  *
      *                                            maschera X.X-XXX il *
      *                                            numero 12345 viene  *
      *                                            editato : 1.2-345   *
      *                                            Con la stessa mas-  *
      *                                            chera invece il nu- *
      *                                            mero 345 sarebbe e- *
      *                                            ditato : 0.0-345    *
      *                                            La fine maschera e' *
      *                                            determinata dal pri-*
      *                                            mo carattere spazio.*
      *                                            Per inserire brutal-*
      *                                            mente degli spazi e'*
      *                                            necessario utilizza-*
      *                                            re il carattere 'b' *
      *                              se V        : come per numerico   *
      *                              se D        : non significativo   *
      *                                                                *
      *                      p-lin = linea su cui eseguire l'operazio- *
      *                              ne : 01-96                        *
      *                                                                *
      *                      p-pos = posizione iniziale : 01-240       *
      *                                                                *
      *                      p-alf = se A        : valore del campo    *
      *                                            alfanumerico da     *
      *                                            editare e inserire  *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se D        : non significativo   *
      *                                                                *
      *                      p-num = se A        : non significativo   *
      *                              se N        : valore del campo    *
      *                                            numerico da edita-  *
      *                                            re e inserire       *
      *                              se V        : valore del campo    *
      *                                            numerico da edita-  *
      *                                            re e inserire       *
      *                              se D        : non significativo   *
      *                                                                *
      *                      p-dat = se A        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se D        : valore del campo    *
      *                                            data da editare e   *
      *                                            inserire            *
      *                                                                *
      *                      p-ats = attributi di stampa. Si possono   *
      *                              impostare al massimo cinque at-   *
      *                              tributi di stampa per ogni campo, *
      *                              per un massimo di 20 campi per    *
      *                              linea. Gli attributi selezionabi- *
      *                              li sono, per adesso, i seguenti : *
      *                              - BOLD : Bold                     *
      *                              - UNDL : Underlined               *
      *                              Ogni attributo viene referenziato *
      *                              come p-att(1) , p-att(2) , ecc.   *
      *                                                                *
      *                              N.B.: MAI UTILIZZATI              *
      *                                                                *
      *                                                                *
      *             Output : p-edt = campo editato                     *
      *                                                                *
      *                      p-edl = lunghezza del campo editato       *
      *                                                                *
      *                      p-ats = attributi di stampa a spaces      *
      *                                                                *
      *                      p-vpm = vertical positioning marker : "0" *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Edit        Editing campo alfa o numerico o data.              *
      *                                                                *
      *             Input  : p-ope = "ED"                              *
      *                                                                *
      *                      p-tip = come per Print Field              *
      *                                                                *
      *                      p-car = come per Print Field              *
      *                                                                *
      *                      p-dec = come per Print Field              *
      *                                                                *
      *                      p-sgn = come per Print Field              *
      *                                                                *
      *                      p-edm = come per Print Field              *
      *                                                                *
      *                      p-msk = come per Print Field              *
      *                                                                *
      *                      p-alf   oppure                            *
      *                      p-num   oppure                            *
      *                      p-dat = valore del campo da editare       *
      *                                                                *
      *             Output : p-edt = campo editato                     *
      *                                                                *
      *                      p-edl = lunghezza del campo editato       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Allineam.   Allineamento di una stringa alfanumerica.          *
      *                                                                *
      *             Input  : p-ope = "AA"                              *
      *                                                                *
      *                      p-car = lunghezza stringa da allineare    *
      *                                                                *
      *                      p-edm = tipo allineamento (D,S,C)         *
      *                                                                *
      *                      p-alf = stringa da allineare              *
      *                                                                *
      *             Output : p-alf = stringa allineata                 *
      *                                                                *
      *             N.B.   : POCO UTILIZZATO                           *
      *             -------------------------------------------------- *
      *                                                                *
      * Ultimo      Determinazione dell'ultimo non-blank di una linea. *
      * Non-blank                                                      *
      *             Input  : p-ope = "UN"                              *
      *                                                                *
      *                      p-lin = numero linea da esaminare         *
      *                                                                *
      *             Output : p-pos = posizione nella linea contenente  *
      *                              l'ultimo carattere non-blank      *
      *                                                                *
      *             N.B.   : POCO UTILIZZATO                           *
      *             -------------------------------------------------- *
      *                                                                *
      * Line-Number Salvataggio numero linea attuale in ciclo stampa.  *
      * Save                                                           *
      *             Input  : p-ope = "LS"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Line-Number Ripristino numero linea precedentemente salvata    *
      * Restore     in ciclo stampa.                                   *
      *                                                                *
      *             Input  : p-ope = "LR"                              *
      *                                                                *
      *             Output : p-lnr = ripristinato                      *
      *                                                                *
      *                      p-vpm = ripristinato                      *
      *                                                                *
      *                      p-res = numero linee  residue             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Get Line    Ottenimento immagine linea in ciclo stampa.        *
      *                                                                *
      *             Input  : p-ope = "GL"                              *
      *                                                                *
      *                      p-lin = numero linea interessato          *
      *                                                                *
      *             Output : p-alf = immagine linea numero p-lin       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Convert     Conversione campo alfanumerico in numerico         *
      *                                                                *
      *             Input  : p-ope = "CV"                              *
      *                                                                *
      *                      p-car = numero caratteri del campo alfa-  *
      *                              numerico da convertire            *
      *                                                                *
      *                      p-alf = campo alfanumerico da convertire  *
      *                                                                *
      *             Output : p-num = campo numerico corrispondente     *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [pfc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pfc   assign to disk           f-pfc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pfc-k01
                   alternate record key   is pfc-k02
                   alternate record key   is pfc-k03
                   alternate record key   is pfc-k04
                   alternate record key   is pfc-k05
                             with   duplicates
                             file status  is                f-pfc-sts .
       
      *    *===========================================================*
      *    * File Control [pss]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pss   assign to disk         f-pss-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pss-key
                             file status  is f-pss-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [pfc]                                    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/fls/rec/rfpfc"                          .

      *    *===========================================================*
      *    * File Description [pss]                                    *
      *    *-----------------------------------------------------------*
       fd  pss       label record standard                            .
       01  pss-rec.
           05  pss-key.
               10  pss-tre                pic  x(04)                  .
               10  pss-kre                pic  x(40)                  .
           05  pss-dat.
               10  pss-chr.
                   15  filler      occurs 1536
                                          pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [pfc]                *
      *    *-----------------------------------------------------------*
       01  f-pfc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-pfc-nam                  pic  x(04) value "pfc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-pfc-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-pfc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [pss]                *
      *    *-----------------------------------------------------------*
       01  f-pss.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-pss-nam                  pic  x(04) value "pss "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-pss-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-pss-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work per records di [pss] 'stp'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssstp0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'imp'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssimp0.cpw"                   .

      *    *===========================================================*
      *    * Nome driver stampa effettivo                              *
      *    *-----------------------------------------------------------*
       01  d-pth.
      *        *-------------------------------------------------------*
      *        * Prefisso                                              *
      *        *-------------------------------------------------------*
           05  d-pth-pfi                  pic  x(16)
                   value "swd/drv/prg/obj/"                          .
      *        *-------------------------------------------------------*
      *        * Driver specifico                                      *
      *        *-------------------------------------------------------*
           05  d-pth-drv                  pic  x(10)                  .

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
      *    * Work per record di [prf]                                  *
      *    *-----------------------------------------------------------*
       01  w-prf-rec.
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  w-prf-key.
      *            *---------------------------------------------------*
      *            * Numero pagina                                     *
      *            *---------------------------------------------------*
               10  w-prf-num-pag          pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero linea                                      *
      *            *---------------------------------------------------*
               10  w-prf-num-lin          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo record                                       *
      *            *                                                   *
      *            *  - 'H' : header pagina                            *
      *            *  - 'A' : attributi pagina                         *
      *            *  - 'P' : nuova pagina                             *
      *            *  - 'I' : immagine riga di stampa                  *
      *            *---------------------------------------------------*
               10  w-prf-tip-rec          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  w-prf-dat.
      *            *---------------------------------------------------*
      *            * Primi 240 caratteri                               *
      *            *---------------------------------------------------*
               10  w-prf-dat-240.
                   15  filler occurs 240  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ulteriori 784 caratteri per arrivare a 1024       *
      *            *---------------------------------------------------*
               10  w-prf-dat-784.
                   15  filler occurs 784  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area locale per controllo interruzione stampa        *
      *    *-----------------------------------------------------------*
       01  w-int-stp.
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento, foregorund o background, letto    *
      *        * alla funzione di Begin                                *
      *        *-------------------------------------------------------*
           05  w-int-stp-tip-fun          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area locale per routine di stampa vera e propria     *
      *    *-----------------------------------------------------------*
       01  w-rds.
      *        *-------------------------------------------------------*
      *        * Area per campi di tipo 'V'                            *
      *        *-------------------------------------------------------*
           05  w-rds-tcv                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per Uppercase e Lowercase                        *
      *        *-------------------------------------------------------*
           05  w-rds-upp.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-rds-upr redefines
               w-rds-upp.
               10  w-rds-upc occurs 26    pic  x(01)                  .
           05  w-rds-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-rds-lor redefines
               w-rds-low.
               10  w-rds-loc occurs 26    pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per combinazioni attributi                       *
      *        *-------------------------------------------------------*
           05  w-rds-att.
               10  w-rds-att-cum          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area per editing data in formato 'GG/MM/AA'           *
      *        *-------------------------------------------------------*
           05  w-rds-dat-gma              pic  9(06)                  .
           05  w-rds-dat-gmr redefines
               w-rds-dat-gma.
               10  w-rds-dat-ggg          pic  9(02)                  .
               10  w-rds-dat-mmm          pic  9(02)                  .
               10  w-rds-dat-aaa          pic  9(02)                  .
           05  w-rds-dat-edt.
               10  w-rds-dat-edx          pic  9(02)/9(02)/9(02)
                                                     blank when zero  .
      *        *-------------------------------------------------------*
      *        * Area per editing data in formato 'GG/MM/AAAA'         *
      *        *-------------------------------------------------------*
           05  w-rds-dat-gms              pic  9(08)                  .
           05  w-rds-dat-gmw redefines
               w-rds-dat-gms.
               10  w-rds-dat-wgg          pic  9(02)                  .
               10  w-rds-dat-wmm          pic  9(02)                  .
               10  w-rds-dat-wsc          pic  9(02)                  .
               10  w-rds-dat-waa          pic  9(02)                  .
           05  w-rds-dat-eds.
               10  w-rds-dat-edz          pic  9(02)/9(02)/9(04)
                                                     blank when zero  .
      *        *-------------------------------------------------------*
      *        * Area per editing numerico                             *
      *        *-------------------------------------------------------*
           05  w-rds-rag                  pic  x(01)                  .
           05  w-rds-zit                  pic  x(01)                  .
           05  w-rds-bwz                  pic  x(01)                  .
           05  w-rds-dqs                  pic  x(01)                  .
           05  w-rds-asx                  pic  x(01)                  .
           05  w-rds-msk                  pic  x(01)                  .
           05  w-rds-ned.
               10  w-rds-ned-chr occurs 25
                                          pic  x(01)                  .
           05  w-rds-ne2 redefines
               w-rds-ned.
               10  filler                 pic  x(01)                  .
               10  w-rds-ned-rc9          pic  9.999.999.999.999,99999.
               10  w-rds-ned-rs9 redefines
                   w-rds-ned-rc9          pic  z.zzz.zzz.zzz.zz9,99999.
               10  filler                 pic  x(01)                  .
           05  w-rds-ne3 redefines
               w-rds-ned.
               10  filler                 pic  x(05)                  .
               10  w-rds-ned-nc9          pic  9999999999999,99999    .
               10  w-rds-ned-ns9 redefines
                   w-rds-ned-nc9          pic  zzzzzzzzzzzz9,99999    .
               10  filler                 pic  x(01)                  .
           05  w-rds-nem                  pic  9(13)                  .
           05  w-rds-ney redefines
               w-rds-nem.
               10  w-rds-nex occurs 13    pic  9(01)                  .
           05  w-rds-edt                  pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Area per editing progressivo/anno                     *
      *        *-------------------------------------------------------*
           05  w-rds-prt                  pic  9(13)                  .
           05  w-prr redefines
               w-rds-prt.
               10  w-rds-pts              pic  9(01)                  .
               10  w-rds-pta              pic  9(02)                  .
               10  w-rds-ptx redefines
                   w-rds-pta.
                   15  w-rds-pax occurs 2 pic  9(01)                  .
               10  w-rds-prn              pic  9(10)                  .
           05  w-rds-pmd                  pic  9(11)                  .
           05  w-rds-int                  pic  9(13)                  .
           05  w-rds-ped.
               10  w-rds-ped-pro          pic  z(10)                  .
               10  w-rds-ped-bar          pic  x(01)                  .
               10  w-rds-ped-ann          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Puntatore                                             *
      *        *-------------------------------------------------------*
           05  w-rds-pnt                  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice                                                *
      *        *-------------------------------------------------------*
           05  w-rds-inx                  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore                                             *
      *        *-------------------------------------------------------*
           05  w-rds-ctr                  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Area da 240 caratteri                                 *
      *        *-------------------------------------------------------*
           05  w-rds-240.
               10  filler occurs 240      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area da 1 carattere                                   *
      *        *-------------------------------------------------------*
           05  w-rds-001                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per memorizzazione pathname di 'locprn00'        *
      *        *-------------------------------------------------------*
           05  w-loc-p00                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Area per memorizzazione pathname di 'locprn99'        *
      *        *-------------------------------------------------------*
           05  w-loc-p99                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area locale per stampa immediata, diretta e in spool *
      *    *-----------------------------------------------------------*
       01  w-imm.
      *        *-------------------------------------------------------*
      *        * Marker per open output                                *
      *        *-------------------------------------------------------*
           05  w-imm-flg-opo              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags da passare al driver di stampa al momento del   *
      *        * richiamo della Open del driver                        *
      *        *-------------------------------------------------------*
           05  w-imm-flg-drv.
      *            *---------------------------------------------------*
      *            * Marker per tipo di stampa immediata               *
      *            * - Spaces : diretta                                *
      *            * - S      : in spool                               *
      *            *             - spooler di stampa                   *
      *            *             - remote copy                         *
      *            *             - su stampante locale                 *
      *            *             - su file                             *
      *            *---------------------------------------------------*
               10  w-imm-flg-dos          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di inibizione salto foglio a fine pagina, in *
      *            * caso di stampa in spool o in remote copy o su di  *
      *            * una stampante locale                              *
      *            * - N : Non inibito                                 *
      *            * - S : Inibito                                     *
      *            *---------------------------------------------------*
               10  w-imm-ibz-fff          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Spooler da utilizzare                                 *
      *        *                                                       *
      *        * - "spool" : spooler di stampa                         *
      *        * - "rcp"   : remote copy                               *
      *        * - "ucp"   : tipo 'ucp' - user pathname copy           *
      *        * - "ghs"   : tipo 'ghostscript' - presume 'ps2pdf'     *
      *        * - "fax"   : tipo 'fax' - presume 'hylafax' (sendfax)  *
      *        * - "eml"   : tipo 'eml' - presume agente di invio      *
      *        * - "#"     : stampa locale                             *
      *        *-------------------------------------------------------*
           05  w-imm-spl-dau              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Nome logico stampante per lo spooler                  *
      *        *                                                       *
      *        * - se "spool" : nome logico stampante                  *
      *        * - se "rcp"   : host e filename stampante              *
      *        * - se "ucp"   : pathname                               *
      *        * - se "ghs"   : pathname                               *
      *        * - se "fax"   : pathname                               *
      *        * - se "eml"   : pathname                               *
      *        * - se "#"     : spaces                                 *
      *        *-------------------------------------------------------*
           05  w-imm-nls-dau              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per filename                                   *
      *        *-------------------------------------------------------*
           05  w-imm-fil-dau              pic  x(40)                  .
           05  w-imm-fil-num              pic  x(15)                  .
           05  w-imm-fil-tim              pic  x(05)                  .
           05  w-imm-fil-dst              pic  x(40)                  .
           05  w-imm-fil-ind              pic  x(40)                  .
           05  w-imm-fil-mes              pic  x(08)                  .
           05  w-imm-fil-ogg              pic  x(40)                  .
           05  w-imm-fil-msa              pic  x(40)                  .
           05  w-imm-fil-psm              pic  x(40)                  .
           05  w-imm-fil-ute              pic  x(08)                  .
           05  w-imm-fil-azi              pic  x(04)                  .
           05  w-imm-fil-uid              pic  x(40)                  .
           05  w-imm-fil-pwd              pic  x(20)                  .
           05  w-imm-fil-pth              pic  x(60)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per esecuzione comando                         *
      *        *-------------------------------------------------------*
           05  w-imm-exe-cmd              pic  x(12)                  .
           05  w-imm-exe-pat              pic  x(40)                  .
           05  w-imm-exe-nls              pic  x(40)                  .
           05  w-imm-exe-fil              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per password server SMTP                       *
      *        *-------------------------------------------------------*
           05  w-imm-pdf-psi              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per password di protezione PDF                 *
      *        *-------------------------------------------------------*
           05  w-imm-pdf-pde              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per pathname direttorio files di spool o per   *
      *        * remote copy o per stampante locale                    *
      *        *-------------------------------------------------------*
           05  w-imm-pdb-spl              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo per file di spool o di remote copy *
      *        * o per stampa locale                                   *
      *        *-------------------------------------------------------*
           05  w-imm-prg-fds              pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per pathname completo file di spool, esclusa   *
      *        * l'eventuale estensione per il sub-file di spool       *
      *        *-------------------------------------------------------*
           05  w-imm-pat-spl              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatore per numero sub-file di spool per Eject      *
      *        *-------------------------------------------------------*
           05  w-imm-sub-fds              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area locale per stampa su disco                      *
      *    *-----------------------------------------------------------*
       01  w-dsk.
      *        *-------------------------------------------------------*
      *        * Marker per open output                                *
      *        *-------------------------------------------------------*
           05  w-dsk-flg-opo              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per numero progressivo                         *
      *        *-------------------------------------------------------*
           05  w-dsk-num-prg              pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per data e ora inizio creazione                *
      *        *-------------------------------------------------------*
           05  w-dsk-dat-icr              pic  9(07)                  .
           05  w-dsk-ora-icr              pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per pathname direttorio files stampa           *
      *        *-------------------------------------------------------*
           05  w-dsk-pdb-prf              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatore per numero linea della pagina               *
      *        *-------------------------------------------------------*
           05  w-dsk-ctr-lin              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area locale per aggiornamenti                        *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Flag per apertura file [pfc]                          *
      *        * - Spaces : eseguita correttamente                     *
      *        * - #      : errore in esecuzione                       *
      *        *-------------------------------------------------------*
           05  w-opn-fil-pfc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag per chiusura file [pfc]                          *
      *        * - Spaces : eseguita correttamente                     *
      *        * - #      : errore in esecuzione                       *
      *        *-------------------------------------------------------*
           05  w-cls-fil-pfc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag per prelevamento numero progressivo da [pfc]     *
      *        * - Spaces : eseguito correttamente                     *
      *        * - #      : errore in esecuzione                       *
      *        *-------------------------------------------------------*
           05  w-pre-num-pfc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo da [pfc] incrementato              *
      *        *-------------------------------------------------------*
           05  w-num-num-pfc              pic  9(12)                  .
      *        *-------------------------------------------------------*
      *        * Flag di aggiornamento iniziale per [pfc]              *
      *        * - Spaces : eseguito correttamente                     *
      *        * - #      : errore in esecuzione aggiornamento         *
      *        *-------------------------------------------------------*
           05  w-agg-ini-pfc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di aggiornamento finale per [pfc]                *
      *        * - Spaces : eseguito correttamente                     *
      *        * - #      : errore in esecuzione aggiornamento         *
      *        *-------------------------------------------------------*
           05  w-agg-fin-pfc              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area locale per normalizzazione buffer stampa        *
      *    *-----------------------------------------------------------*
       01  w-nor-buf.
      *        *-------------------------------------------------------*
      *        * Numero di caratteri oltre l'ampiezza massima della    *
      *        * linea di stampa, ed entro il 240mo carattere          
      *        *-------------------------------------------------------*
           05  w-nor-buf-nce              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice del primo carattere eccedente                  *
      *        *-------------------------------------------------------*
           05  w-nor-buf-ipc              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Literal per 240 spaces                                *
      *        *-------------------------------------------------------*
           05  w-nor-spc-240.
               10  filler     occurs 240  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice 01..96                                         *
      *        *-------------------------------------------------------*
           05  w-nor-buf-inx              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Buffer per una linea                                  *
      *        *-------------------------------------------------------*
           05  w-nor-lin-240.
               10  filler     occurs 240  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area locale per rilascio stampante                   *
      *    *-----------------------------------------------------------*
       01  w-ril.
      *        *-------------------------------------------------------*
      *        * Flag di rilascio stampante eseguito                   *
      *        *-------------------------------------------------------*
           05  w-ril-flg-ril              pic  x(01) value spaces     .

      *    *===========================================================*
      *    * Work-area locale per chiamata al system spooler           *
      *    *-----------------------------------------------------------*
       01  w-sys-spl.
      *        *-------------------------------------------------------*
      *        * Default per il template dello spooler di stampa       *
      *        * per lo spooler di stampa, a 'lpr'                     *
      *        *-------------------------------------------------------*
           05  w-sys-spl-cmd-def          pic  x(60) value
                     "(p=@p; f=@f; cat $f | lpr -P $p -h -l -s; rm -f $f
      -              ") &       "                                     .
      *        *-------------------------------------------------------*
      *        * Area per la personalizzazione relativa al template    *
      *        * per lo spooler di stampa                              *
      *        *-------------------------------------------------------*
           05  w-sys-spl-cmd-tem.
               10  w-sys-spl-cmd-tex
                               occurs 60  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per composizione comando di sistema              *
      *        *-------------------------------------------------------*
           05  w-sys-spl-cmd.
      *            *---------------------------------------------------*
      *            * Codice della stampante in 'printcap'              *
      *            *---------------------------------------------------*
               10  w-sys-spl-cmd-cst      pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Pathname del file da stampare                     *
      *            *---------------------------------------------------*
               10  w-sys-spl-cmd-pat      pic  x(60)                  .
      *            *---------------------------------------------------*
      *            * Redirezione output ed errori                      *
      *            *---------------------------------------------------*
               10  w-sys-spl-cmd-roe      pic  x(30) value
                     " 1>/dev/null 2>&1             "                 .
      *        *-------------------------------------------------------*
      *        * Contatori, indici, puntatori, locali                  *
      *        *-------------------------------------------------------*
           05  w-sys-spl-inx-tem          pic  9(03)                  .
           05  w-sys-spl-pnt-tem          pic  9(03)                  .
           05  w-sys-spl-inx-hpr          pic  9(03)                  .
           05  w-sys-spl-ctr-001          pic  9(03)                  .
           05  w-sys-spl-ctr-002          pic  9(03)                  .
           05  w-sys-spl-wkx-060          pic  x(60)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Field                                                 *
      *        *-------------------------------------------------------*
           05  w-fld.
               10  w-cho occurs 133.
                   15  w-chr              pic  x(01)                  .
                   15  w-chn redefines
                       w-chr              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo per usi locali                    *
      *        *-------------------------------------------------------*
           05  w-ctr                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per parte intera di un numerico                *
      *        *-------------------------------------------------------*
           05  w-int                      pic  9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per parte decimale di un numerico              *
      *        *-------------------------------------------------------*
           05  w-dec                      pic  9(05)                  .
           05  w-dcr redefines w-dec.
               10  w-dcc occurs 05        pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di decimali gia' impostati in un numerico      *
      *        *-------------------------------------------------------*
           05  w-dim                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per valore numerico                            *
      *        *-------------------------------------------------------*
           05  w-num                      pic s9(13)v9(05)            .
           05  w-num-cnv                  pic s9(13)v9(02)            .
           05  w-sgn                      pic  x(01)                  .

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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      ******************************************************************
       Procedure Division                 using p                     .
      ******************************************************************

      *================================================================*
      *      Declaratives                                              *
      *================================================================*
       Declaratives.
      *    *===========================================================*
      *    * [pfc]                                                     *
      *    *-----------------------------------------------------------*
       Decl-pfc Section.
           Use after standard error procedure on pfc.
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status ritornato  *
      *              * dal cobol nel codice di i-o status convenziona- *
      *              * le                                              *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-pfc                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-pfc-sts            to   e-sts                  .
      *    *===========================================================*
      *    * [pss]                                                     *
      *    *-----------------------------------------------------------*
       Decl-pss Section.
           Use after standard error procedure on pss.
       decl-pss-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status ritornato  *
      *              * dal cobol nel codice di i-o status convenziona- *
      *              * le                                              *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-pss                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-pss-sts            to   e-sts                  .
       End Declaratives.

      *================================================================*
      *    Main                                                        *
      *================================================================*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione return status code              *
      *              *-------------------------------------------------*
           move      spaces               to   p-rsc                  .
       main-200.
      *              *-------------------------------------------------*
      *              * Pre-intervento per tipo campo 'V''              *
      *              *-------------------------------------------------*
           perform   pre-icv-000          thru pre-icv-999            .
       main-400.
      *              *-------------------------------------------------*
      *              * Test su tipo operazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Print Field                                 *
      *                  *---------------------------------------------*
           if        p-ope                =    "PF"
                     perform prf-000      thru prf-999
      *                  *---------------------------------------------*
      *                  * Line Feed Singolo                           *
      *                  *---------------------------------------------*
           else if   p-ope                =    "LF"
                     perform lfd-000      thru lfd-999
      *                  *---------------------------------------------*
      *                  * Line Feed Multiplo                          *
      *                  *---------------------------------------------*
           else if   p-ope                =    "L+"
                     perform lfm-000      thru lfm-999
      *                  *---------------------------------------------*
      *                  * Vertical Positioning                        *
      *                  *---------------------------------------------*
           else if   p-ope                =    "VP"
                     perform vpo-000      thru vpo-999
      *                  *---------------------------------------------*
      *                  * Vertical Positioning Subordinate            *
      *                  *---------------------------------------------*
           else if   p-ope                =    "VS"
                     perform vsu-000      thru vsu-999
      *                  *---------------------------------------------*
      *                  * Edit                                        *
      *                  *---------------------------------------------*
           else if   p-ope                =    "ED"
                     perform edt-000      thru edt-999
      *                  *---------------------------------------------*
      *                  * Allineamento stringa alfanumerica           *
      *                  *---------------------------------------------*
           else if   p-ope                =    "AA"
                     perform asa-000      thru asa-999
      *                  *---------------------------------------------*
      *                  * Page Advance                                *
      *                  *---------------------------------------------*
           else if   p-ope                =    "PA"
                     perform adp-000      thru adp-999
      *                  *---------------------------------------------*
      *                  * Eject                                       *
      *                  *---------------------------------------------*
           else if   p-ope                =    "EJ"
                     perform ejc-000      thru ejc-999
      *                  *---------------------------------------------*
      *                  * Determinazione posizione ultimo non-blank   *
      *                  *---------------------------------------------*
           else if   p-ope                =    "UN"
                     perform unb-000      thru unb-999
      *                  *---------------------------------------------*
      *                  * Line-number Save                            *
      *                  *---------------------------------------------*
           else if   p-ope                =    "LS"
                     perform lns-000      thru lns-999
      *                  *---------------------------------------------*
      *                  * Line-number Restore                         *
      *                  *---------------------------------------------*
           else if   p-ope                =    "LR"
                     perform lnr-000      thru lnr-999
      *                  *---------------------------------------------*
      *                  * Get Line                                    *
      *                  *---------------------------------------------*
           else if   p-ope                =    "GL"
                     perform gtl-000      thru gtl-999
      *                  *---------------------------------------------*
      *                  * Begin                                       *
      *                  *---------------------------------------------*
           else if   p-ope                =    "BE"
                     perform beg-000      thru beg-999
      *                  *---------------------------------------------*
      *                  * End                                         *
      *                  *---------------------------------------------*
           else if   p-ope                =    "EN"
                     perform end-000      thru end-999
      *                  *---------------------------------------------*
      *                  * Convert                                     *
      *                  *---------------------------------------------*
           else if   p-ope                =    "CV"
                     perform cvt-000      thru cvt-999                .
       main-600.
      *              *-------------------------------------------------*
      *              * Post-intervento per tipo campo 'V''             *
      *              *-------------------------------------------------*
           perform   pos-icv-000          thru pos-icv-999            .
       main-999.
           exit      program                                          .

      *================================================================*
      *    Begin                                                       *
      *----------------------------------------------------------------*
       beg-000.
      *              *-------------------------------------------------*
      *              * Composizione nome driver di stampa effettivo    *
      *              *-------------------------------------------------*
           move      p-sel-drv-tst        to   d-pth-drv              .
       beg-010.
      *              *-------------------------------------------------*
      *              * Determinazione se funzionamento in foreground o *
      *              * in background                                   *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-fun                to   w-int-stp-tip-fun      .
      *              *-------------------------------------------------*
      *              * Flag di rilascio stampante eseguito a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-ril-flg-ril          .
      *              *-------------------------------------------------*
      *              * Marker generico per ciclo stampa a spaces       *
      *              *-------------------------------------------------*
           move      spaces               to   p-mrk                  .
      *              *-------------------------------------------------*
      *              * Flag errori di stampa a spaces                  *
      *              *-------------------------------------------------*
           move      spaces               to   p-err                  .
      *              *-------------------------------------------------*
      *              * Numero pagina attuale : zero                    *
      *              *-------------------------------------------------*
           move      zero                 to   p-pag                  .
      *              *-------------------------------------------------*
      *              * Numero linea  attuale : 1                       *
      *              *-------------------------------------------------*
           move      1                    to   p-lnr                  .
      *              *-------------------------------------------------*
      *              * Numero linee  residue                           *
      *              *-------------------------------------------------*
           move      p-sel-efc-sel        to   p-res                  .
      *              *-------------------------------------------------*
      *              * Livello di profondita' per Line-numbers Save e  *
      *              * Restore a zero                                  *
      *              *-------------------------------------------------*
           move      zero                 to   p-qlp                  .
      *              *-------------------------------------------------*
      *              * Vertical positioning marker in on               *
      *              *-------------------------------------------------*
           move      "1"                  to   p-vpm                  .
      *              *-------------------------------------------------*
      *              * Attributi di stampa a spaces                    *
      *              *-------------------------------------------------*
           move      spaces               to   p-ats                  .
       beg-100.
      *              *-------------------------------------------------*
      *              * Eventuale forzatura del canale di stampa in ca- *
      *              * so di stampante locale                          *
      *              *-------------------------------------------------*
           if        p-sel-stp-sel (1 : 1)
                                          =    "#"
                     move  "#"            to   p-sel-can-stp          .
      *              *-------------------------------------------------*
      *              * Separazione del parametro 'canale di stampa' in *
      *              * una o due componenti                            *
      *              *                                                 *
      *              * - Se una sola componente :                      *
      *              *      - w-imm-spl-dau = canale di stampa         *
      *              *      - w-imm-nls-dau = spaces                   *
      *              *                                                 *
      *              * - Se due componenti :                           *
      *              *      - w-imm-spl-dau = spooler da utilizzare    *
      *              *                        - "spool"                *
      *              *                        - "rcp"                  *
      *              *                        - "ucp"                  *
      *              *                        - "ghs"                  *
      *              *                        - "fax"                  *
      *              *                        - "eml"                  *
      *              *      - w-imm-nls-dau = nome logico stampante    *
      *              *                        per lo spooler o remo-   *
      *              *                        te copy                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-imm-spl-dau          .
           move      spaces               to   w-imm-nls-dau          .
           unstring  p-sel-can-stp
                                delimited by   all spaces
                                          into w-imm-spl-dau
                                               w-imm-nls-dau          .
      *              *-------------------------------------------------*
      *              * Eventuale recupero del file name                *
      *              *-------------------------------------------------*
           move      p-sel-fut-nam        to   w-imm-fil-dau          .
      *              *-------------------------------------------------*
      *              * Eventuale recupero di parametri 'fax'           *
      *              *-------------------------------------------------*
           move      p-sel-fut-nam        to   w-imm-fil-num          .
           move      p-sel-fut-nam
                    (16 : 02)             to   w-imm-fil-tim          .
           move      ":"                  to   w-imm-fil-tim
                                              (03 : 01)               .
           move      p-sel-fut-nam
                    (18 : 02)             to   w-imm-fil-tim
                                              (04 : 02)               .
           move      p-sel-fut-exp        to   w-imm-fil-dst          .
      *              *-------------------------------------------------*
      *              * Eventuale recupero di parametri 'eml'           *
      *              *-------------------------------------------------*
           move      p-sel-fut-nam        to   w-imm-fil-ind          .
           move      p-sel-fut-exp        to   w-imm-fil-ogg          .
      *              *-------------------------------------------------*
      *              * Eliminazione spazi vuoti                        *
      *              * ___ DA RIVEDERE ___                             *
      *              *-------------------------------------------------*
           inspect   w-imm-fil-ogg
                                replacing all spaces
                                          by  "_"                     .
      *              *-------------------------------------------------*
      *              * Assemblaggio nome file messaggio per 'eml'      *
      *              *-------------------------------------------------*
           move      08                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "m"                  to   w-all-str-cat (1)      .
           move      p-sel-are-ges        to   w-all-str-cat (2)      .
           move      ".txt"               to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-imm-fil-mes          .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa alla   *
      *              * password per il server SMTP                     *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "pdf-psi"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-pdf-psi          .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa alla   *
      *              * password di protezione dei documenti PDF        *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "pdf-pde"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-pdf-pde          .
       beg-200.
      *              *-------------------------------------------------*
      *              * Flag di stampa diretta o in spool               *
      *              *-------------------------------------------------*
           if        w-imm-spl-dau        =    "spool" or
                     w-imm-spl-dau        =    "rcp"   or
                     w-imm-spl-dau        =    "ucp"   or
                     w-imm-spl-dau        =    "ghs"   or
                     w-imm-spl-dau        =    "fax"   or
                     w-imm-spl-dau        =    "eml"   or
                     w-imm-spl-dau        =    "#"
                     move  "S"            to   w-imm-flg-dos
           else      move  spaces         to   w-imm-flg-dos          .
       beg-300.
      *              *-------------------------------------------------*
      *              * Lettura flag di inibizione salto pagina a fine  *
      *              * stampa in caso di stampa in spool, associato al *
      *              * codice stampante                                *
      *              *-------------------------------------------------*
           perform   ibz-fff-stp-000      thru ibz-fff-stp-999        .
       beg-400.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al     *
      *              * template per lo spooler di stampa, con eventua- *
      *              * le normalizzazione con il default               *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "tem-spl"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-sys-spl-cmd-tem      .
           if        w-sys-spl-cmd-tem    =    spaces
                     move  w-sys-spl-cmd-def
                                          to   w-sys-spl-cmd-tem      .
       beg-500.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di stampa       *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        =    "I" or
                     p-sel-tds-sel        =    "F"
                     go to beg-600
           else if   p-sel-tds-sel        =    "D" or
                     p-sel-tds-sel        =    "V"
                     go to beg-700
           else      go to beg-999.
       beg-600.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : immediata                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open output per stampa immediata            *
      *                  *---------------------------------------------*
           perform   imm-opn-out-000      thru imm-opn-out-999        .
           go to     beg-900.
       beg-700.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : su disco o a video             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open output file su disco                   *
      *                  *---------------------------------------------*
           perform   dsk-opn-out-000      thru dsk-opn-out-999        .
           go to     beg-900.
       beg-900.
      *              *-------------------------------------------------*
      *              * Se errori : rilascio impegno stampante          *
      *              *-------------------------------------------------*
           if        p-rsc                =    spaces
                     go to beg-999.
           perform   ril-imp-stp-000      thru ril-imp-stp-999        .
       beg-999.
           exit.

      *================================================================*
      *    End                                                         *
      *----------------------------------------------------------------*
       end-000.
      *              *-------------------------------------------------*
      *              * Composizione nome driver di stampa effettivo    *
      *              *-------------------------------------------------*
           move      p-sel-drv-tst        to   d-pth-drv              .
      *              *-------------------------------------------------*
      *              * Se flag errori di stampa a non-spaces non si e- *
      *              * segue alcuna operazione                         *
      *              *-------------------------------------------------*
           if        p-err                not  = spaces
                     go to end-800.
      *              *-------------------------------------------------*
      *              * Test su tipo di stampa                          *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        =    "I" or
                     p-sel-tds-sel        =    "F"
                     go to end-400
           else if   p-sel-tds-sel        =    "D" or
                     p-sel-tds-sel        =    "V"
                     go to end-600
           else      go to end-800.
       end-400.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : immediata                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close output per stampa immediata           *
      *                  *---------------------------------------------*
           perform   imm-cls-out-000      thru imm-cls-out-999        .
      *                  *---------------------------------------------*
      *                  * Ad assestamenti finali                      *
      *                  *---------------------------------------------*
           go to     end-800.
       end-600.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : su disco o a video             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close output su disco                       *
      *                  *---------------------------------------------*
           perform   dsk-cls-out-000      thru dsk-cls-out-999        .
       end-800.
      *              *-------------------------------------------------*
      *              * Assestamenti per ogni tipo di stampa            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attributi di stampa a spaces                *
      *                  *---------------------------------------------*
           move      spaces               to   p-ats                  .
      *                  *---------------------------------------------*
      *                  * Vertical positioning marker in off          *
      *                  *---------------------------------------------*
           move      "0"                  to   p-vpm                  .
       end-900.
      *              *-------------------------------------------------*
      *              * Rilascio impegno stampante                      *
      *              *-------------------------------------------------*
           perform   ril-imp-stp-000      thru ril-imp-stp-999        .
       end-925.
      *              *-------------------------------------------------*
      *              * Se stampa immediata : cancellazione driver      *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        not  = "I" and
                     p-sel-tds-sel        not  = "F"
                     go to end-950.
           cancel    d-pth                                            .
       end-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     end-999.
       end-999.
           exit.

      *================================================================*
      *    Page Advance                                                *
      *----------------------------------------------------------------*
       adp-000.
      *              *-------------------------------------------------*
      *              * Se l'ultima operazione eseguita e' stata eject  *
      *              * non si esegue alcuna operazione, solo assesta-  *
      *              * menti per nuova pagina                          *
      *              *-------------------------------------------------*
           if        p-vpm                =    "2"
                     go to adp-800.
      *              *-------------------------------------------------*
      *              * Se flag errori di stampa a non-spaces non si e- *
      *              * segue alcuna operazione                         *
      *              *-------------------------------------------------*
           if        p-err                not  = spaces
                     go to adp-999.
       adp-200.
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata da operatore       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo funzionamento non in foreground :   *
      *                  * nessun controllo                            *
      *                  *---------------------------------------------*
           if        w-int-stp-tip-fun    not  = "F"
                     go to adp-400.
      *                  *---------------------------------------------*
      *                  * Richiesta al modulo di interfaccia se in-   *
      *                  * tervenuta interruzione da parte dell'opera- *
      *                  * tore oppure no.                             *
      *                  *---------------------------------------------*
           move      "<>"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Se non c'e' stata interruzione si continua  *
      *                  * normalmente                                 *
      *                  *---------------------------------------------*
           if        o-sts                =    spaces
                     go to adp-400.
      *                  *---------------------------------------------*
      *                  * Refresh video automatico                    *
      *                  *---------------------------------------------*
           move      "RF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Salvataggio video                           *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box interno                                 *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      10                   to   v-lin                  .
           move      20                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      61                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio interno al box                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "Interruzione stampa in esecuzione ..."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Attesa di 5 secondi per dar modo di vedere  *
      *                  * il messaggio                                *
      *                  *---------------------------------------------*
           move      "WT"                 to   s-ope                  .
           move      05                   to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Ripristino video                            *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Flag di errore in On                        *
      *                  *---------------------------------------------*
           move      "#"                  to   p-rsc                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     adp-999.
       adp-400.
      *              *-------------------------------------------------*
      *              * Test su tipo di stampa                          *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        =    "I" or
                     p-sel-tds-sel        =    "F"
                     go to adp-500
           else if   p-sel-tds-sel        =    "D" or
                     p-sel-tds-sel        =    "V"
                     go to adp-600
           else      go to adp-700.
       adp-500.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : immediata                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Write Page per stampa immediata             *
      *                  *---------------------------------------------*
           perform   imm-wrt-pag-000      thru imm-wrt-pag-999        .
      *                  *---------------------------------------------*
      *                  * A preparazioni per pagina successiva        *
      *                  *---------------------------------------------*
           go to     adp-700.
       adp-600.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : su disco o a video             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Write Page su disco                         *
      *                  *---------------------------------------------*
           perform   dsk-wrt-pag-000      thru dsk-wrt-pag-999        .
       adp-700.
      *              *-------------------------------------------------*
      *              * Preparazioni per pagina successiva              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero linea  attuale : 1                   *
      *                  *---------------------------------------------*
           move      1                    to   p-lnr                  .
      *                  *---------------------------------------------*
      *                  * Numero linee  residue                       *
      *                  *---------------------------------------------*
           move      p-sel-efc-sel        to   p-res                  .
      *                  *---------------------------------------------*
      *                  * Buffer immagine pagina di stampa a spaces   *
      *                  *---------------------------------------------*
           move      spaces               to   p-buf                  .
      *                  *---------------------------------------------*
      *                  * Buffer attributi pagina di stampa a spaces  *
      *                  *---------------------------------------------*
           move      spaces               to   p-bat                  .
      *                  *---------------------------------------------*
      *                  * Attributi di stampa a spaces                *
      *                  *---------------------------------------------*
           move      spaces               to   p-ats                  .
       adp-800.
      *              *-------------------------------------------------*
      *              * Preparazioni per nuova pagina                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina                    *
      *                  *---------------------------------------------*
           add       1                    to   p-pag                  .
      *                  *---------------------------------------------*
      *                  * Vertical positioning marker in on           *
      *                  *---------------------------------------------*
           move      "1"                  to   p-vpm                  .
       adp-999.
           exit.

      *================================================================*
      *    Eject                                                       *
      *----------------------------------------------------------------*
       ejc-000.
      *              *-------------------------------------------------*
      *              * Se l'ultima operazione eseguita e' stata eject  *
      *              * non si esegue alcuna operazione                 *
      *              *-------------------------------------------------*
           if        p-vpm                =    "2"
                     go to ejc-999.
      *              *-------------------------------------------------*
      *              * Se flag errori di stampa a non-spaces non si e- *
      *              * segue alcuna operazione                         *
      *              *-------------------------------------------------*
           if        p-err                not  = spaces
                     go to ejc-800.
      *              *-------------------------------------------------*
      *              * Test su tipo di stampa                          *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        =    "I" or
                     p-sel-tds-sel        =    "F"
                     go to ejc-400
           else if   p-sel-tds-sel        =    "D" or
                     p-sel-tds-sel        =    "V"
                     go to ejc-600
           else      go to ejc-800.
       ejc-400.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : immediata                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eject Page per stampa immediata             *
      *                  *---------------------------------------------*
           perform   imm-ejc-pag-000      thru imm-ejc-pag-999        .
      *                  *---------------------------------------------*
      *                  * A preparazioni per pagina successiva        *
      *                  *---------------------------------------------*
           go to     ejc-800.
       ejc-600.
      *              *-------------------------------------------------*
      *              * Tipo di stampa : su disco o a video             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eject Page su disco                         *
      *                  *---------------------------------------------*
           perform   dsk-ejc-pag-000      thru dsk-ejc-pag-999        .
       ejc-800.
      *              *-------------------------------------------------*
      *              * Preparazioni per pagina successiva              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero linea  attuale : 1                   *
      *                  *---------------------------------------------*
           move      1                    to   p-lnr                  .
      *                  *---------------------------------------------*
      *                  * Numero linee  residue                       *
      *                  *---------------------------------------------*
           move      p-sel-efc-sel        to   p-res                  .
      *                  *---------------------------------------------*
      *                  * Buffer immagine pagina di stampa a spaces   *
      *                  *---------------------------------------------*
           move      spaces               to   p-buf                  .
      *                  *---------------------------------------------*
      *                  * Buffer attributi pagina di stampa a spaces  *
      *                  *---------------------------------------------*
           move      spaces               to   p-bat                  .
      *                  *---------------------------------------------*
      *                  * Vertical positioning marker in eject-state  *
      *                  *---------------------------------------------*
           move      "2"                  to   p-vpm                  .
      *                  *---------------------------------------------*
      *                  * Attributi di stampa a spaces                *
      *                  *---------------------------------------------*
           move      spaces               to   p-ats                  .
       ejc-900.
      *              *-------------------------------------------------*
      *              * Se errori : rilascio impegno stampante          *
      *              *-------------------------------------------------*
           if        p-rsc                =    spaces
                     go to ejc-999.
           perform   ril-imp-stp-000      thru ril-imp-stp-999        .
       ejc-999.
           exit.

      *================================================================*
      *    Line Feed Singolo                                           *
      *----------------------------------------------------------------*
       lfd-000.
      *              *-------------------------------------------------*
      *              * Line Feed Multiplo di una linea                 *
      *              *-------------------------------------------------*
           move      1                    to   p-lin                  .
           perform   lfm-000              thru lfm-999                .
       lfd-999.
           exit.

      *================================================================*
      *    Line Feed Multiplo                                          *
      *----------------------------------------------------------------*
       lfm-000.
      *              *-------------------------------------------------*
      *              * Se vert. posit. marker non a "0" : uscita       *
      *              *-------------------------------------------------*
           if        p-vpm                not  = "0"
                     go to lfm-500.
      *              *-------------------------------------------------*
      *              * Incremento numero linea  attuale                *
      *              *-------------------------------------------------*
           add       p-lin                to   p-lnr                  .
      *              *-------------------------------------------------*
      *              * Determinazione numero linee residue             *
      *              *-------------------------------------------------*
           subtract  p-lnr                from p-sel-efc-sel
                                        giving p-res                  .
           add       1                    to   p-res                  .
       lfm-500.
      *              *-------------------------------------------------*
      *              * Vertical positioning marker in off              *
      *              *-------------------------------------------------*
           move      "0"                  to   p-vpm                  .
      *              *-------------------------------------------------*
      *              * Attributi di stampa a spaces                    *
      *              *-------------------------------------------------*
           move      spaces               to   p-ats                  .
       lfm-999.
           exit.

      *================================================================*
      *    Vertical Positioning                                        *
      *----------------------------------------------------------------*
       vpo-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento numero linea attuale              *
      *              *-------------------------------------------------*
           move      p-lin                to   p-lnr                  .
      *              *-------------------------------------------------*
      *              * Determinazione numero linee residue             *
      *              *-------------------------------------------------*
           subtract  p-lnr                from p-sel-efc-sel
                                        giving p-res                  .
           add       1                    to   p-res                  .
      *              *-------------------------------------------------*
      *              * Vertical positioning marker in on               *
      *              *-------------------------------------------------*
           move      "1"                  to   p-vpm                  .
      *              *-------------------------------------------------*
      *              * Attributi di stampa a spaces                    *
      *              *-------------------------------------------------*
           move      spaces               to   p-ats                  .
       vpo-999.
           exit.

      *================================================================*
      *    Vertical Positioning Subordinate                            *
      *----------------------------------------------------------------*
       vsu-000.
      *              *-------------------------------------------------*
      *              * Se vert. posit. marker non a "0" : uscita       *
      *              *-------------------------------------------------*
           if        p-vpm                not  = "0"
                     move  "1"            to   p-vpm
                     go to vsu-999.
      *              *-------------------------------------------------*
      *              * Vertical Positioning                            *
      *              *-------------------------------------------------*
           perform   vpo-000              thru vpo-999                .
       vsu-999.
           exit.

      *================================================================*
      *    Print Field                                                 *
      *----------------------------------------------------------------*
       prf-000.
      *              *-------------------------------------------------*
      *              * Esecuzione editing                              *
      *              *-------------------------------------------------*
           perform   edt-000              thru edt-999                .
      *              *-------------------------------------------------*
      *              * Se numero linea di stampa non valido : uscita   *
      *              *-------------------------------------------------*
           if        p-lin                =    zero       or
                     p-lin                >    p-sel-efc-sel
                     go to prf-999.
      *              *-------------------------------------------------*
      *              * Spostamento dell'immagine editata nel buffer    *
      *              * della pagina di stampa                          *
      *              *-------------------------------------------------*
           add       1,
                     p-edl              giving w-rds-pnt              .
           move      high-value           to   p-edx
                                              (w-rds-pnt)             .
           move      p-pos                to   w-rds-inx              .
           string    p-edt      delimited by   high-value
                                          into p-buf-lin (p-lin)
                                  with pointer w-rds-inx              .
           move      spaces               to   p-edx
                                              (w-rds-pnt)             .
      *              *-------------------------------------------------*
      *              * Se nessun attributo di stampa : uscita          *
      *              *-------------------------------------------------*
           if        p-ats                =    spaces
                     go to prf-950.
      *              *-------------------------------------------------*
      *              * Memorizzazione degli attributi di stampa        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca prima posizione libera su attributi *
      *                  * per linea                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione puntatore              *
      *                      *-----------------------------------------*
           move      zero                 to   w-rds-pnt              .
       prf-100.
      *                      *-----------------------------------------*
      *                      * Incremento puntatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-rds-pnt              .
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo : uscita            *
      *                      *-----------------------------------------*
           if        w-rds-pnt            >    30
                     go to prf-900.
      *                      *-----------------------------------------*
      *                      * Se posizione gia' impegnata : riciclo   *
      *                      *-----------------------------------------*
           if        p-bat-def
                    (p-lin, w-rds-pnt)    not  = spaces
                     go to prf-100.
      *                  *---------------------------------------------*
      *                  * Inizializzazione cumulativo attributi       *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-att-cum          .
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore numero attributi *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-ctr              .
       prf-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero attributi       *
      *                  *---------------------------------------------*
           add       1                    to   w-rds-ctr              .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : uscita                *
      *                  *---------------------------------------------*
           if        w-rds-ctr            >    5
                     go to prf-300.
      *                  *---------------------------------------------*
      *                  * Se attributo a spaces : riciclo             *
      *                  *---------------------------------------------*
           if        p-att(w-rds-ctr)     =    spaces
                     go to prf-200.
      *                  *---------------------------------------------*
      *                  * Cumulo attributo                            *
      *                  *---------------------------------------------*
           if        p-att(w-rds-ctr)     =    "BOLD"
                     add   1              to   w-rds-att-cum
           else if   p-att(w-rds-ctr)     =    "UNDL"
                     add   2              to   w-rds-att-cum          .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     prf-200.
       prf-300.
      *                  *---------------------------------------------*
      *                  * Se nessun attributo valido : uscita         *
      *                  *---------------------------------------------*
           if        w-rds-att-cum        =    zero
                     go to prf-900.
      *                  *---------------------------------------------*
      *                  * Memorizzazione posizione iniziale           *
      *                  *---------------------------------------------*
           move      p-pos                to   p-bat-min
                                              (p-lin, w-rds-pnt)      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione posizione finale             *
      *                  *---------------------------------------------*
           move      p-pos                to   p-bat-max
                                              (p-lin, w-rds-pnt)      .
           add       p-edl                to   p-bat-max
                                              (p-lin, w-rds-pnt)      .
           subtract  1                    from p-bat-max
                                              (p-lin, w-rds-pnt)      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione cumulo attributi             *
      *                  *---------------------------------------------*
           move      w-rds-att-cum        to   p-bat-att
                                              (p-lin, w-rds-pnt)      .
       prf-900.
      *              *-------------------------------------------------*
      *              * Attributi di stampa a spaces                    *
      *              *-------------------------------------------------*
           move      spaces               to   p-ats                  .
       prf-950.
      *              *-------------------------------------------------*
      *              * Vertical positioning marker in off              *
      *              *-------------------------------------------------*
           move      "0"                  to   p-vpm                  .
       prf-999.
           exit.

      *================================================================*
      *    Pre-intervento per tipo campo 'V'                           *
      *----------------------------------------------------------------*
       pre-icv-000.
      *              *-------------------------------------------------*
      *              * Pre-intervento per tipo campo 'V''              *
      *              *-------------------------------------------------*
           if       (p-tip                not  = "V"    ) or
                    (p-ope                not  = "ED" and
                     p-ope                not  = "PF"   )
                     move  spaces         to   w-rds-tcv
                     go to pre-icv-999.
      *
           move      "#"                  to   w-rds-tcv              .
           move      "N"                  to   p-tip                  .
           subtract  p-dec                from p-car                  .
      *
           if        p-dec                =    1
                     divide 10            into p-num
           else if   p-dec                =    2
                     divide 100           into p-num
           else if   p-dec                =    3
                     divide 1000          into p-num
           else if   p-dec                =    4
                     divide 10000         into p-num
           else if   p-dec                =    5
                     divide 100000        into p-num                  .
       pre-icv-999.
           exit.

      *================================================================*
      *    Post-intervento per tipo campo 'V'                          *
      *----------------------------------------------------------------*
       pos-icv-000.
      *              *-------------------------------------------------*
      *              * Post-intervento per tipo campo 'V''             *
      *              *-------------------------------------------------*
           if        w-rds-tcv            =    spaces
                     go to pos-icv-999.
           move      "V"                  to   p-tip                  .
           add       p-dec                to   p-car                  .
           if        p-dec                =    1
                     multiply 10          by   p-num
           else if   p-dec                =    2
                     multiply 100         by   p-num
           else if   p-dec                =    3
                     multiply 1000        by   p-num
           else if   p-dec                =    4
                     multiply 10000       by   p-num
           else if   p-dec                =    5
                     multiply 100000      by   p-num                  .
       pos-icv-999.
           exit.

      *================================================================*
      *    Edit                                                        *
      *----------------------------------------------------------------*
       edt-000.
      *              *-------------------------------------------------*
      *              * Se alfanumerico                                 *
      *              *-------------------------------------------------*
           if        p-tip                =    "A"
                     perform edt-alf-000  thru edt-alf-999
      *              *-------------------------------------------------*
      *              * Se alfanumerico uppercase                       *
      *              *-------------------------------------------------*
           else if   p-tip                =    "U"
                     perform edt-upp-000  thru edt-upp-999
      *              *-------------------------------------------------*
      *              * Se alfanumerico lowercase                       *
      *              *-------------------------------------------------*
           else if   p-tip                =    "L"
                     perform edt-low-000  thru edt-low-999
      *              *-------------------------------------------------*
      *              * Se numerico                                     *
      *              *-------------------------------------------------*
           else if   p-tip                =    "N"
                     perform edt-num-000  thru edt-num-999
      *              *-------------------------------------------------*
      *              * Se progressivo/anno                             *
      *              *-------------------------------------------------*
           else if   p-tip                =    "P"
                     perform edt-pga-000  thru edt-pga-999
      *              *-------------------------------------------------*
      *              * Se data                                         *
      *              *-------------------------------------------------*
           else if   p-tip                =    "D"
                     perform edt-dat-000  thru edt-dat-999            .
       edt-999.
           exit.

      *================================================================*
      *    Edit alfanumerico                                           *
      *----------------------------------------------------------------*
       edt-alf-000.
           move      p-alf                to   p-edt                  .
           move      p-car                to   p-edl                  .
       edt-alf-999.
           exit.

      *================================================================*
      *    Edit alfanumerico uppercase                                 *
      *----------------------------------------------------------------*
       edt-upp-000.
           move      p-alf                to   p-edt                  .
           move      p-car                to   p-edl                  .
           move      p-car                to   w-rds-ctr              .
       edt-upp-200.
           subtract  1                    from w-rds-ctr              .
           if        w-rds-ctr            =    zero
                     go to edt-upp-999.
           move      zero                 to   w-rds-pnt              .
           inspect   w-rds-low        tallying w-rds-pnt
                     for characters     before initial p-edx
                                                      (w-rds-ctr)     .
           if        w-rds-pnt            <    26
                     add     1            to   w-rds-pnt
                     move    w-rds-upc
                            (w-rds-pnt)   to   p-edx
                                              (w-rds-ctr)             .
           go to     edt-upp-200.
       edt-upp-999.
           exit.

      *================================================================*
      *    Edit alfanumerico lowercase                                 *
      *----------------------------------------------------------------*
       edt-low-000.
           move      p-alf                to   p-edt                  .
           move      p-car                to   p-edl                  .
           move      p-car                to   w-rds-ctr              .
       edt-low-200.
           subtract  1                    from w-rds-ctr              .
           if        w-rds-ctr            =    zero
                     go to edt-low-999.
           move      zero                 to   w-rds-pnt              .
           inspect   w-rds-upp        tallying w-rds-pnt
                     for characters     before initial p-edx
                                                      (w-rds-ctr)     .
           if        w-rds-pnt            <    26
                     add     1            to   w-rds-pnt
                     move    w-rds-loc
                            (w-rds-pnt)   to   p-edx
                                              (w-rds-ctr)             .
           go to     edt-low-200.
       edt-low-999.
           exit.

      *================================================================*
      *    Edit numerico                                               *
      *----------------------------------------------------------------*
       edt-num-000.
      *              *-------------------------------------------------*
      *              * Determinazione caratteristiche per editing      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se raggruppamento a tre a tre               *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "G"                                   .
           if        w-rds-ctr            =    zero
                     move   spaces        to   w-rds-rag
           else      move   "G"           to   w-rds-rag              .
      *                  *---------------------------------------------*
      *                  * Se zeri in testa                            *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "9"                                   .
           if        w-rds-ctr            =    zero
                     move   spaces        to   w-rds-zit
           else      move   "9"           to   w-rds-zit              .
      *                  *---------------------------------------------*
      *                  * Se blank when zero                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "B"                                   .
           if        w-rds-ctr            =    zero
                     move   spaces        to   w-rds-bwz
           else      move   "B"           to   w-rds-bwz              .
      *                  *---------------------------------------------*
      *                  * Se decimali quanti sono                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "D"                                   .
           if        w-rds-ctr            =    zero
                     move   spaces        to   w-rds-dqs
           else      move   "D"           to   w-rds-dqs              .
      *                  *---------------------------------------------*
      *                  * Se allineamento a sinistra                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-rds-asx              .
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "<"                                   .
           if        w-rds-ctr            >    zero   and
                     w-rds-zit            =    spaces
                     move   "<"           to   w-rds-asx              .
      *                  *---------------------------------------------*
      *                  * Se maschera di editing                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "M"                                   .
           if        w-rds-ctr            =    zero
                     move   spaces        to   w-rds-msk
           else      move   "M"           to   w-rds-msk              .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza totale campo edi-  *
      *                  * tato in p-edl, e dell' indice per unstring  *
      *                  * in w-rds-inx                                *
      *                  *---------------------------------------------*
           if        w-rds-msk            =    spaces
                     go to edt-num-100.
      *                      *-----------------------------------------*
      *                      * Se editing con maschera                 *
      *                      *-----------------------------------------*
           move      zero                 to   p-edl                  .
           inspect   p-msk            tallying p-edl
                     for    characters  before initial space          .
           move      1                    to   w-rds-inx              .
           go to     edt-num-200.
       edt-num-100.
      *                      *-----------------------------------------*
      *                      * Se editing senza maschera               *
      *                      *-----------------------------------------*
           move      p-car                to   p-edl                  .
           subtract  p-car                from 19
                                        giving w-rds-inx              .
           if        p-dec                >    zero
                     add      p-dec       to   p-edl
                     add      1           to   p-edl                  .
           if        p-sgn                =    "S"
                     add      1           to   p-edl
                     subtract 1           from w-rds-inx              .
           if        w-rds-rag            not  = spaces
                     move     p-car       to   w-rds-ctr
                     subtract 1           from w-rds-ctr
                     divide   3           into w-rds-ctr
                     add      w-rds-ctr   to   p-edl
                     subtract w-rds-ctr   from w-rds-inx              .
       edt-num-200.
      *              *-------------------------------------------------*
      *              * Editing effettivo                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione area editata a spaces          *
      *                  *---------------------------------------------*
           move      spaces               to   w-rds-ned              .
      *                  *---------------------------------------------*
      *                  * Test se campo a zero e clausola blank when  *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           if        p-num                =    zero and
                     w-rds-bwz            not  = spaces
                     go to edt-num-600.
      *                  *---------------------------------------------*
      *                  * Test se editing con maschera o normale      *
      *                  *---------------------------------------------*
           if        w-rds-msk            =    spaces
                     go to edt-num-400.
      *                  *---------------------------------------------*
      *                  * Editing con maschera                        *
      *                  *---------------------------------------------*
           move      p-num                to   w-rds-nem              .
           move      p-msk                to   w-rds-ned              .
           move      p-edl                to   w-rds-inx              .
           move      13                   to   w-rds-pnt              .
       edt-num-300.
           if        w-rds-ned-chr
                    (w-rds-inx)           =    "X"
                     move  w-rds-nex
                          (w-rds-pnt)     to   w-rds-ned-chr
                                              (w-rds-inx)
                     subtract 1           from w-rds-pnt
           else      if    w-rds-ned-chr
                          (w-rds-inx)     =    "b"
                           move  spaces   to   w-rds-ned-chr
                                              (w-rds-inx)             .
           if        w-rds-inx            >    1
                     subtract 1           from w-rds-inx
                     go to edt-num-300
           else      go to edt-num-600.
       edt-num-400.
      *                  *---------------------------------------------*
      *                  * Editing senza maschera                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing preliminare                     *
      *                      *-----------------------------------------*
           if        w-rds-rag            =    spaces
                     if     w-rds-zit     =    spaces
                            move   p-num  to   w-rds-ned-ns9
                     else   move   p-num  to   w-rds-ned-nc9
           else      if     w-rds-zit     =    spaces
                            move   p-num  to   w-rds-ned-rs9
                     else   move   p-num  to   w-rds-ned-rc9          .
      *                      *-----------------------------------------*
      *                      * Posizionamento segno se presente        *
      *                      *-----------------------------------------*
           if        p-num                <    zero and
                     p-sgn                not  = spaces
                     if    w-rds-zit      not  = spaces
                           move  "-"      to   w-rds-ned-chr
                                              (w-rds-inx)
                     else  move    zero   to   w-rds-ctr
                           inspect w-rds-ned
                                       tallying w-rds-ctr
                                          for  leading spaces
                           move    "-"    to   w-rds-ned-chr
                                              (w-rds-ctr)             .
      *                      *-----------------------------------------*
      *                      * Test se clausola di editing "D"         *
      *                      *-----------------------------------------*
           if        w-rds-dqs            =    spaces or
                     p-dec                not  > zero
                     go to edt-num-600.
           move      24                   to   w-rds-ctr              .
       edt-num-500.
           if        w-rds-ned-chr
                    (w-rds-ctr)           =    ","
                     move     spaces      to   w-rds-ned-chr
                                              (w-rds-ctr)
                     go to    edt-num-600.
           if        w-rds-ned-chr
                    (w-rds-ctr)           =    "0"
                     move     spaces      to   w-rds-ned-chr
                                              (w-rds-ctr)
                     subtract 1           from w-rds-ctr
                     go to    edt-num-500.
       edt-num-600.
      *                  *---------------------------------------------*
      *                  * Trasferimento campo editato in uscita       *
      *                  *---------------------------------------------*
           move      spaces               to   w-rds-edt              .
           add       w-rds-inx
                     p-edl              giving w-rds-pnt              .
           move      high-value           to   w-rds-ned-chr
                                              (w-rds-pnt)             .
      *                  *---------------------------------------------*
      *                  * Test se allineamento a sinistra             *
      *                  *---------------------------------------------*
           if        w-rds-asx            =    spaces
                     move  w-rds-inx      to   w-rds-pnt
                     go to edt-num-700.
           move      zero                 to   w-rds-pnt              .
           inspect   w-rds-ned        tallying w-rds-pnt
                                   for leading spaces                 .
           add       1                    to   w-rds-pnt              .
       edt-num-700.
           unstring  w-rds-ned  delimited by   high-value
                                          into w-rds-edt
                                  with pointer w-rds-pnt              .
           move      w-rds-edt            to   p-edt                  .
       edt-num-999.
           exit.

      *================================================================*
      *    Edit progressivo/anno                                       *
      *----------------------------------------------------------------*
       edt-pga-000.
      *              *-------------------------------------------------*
      *              * Determinazione moltiplicatore divisore          *
      *              *-------------------------------------------------*
           move      1                    to   w-rds-pmd              .
           move      p-car                to   w-rds-ctr              .
       edt-pga-200.
           if        w-rds-ctr            >    zero
                     multiply 10          by   w-rds-pmd
                     subtract 1           from w-rds-ctr
                     go to    edt-pga-200.
      *              *-------------------------------------------------*
      *              * Separazione campo numerico p-num in componenti  *
      *              * - w-rds-pts : secolo                            *
      *              * - w-rds-pta : anno                              *
      *              * - w-rds-prn : numero progressivo                *
      *              *-------------------------------------------------*
           move      p-num                to   w-rds-int              .
           divide    w-rds-pmd            into w-rds-int
                                        giving w-rds-ctr
                                     remainder w-rds-prn              .
           move      w-rds-ctr            to   w-rds-pta              .
           if        w-rds-prn            =    zero and
                     w-rds-pta            =    zero
                     move  zero           to   w-rds-pts
           else      if    w-rds-pta      <    25
                           move   1       to   w-rds-pts
                     else  move   zero    to   w-rds-pts              .
      *              *-------------------------------------------------*
      *              * Abblencamento rappresentazione editata totale   *
      *              *-------------------------------------------------*
           move      spaces               to   w-rds-ped              .
      *              *-------------------------------------------------*
      *              * Se tutto a zero : omette ulteriore editing      *
      *              *-------------------------------------------------*
           if        w-rds-prn            =    zero and
                     w-rds-pta            =    zero
                     go to edt-pga-400.
      *              *-------------------------------------------------*
      *              * Editing di : progressivo , barra , anno         *
      *              *-------------------------------------------------*
           move      w-rds-prn            to   w-rds-ped-pro          .
           move      "/"                  to   w-rds-ped-bar          .
           move      w-rds-pta            to   w-rds-ped-ann          .
       edt-pga-400.
      *              *-------------------------------------------------*
      *              * Lunghezza campo editato in uscita               *
      *              *-------------------------------------------------*
           add       3
                     p-car              giving p-edl                  .
      *              *-------------------------------------------------*
      *              * Valore editato in uscita                        *
      *              *-------------------------------------------------*
           move      spaces               to   p-edt                  .
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "<"                                   .
           if        w-rds-ctr            =    zero
                     subtract p-car       from 11
                                        giving w-rds-pnt
                     go to    edt-pga-600.
           move      zero                 to   w-rds-pnt              .
           inspect   w-rds-ped        tallying w-rds-pnt
                                   for leading spaces                 .
           add       1                    to   w-rds-pnt              .
       edt-pga-600.
           unstring  w-rds-ped            into p-edt
                                  with pointer w-rds-pnt              .
       edt-pga-999.
           exit.

      *================================================================*
      *    Edit data                                                   *
      *----------------------------------------------------------------*
       edt-dat-000.
      *              *-------------------------------------------------*
      *              * Valore editato in uscita                        *
      *              *-------------------------------------------------*
           move      spaces               to   p-edt                  .
           move      zero                 to   w-rds-ctr              .
           inspect   p-edm            tallying w-rds-ctr
                     for    all "S"                                   .
           if        w-rds-ctr            =    zero
                     go to    edt-dat-200.
       edt-dat-100.
      *              *-------------------------------------------------*
      *              * Data in formato 'GG/MM/AAAA'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore editato in uscita                    *
      *                  *---------------------------------------------*
           move      p-sec                to   w-rds-dat-wsc          .
           add       19                   to   w-rds-dat-wsc          .
           move      p-ann                to   w-rds-dat-waa          .
           move      p-mes                to   w-rds-dat-wmm          .
           move      p-gio                to   w-rds-dat-wgg          .
           move      w-rds-dat-gms        to   w-rds-dat-edz          .
           move      w-rds-dat-eds        to   p-edt                  .
           move      10                   to   p-edl                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     edt-dat-999.
       edt-dat-200.
      *              *-------------------------------------------------*
      *              * Data in formato 'GG/MM/AA'                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore editato in uscita                    *
      *                  *---------------------------------------------*
           move      p-ann                to   w-rds-dat-aaa          .
           move      p-mes                to   w-rds-dat-mmm          .
           move      p-gio                to   w-rds-dat-ggg          .
           move      w-rds-dat-gma        to   w-rds-dat-edx          .
           move      w-rds-dat-edt        to   p-edt                  .
           move      8                    to   p-edl                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     edt-dat-999.
       edt-dat-999.
           exit.

      *================================================================*
      *    Allineamento stringa alfanumerica                           *
      *----------------------------------------------------------------*
       asa-000.
      *              *-------------------------------------------------*
      *              * Subroutine di allineamento                      *
      *              *-------------------------------------------------*
           move      p-car                to   w-all-str-lun          .
           move      p-alf                to   w-all-str-alf          .
           if        p-edm                =    "C"
                     perform  all-str-cen-000
                                          thru all-str-cen-999
           else if   p-edm                =    "D"
                     perform  all-str-adx-000
                                          thru all-str-adx-999
           else if   p-edm                =    "S"
                     perform  all-str-asx-000
                                          thru all-str-asx-999
           else      perform  all-str-asx-000
                                          thru all-str-asx-999        .
      *              *-------------------------------------------------*
      *              * In valore di uscita                             *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   p-alf                  .
       asa-999.
           exit.

      *================================================================*
      *    Determinazione posizione ultimo carattere non-blank         *
      *----------------------------------------------------------------*
       unb-000.
      *              *-------------------------------------------------*
      *              * Se numero linea di stampa non valido : uscita   *
      *              * con risultato zero                              *
      *              *-------------------------------------------------*
           if        p-lin                =    zero       or
                     p-lin                >    p-sel-efc-sel
                     move  zero           to   p-pos
                     go to unb-999.
      *              *-------------------------------------------------*
      *              * Determinazione posizione ultimo non-blank       *
      *              *-------------------------------------------------*
           move      p-buf-lin (p-lin)    to   w-rds-240              .
           move      1                    to   w-rds-pnt              .
       unb-100.
           move      w-rds-pnt            to   p-pos                  .
           unstring  w-rds-240  delimited by   all spaces
                                          into w-rds-001
                                    count in   w-rds-ctr
                                  with pointer w-rds-pnt              .
           if        w-rds-pnt            not  > 240
                     go to unb-100.
           add       w-rds-ctr            to   p-pos                  .
           subtract  1                    from p-pos                  .
       unb-999.
           exit.

      *================================================================*
      *    Line-number Save                                            *
      *----------------------------------------------------------------*
       lns-000.
           if        p-qlp                <    10
                     add    1             to   p-qlp
                     move   p-lnr         to   p-qll (p-qlp)
                     move   p-vpm         to   p-qvm (p-qlp)          .
       lns-999.
           exit.

      *================================================================*
      *    Line-number Restore                                         *
      *----------------------------------------------------------------*
       lnr-000.
           if        p-qlp                >    zero
                     move     p-qll (p-qlp)
                                          to   p-lnr
                     move     p-qvm (p-qlp)
                                          to   p-vpm
                     subtract 1           from p-qlp                  .
           subtract  p-lnr                from p-sel-efc-sel
                                        giving p-res                  .
           add       1                    to   p-res                  .
       lnr-999.
           exit.

      *================================================================*
      *    Get Line                                                    *
      *----------------------------------------------------------------*
       gtl-000.
      *              *-------------------------------------------------*
      *              * Se numero linea di stampa non valido : uscita   *
      *              * con risultato zero                              *
      *              *-------------------------------------------------*
           if        p-lin                =    zero       or
                     p-lin                >    p-sel-efc-sel
                     move  spaces         to   p-alf
                     go to gtl-999.
      *              *-------------------------------------------------*
      *              * Estrazione contenuto linea                      *
      *              *-------------------------------------------------*
           move      p-buf-lin (p-lin)    to   p-alf                  .
       gtl-999.
           exit.

      *    *===========================================================*
      *    * Open output per stampa immediata                          *
      *    *-----------------------------------------------------------*
       imm-opn-out-000.
      *              *-------------------------------------------------*
      *              * Flag di open output in sospeso                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-imm-flg-opo          .
       imm-opn-out-999.
           exit.

      *    *===========================================================*
      *    * Close output per stampa immediata                         *
      *    *-----------------------------------------------------------*
       imm-cls-out-000.
      *              *-------------------------------------------------*
      *              * Se numero pagina zero non si esegue nessuna o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           if        p-pag                =    zero
                     go to imm-cls-out-999.
      *              *-------------------------------------------------*
      *              * Se l'ultima operazione eseguita e' stata eject  *
      *              * non si scarica la pagina in sospeso             *
      *              *-------------------------------------------------*
           if        p-vpm                =    "2"
                     go to imm-cls-out-200.
      *              *-------------------------------------------------*
      *              * Scaricamento pagina in sospeso su stampante     *
      *              *-------------------------------------------------*
           perform   imm-wrt-pag-000      thru imm-wrt-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione : uscita            *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to imm-cls-out-999.
       imm-cls-out-200.
      *              *-------------------------------------------------*
      *              * Close stampante mediante richiamo del driver di *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           move      "CO"                 to   p-sub                  .
           call      d-pth               using p                      .
      *              *-------------------------------------------------*
      *              * Se errori in close stampante                    *
      *              *-------------------------------------------------*
           if        p-rsc                =    spaces
                     go to imm-cls-out-400.
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore per stampante *
      *                  *---------------------------------------------*
           perform   imm-err-stp-000      thru imm-err-stp-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     imm-cls-out-999.
       imm-cls-out-400.
      *              *-------------------------------------------------*
      *              * Se stampa non in spool : uscita                 *
      *              *-------------------------------------------------*
           if        w-imm-flg-dos        =    spaces
                     go to imm-cls-out-999.
      *              *-------------------------------------------------*
      *              * Se stampa in spool                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il contatore per numero sub-file di spo- *
      *                  * ol e' diverso da zero, significa che sono   *
      *                  * state eseguite delle istruzioni Eject, per- *
      *                  * tanto non e' necessaria alcuna azione di    *
      *                  * chiamata allo spooler di sistema, in quanto *
      *                  * gia' eseguita volta per volta alla fine di  *
      *                  * ogni documento. Sara' invece necessaria la  *
      *                  * semplice cancellazione del file generale di *
      *                  * transito, che deve essere attualmente vuoto *
      *                  *---------------------------------------------*
           if        w-imm-sub-fds        =    zero
                     go to imm-cls-out-600.
      *                      *-----------------------------------------*
      *                      * Richiamo driver di stampa per funzione  *
      *                      * di cancellazione file di stampa         *
      *                      *-----------------------------------------*
           move      "SD"                 to   p-sub                  .
           call      d-pth               using p                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     imm-cls-out-999.
       imm-cls-out-600.
      *                  *---------------------------------------------*
      *                  * Se il contatore per numero sub-file di spo- *
      *                  * ol e' uguale a zero, significa che non sono *
      *                  * state eseguite delle istruzioni Eject, per- *
      *                  * tanto e' necessaria la chiamata al sistema  *
      *                  * per lo spooler di sistema                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo spooler di sistema             *
      *                      *-----------------------------------------*
           perform   cll-sys-spl-000      thru cll-sys-spl-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     imm-cls-out-999.
       imm-cls-out-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema per il richiamo dello spooler di si-  *
      *    * stema                                                     *
      *    *-----------------------------------------------------------*
       cll-sys-spl-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore del contatore   *
      *              * per il numero di sub-file di spool              *
      *              *-------------------------------------------------*
           if        w-imm-sub-fds        =    zero
                     go to cll-sys-spl-100
           else      go to cll-sys-spl-500.
       cll-sys-spl-100.
      *              *-------------------------------------------------*
      *              * Se numero di sub-file di spool a zero           *
      *              *-------------------------------------------------*
       cll-sys-spl-125.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di spooler    *
      *                  * da utilizzare                               *
      *                  *---------------------------------------------*
           if        w-imm-spl-dau        =    "rcp"
                     go to cll-sys-spl-300
           else if   w-imm-spl-dau        =    "ucp"
                     go to cll-sys-spl-350
           else if   w-imm-spl-dau        =    "ghs"
                     go to cll-sys-spl-375
           else if   w-imm-spl-dau        =    "fax"
                     go to cll-sys-spl-385
           else if   w-imm-spl-dau        =    "eml"
                     go to cll-sys-spl-390
           else if   w-imm-spl-dau        =    "#"
                     go to cll-sys-spl-400.
       cll-sys-spl-200.
      *                  *---------------------------------------------*
      *                  * Se spooler "spool"                          *
      *                  *---------------------------------------------*
       cll-sys-spl-210.
      *                      *-----------------------------------------*
      *                      * Composizione del comando per il richia- *
      *                      * mo dello spooler                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice stampante                    *
      *                          *-------------------------------------*
           if        w-imm-nls-dau        =    spaces
                     move  p-sel-stp-sel  to   w-sys-spl-cmd-cst
           else      move  w-imm-nls-dau  to   w-sys-spl-cmd-cst      .
      *                          *-------------------------------------*
      *                          * Pathname del file da stampare       *
      *                          *-------------------------------------*
           move      w-imm-pat-spl        to   w-sys-spl-cmd-pat      .
      *                          *-------------------------------------*
      *                          * Costruzione del comando in base al  *
      *                          * template                            *
      *                          *-------------------------------------*
           perform   cll-sys-spc-000      thru cll-sys-spc-999        .
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema effettiva           *
      *                      *-----------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-300.
      *                  *---------------------------------------------*
      *                  * Se spooler "rcp"                            *
      *                  *                                             *
      *                  * In obsolescenza                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione del comando per il richia- *
      *                      * mo del remote copy                      *
      *                      *                                         *
      *                      *           I M P O R T A N T E           *
      *                      *                                         *
      *                      * Il comando 'rcp' crea dei file con il   *
      *                      * proprietario che ha effettuato il login *
      *                      * e non quello assunto dal runtime cobol! *
      *                      *-----------------------------------------*
           move      spaces               to   o-shs                  .
           string    "rcp "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-nls-dau
                                delimited by   spaces
                     " ; rm -f "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     " &"
                                delimited by   size
                                          into o-shs                  .
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema effettiva           *
      *                      *-----------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-350.
      *                  *---------------------------------------------*
      *                  * Se spooler "ucp"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           move      "cp        "         to   w-imm-exe-cmd          .
           move      w-imm-pat-spl        to   w-imm-exe-pat          .
           move      w-imm-nls-dau        to   w-imm-exe-nls          .
           move      w-imm-fil-dau        to   w-imm-exe-fil          .
           perform   cll-sys-spl-exe-000  thru cll-sys-spl-exe-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-375.
      *                  *---------------------------------------------*
      *                  * Se spooler "ghs"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assemblaggio del nome con il suffisso   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-imm-fil-dau        to   w-all-str-cat (1)      .
           move      ".pdf"               to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           move      "ps2pdf    "         to   w-imm-exe-cmd          .
           move      w-imm-pat-spl        to   w-imm-exe-pat          .
           move      w-imm-nls-dau        to   w-imm-exe-nls          .
           move      w-all-str-alf        to   w-imm-exe-fil          .
           perform   cll-sys-spl-exe-000  thru cll-sys-spl-exe-999    .
      *                      *-----------------------------------------*
      *                      * Eventuale protezione PDF                *
      *                      *-----------------------------------------*
           perform   cll-pdf-tkp-000      thru cll-pdf-tkp-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-385.
      *                  *---------------------------------------------*
      *                  * Se spooler "fax"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           move      "sendfax   "         to   w-imm-exe-cmd          .
           perform   cll-sys-spl-fax-000  thru cll-sys-spl-fax-999    .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-390.
      *                  *---------------------------------------------*
      *                  * Se spooler "eml"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   cll-sys-spl-eml-000  thru cll-sys-spl-eml-999    .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-400.
      *                  *---------------------------------------------*
      *                  * Se stampante locale                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per 'locprn00'    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "run"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "locprn00"           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-loc-p00              .
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per 'locprn99'    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "run"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "locprn99"           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-loc-p99              .
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema effettiva           *
      *                      *-----------------------------------------*
           move      "CU"                 to   o-ope                  .
           move      spaces               to   o-shs                  .
           string    "cat "
                                delimited by   size
                     w-loc-p00
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-loc-p99
                                delimited by   spaces
                     " ; "
                                delimited by   size
                     "rm -f "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                                          into o-shs                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-500.
      *              *-------------------------------------------------*
      *              * Se numero di sub-file di spool a non-zero       *
      *              *-------------------------------------------------*
       cll-sys-spl-525.
      *                  *---------------------------------------------*
      *                  * Richiamo driver di stampa per funzione di   *
      *                  * chiusura file di stampa                     *
      *                  *---------------------------------------------*
           move      "SC"                 to   p-sub                  .
           call      d-pth               using p                      .
      *                  *---------------------------------------------*
      *                  * Chiamata di sistema per ridenominare il fi- *
      *                  * le di stampa con il suffisso per il subfile *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      w-imm-pat-spl        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    w-imm-pat-spl
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-imm-sub-fds
                                delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "RP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cll-sys-spl-550.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di spooler    *
      *                  * da utilizzare                               *
      *                  *---------------------------------------------*
           if        w-imm-spl-dau        =    "rcp"
                     go to cll-sys-spl-700
           else if   w-imm-spl-dau        =    "ucp"
                     go to cll-sys-spl-750
           else if   w-imm-spl-dau        =    "ghs"
                     go to cll-sys-spl-775
           else if   w-imm-spl-dau        =    "fax"
                     go to cll-sys-spl-785
           else if   w-imm-spl-dau        =    "eml"
                     go to cll-sys-spl-790
           else if   w-imm-spl-dau        =    "#"
                     go to cll-sys-spl-800.
       cll-sys-spl-600.
      *                  *---------------------------------------------*
      *                  * Se spooler "spool"                          *
      *                  *---------------------------------------------*
       cll-sys-spl-610.
      *                      *-----------------------------------------*
      *                      * Composizione del comando per il richia- *
      *                      * mo dello spooler                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice stampante                    *
      *                          *-------------------------------------*
           if        w-imm-nls-dau        =    spaces
                     move  p-sel-stp-sel  to   w-sys-spl-cmd-cst
           else      move  w-imm-nls-dau  to   w-sys-spl-cmd-cst      .
      *                          *-------------------------------------*
      *                          * Pathname del file da stampare       *
      *                          *-------------------------------------*
           move      spaces               to   w-sys-spl-cmd-pat      .
           string    w-imm-pat-spl
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-imm-sub-fds
                                delimited by   size
                                          into w-sys-spl-cmd-pat      .
      *                          *-------------------------------------*
      *                          * Costruzione del comando in base al  *
      *                          * template                            *
      *                          *-------------------------------------*
           perform   cll-sys-spc-000      thru cll-sys-spc-999        .
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema effettiva           *
      *                      *-----------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-700.
      *                  *---------------------------------------------*
      *                  * Se spooler "rcp"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione del comando per il richia- *
      *                      * mo del remote copy                      *
      *                      *-----------------------------------------*
           move      spaces               to   o-shs                  .
           string    "rcp "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-imm-sub-fds
                                delimited by   size
                     " "
                                delimited by   size
                     w-imm-nls-dau
                                delimited by   spaces
                     " ; rm -f "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-imm-sub-fds
                                delimited by   size
                     " &"
                                delimited by   size
                                          into o-shs                  .
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema effettiva           *
      *                      *-----------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-750.
      *                  *---------------------------------------------*
      *                  * Se spooler "ucp"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assemblaggio del nome per il sub-file   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-imm-pat-spl        to   w-all-str-cat (1)      .
           move      "."                  to   w-all-str-cat (2)      .
           move      w-imm-sub-fds        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           move      "cp        "         to   w-imm-exe-cmd          .
           move      w-all-str-alf        to   w-imm-exe-pat          .
           move      w-imm-nls-dau        to   w-imm-exe-nls          .
           move      w-imm-fil-dau        to   w-imm-exe-fil          .
           perform   cll-sys-spl-exe-000  thru cll-sys-spl-exe-999    .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-775.
      *                  *---------------------------------------------*
      *                  * Se spooler "ghs"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assemblaggio del nome per il sub-file   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-imm-pat-spl        to   w-all-str-cat (1)      .
           move      "."                  to   w-all-str-cat (2)      .
           move      w-imm-sub-fds        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-imm-exe-pat          .
      *                      *-----------------------------------------*
      *                      * Assemblaggio del nome con il suffisso   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-imm-fil-dau        to   w-all-str-cat (1)      .
      *
           if        w-imm-sub-fds        =    001
                     move  spaces         to   w-all-str-cat (2)
                     move  spaces         to   w-all-str-cat (3)
           else      move  "."            to   w-all-str-cat (2)
                     move  w-imm-sub-fds  to   w-all-str-cat (3)      .
      *
           move      ".pdf"               to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-imm-exe-fil          .
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           move      "ps2pdf    "         to   w-imm-exe-cmd          .
           move      w-imm-nls-dau        to   w-imm-exe-nls          .
           perform   cll-sys-spl-exe-000  thru cll-sys-spl-exe-999    .
      *                      *-----------------------------------------*
      *                      * Eventuale protezione PDF                *
      *                      *-----------------------------------------*
           perform   cll-pdf-tkp-000      thru cll-pdf-tkp-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-785.
      *                  *---------------------------------------------*
      *                  * Se spooler "fax"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assemblaggio del nome per il sub-file   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-imm-pat-spl        to   w-all-str-cat (1)      .
           move      "."                  to   w-all-str-cat (2)      .
           move      w-imm-sub-fds        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-imm-pat-spl          .
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           move      "sendfax   "         to   w-imm-exe-cmd          .
           perform   cll-sys-spl-fax-000  thru cll-sys-spl-fax-999    .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-790.
      *                  *---------------------------------------------*
      *                  * Se spooler "eml"                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assemblaggio del nome per il sub-file   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-imm-pat-spl        to   w-all-str-cat (1)      .
           move      "."                  to   w-all-str-cat (2)      .
           move      w-imm-sub-fds        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-imm-pat-spl          .
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   cll-sys-spl-eml-000  thru cll-sys-spl-eml-999    .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-800.
      *                  *---------------------------------------------*
      *                  * Se stampante locale                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per 'locprn00'    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "run"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "locprn00"           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-loc-p00              .
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per 'locprn99'    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "run"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "locprn99"           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-loc-p99              .
      *                      *-----------------------------------------*
      *                      * Chiamata al sistema effettiva           *
      *                      *-----------------------------------------*
           move      "CU"                 to   o-ope                  .
           move      spaces               to   o-shs                  .
           string    "cat "
                                delimited by   size
                     w-loc-p00
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-imm-sub-fds
                                delimited by   size
                     " "
                                delimited by   size
                     w-loc-p99
                                delimited by   spaces
                     " ; "
                                delimited by   size
                     "rm -f "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     "."
                                delimited by   size
                     w-imm-sub-fds
                                delimited by   size
                                          into o-shs                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-900.
       cll-sys-spl-900.
      *                  *---------------------------------------------*
      *                  * Continuazione comune sia a "spool" che a    *
      *                  * "rcp" o "ucp" o "ghs" o "fax" o "eml"       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo driver di stampa per funzione  *
      *                      * di ri-apertura file di stampa           *
      *                      *-----------------------------------------*
           move      "SO"                 to   p-sub                  .
           call      d-pth               using p                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cll-sys-spl-999.
       cll-sys-spl-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema per il richiamo dello spooler di si-  *
      *    * stema                                                     *
      *    *                                                           *
      *    * Esecuzione del comando                                    *
      *    *-----------------------------------------------------------*
       cll-sys-spl-exe-000.
      *              *-------------------------------------------------*
      *              * Composizione del comando                        *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
           string    w-imm-exe-cmd
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-exe-pat
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-exe-nls
                                delimited by   spaces
                     w-imm-exe-fil
                                delimited by   spaces
                     " ; rm -f "
                                delimited by   size
                     w-imm-exe-pat
                                delimited by   spaces
                     " &"
                                delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cll-sys-spl-exe-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cll-sys-spl-exe-999.
       cll-sys-spl-exe-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema per il richiamo dello spooler di si-  *
      *    * stema                                                     *
      *    *                                                           *
      *    * 'fax'                                                     *
      *    *-----------------------------------------------------------*
       cll-sys-spl-fax-000.
      *              *-------------------------------------------------*
      *              * Composizione del comando                        *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
           string    w-imm-exe-cmd
                                delimited by   spaces
                     " -s A4 -m"
                                delimited by   size
                     " -a """
                                delimited by   size
                     w-imm-fil-tim
                                delimited by   spaces
                     """ -r """
                                delimited by   size
                     w-imm-fil-dst
                                delimited by   size
                     """ -d "
                                delimited by   size
                     w-imm-fil-num
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     " > /dev/null; rm -f "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     " &"
                                delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cll-sys-spl-fax-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cll-sys-spl-fax-999.
       cll-sys-spl-fax-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema per il richiamo dello spooler di si-  *
      *    * stema                                                     *
      *    *                                                           *
      *    * 'eml'                                                     *
      *    *-----------------------------------------------------------*
       cll-sys-spl-eml-000.
      *              *-------------------------------------------------*
      *              * Lettura User ID per invio posta da segreteria   *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "uid-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-fil-uid          .
      *              *-------------------------------------------------*
      *              * Lettura Password per invio posta da segreteria  *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "pwd-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-fil-pwd          .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al     *
      *              * mail server                                     *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "msa-isp"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-fil-msa          .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al     *
      *              * postmaster                                      *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "eml-psm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-fil-psm          .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa        *
      *              * all'agente di invio documenti                   *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "ads-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-imm-exe-cmd          .
      *              *-------------------------------------------------*
      *              * Informazioni generali da segreteria             *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-ute                to   w-imm-fil-ute          .
           move      s-azi                to   w-imm-fil-azi          .
      *                  *---------------------------------------------*
      *                  * Eventuale indirizzo mail associato all'u-   *
      *                  * tente come mittente                         *
      *                  *---------------------------------------------*
           if        s-adx                not  = spaces
                     move  s-adx          to   w-imm-fil-psm          .
       cll-sys-spl-eml-100.
      *              *-------------------------------------------------*
      *              * Trasformazione dell'allegato in formato PDF     *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
           string    "ps2pdf"
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-imm-pat-spl
                                delimited by   spaces
                     ".pdf ;"
                                delimited by   size

      *              *-------------------------------------------------*
      *              * ISTRUZIONI RIMOSSE - 17/04/13                   *
      *              *                                                 *
      *              * N.B.: la cancellazione produceva un errore      *
      *              *-------------------------------------------------*
______*              ".pdf ; rm -f "
______*                         delimited by   size
______*              w-imm-pat-spl
______*                         delimited by   spaces
______*              " &"
______*                         delimited by   size
      *              *-------------------------------------------------*
                                
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Eventuale protezione PDF                        *
      *              *-------------------------------------------------*
______*    perform   cll-pdf-tkp-000      thru cll-pdf-tkp-999        .
       cll-sys-spl-eml-300.
      *              *-------------------------------------------------*
      *              * Assemblaggio preliminare di destinatario e nome *
      *              * allegato                                        *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      w-imm-fil-ind        to   w-all-str-cat (1)      .
           move      "#"                  to   w-all-str-cat (2)      .
           move      w-imm-pat-spl        to   w-all-str-cat (3)      .
           move      ".pdf"               to   w-all-str-cat (4)      .
           move      "#"                  to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-imm-fil-pth          .
      *              *-------------------------------------------------*
      *              * Composizione del comando                        *
      *              *                                                 *
      *              *   - [Agente di spedizione - modulo PERL t_xxxx] *
      *              *   - Codice azienda                              *
      *              *   - Server di posta SMTP                        *
      *              *   - Mittente                                    *
      *              *   - username  (obblig. solo PEC)                *
      *              *   - password  (obblig. solo PEC)                *
      *              *   - indirizzi (opzionale, default 'indi.txt')   *
      *              *   - soggetto  (opzionale, default 'sogg.txt')   *
      *              *   - messaggio (opzionale, default 'mess.txt')   *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
      *
           string    w-imm-exe-cmd
                                delimited by   spaces
                     " "        delimited by   size
      *              *-------------------------------------------------*
      *              * Azienda                                         *
      *              *-------------------------------------------------*
                     w-imm-fil-azi
                                delimited by   spaces
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * Server di posta SMTP                            *
      *              *-------------------------------------------------*
                     w-imm-fil-msa
                                delimited by   spaces
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * Mittente (o Postmaster)                         *
      *              *-------------------------------------------------*
                     w-imm-fil-psm
                                delimited by   spaces
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * User ID Account SMTP                            *
      *              *-------------------------------------------------*
                     w-imm-fil-uid
                                delimited by   spaces
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * Password Account SMTP                           *
      *              *-------------------------------------------------*
                     w-imm-fil-pwd
                                delimited by   spaces
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * Indirizzo mail destinatario cablato con path    *
      *              * allegato o lista indirizzi                      *
      *              *-------------------------------------------------*
                     w-imm-fil-pth
                                delimited by   spaces
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * Soggetto o nome file soggetto                   *
      *              *-------------------------------------------------*
                     w-imm-fil-ogg
                                delimited by   size
                     ","        delimited by   size
      *              *-------------------------------------------------*
      *              * Messaggio o nome file messaggio                 *
      *              *-------------------------------------------------*
                     w-imm-fil-mes
                                delimited by   spaces
                     ","        delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cll-sys-spl-eml-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cll-sys-spl-eml-999.
       cll-sys-spl-eml-999.
           exit.

      *    *===========================================================*
      *    * Costruzione del comando per lo spooler di stampa in base  *
      *    * ai seguenti parametri :                                   *
      *    *                                                           *
      *    * - w-sys-spl-cmd-tem : Template per lo spooler             *
      *    * - w-sys-spl-cmd-cst : Codice della stampante              *
      *    * - w-sys-spl-cmd-pat : Pathname del file da stampare       *
      *    * - w-sys-spl-cmd-roe : Redirezione output ed errori        *
      *    *                                                           *
      *    * ponendo il risultato in :                                 *
      *    *                                                           *
      *    * - o-shs : area parametri per chiamate al sistema          *
      *    *-----------------------------------------------------------*
       cll-sys-spc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       cll-sys-spc-020.
      *                  *---------------------------------------------*
      *                  * Area o-shs                                  *
      *                  *---------------------------------------------*
           move      spaces               to   o-shs                  .
       cll-sys-spc-040.
      *                  *---------------------------------------------*
      *                  * Contatori, indici, puntatori locali         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indice su template                      *
      *                      *-----------------------------------------*
           move      001                  to   w-sys-spl-inx-tem      .
      *                      *-----------------------------------------*
      *                      * Indice su area o-shs                    *
      *                      *-----------------------------------------*
           move      001                  to   w-sys-spl-inx-hpr      .
       cll-sys-spc-050.
      *                  *---------------------------------------------*
      *                  * Controllo che esistano due e solo due ca-   *
      *                  * ratteri '@'                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-sys-spl-ctr-001      .
           inspect   w-sys-spl-cmd-tem
                                      tallying w-sys-spl-ctr-001
                                          for  all   "@"              .
           if        w-sys-spl-ctr-001    not  = 2
                     move  w-sys-spl-cmd-tem
                                          to   o-shs
                     go to cll-sys-spc-100.
       cll-sys-spc-100.
      *              *-------------------------------------------------*
      *              * Copiatura porzione precedente il primo caratte- *
      *              * re '@'                                          *
      *              *-------------------------------------------------*
       cll-sys-spc-120.
      *                  *---------------------------------------------*
      *                  * Determinazione numero di caratteri che pre- *
      *                  * cedono il primo carattere '@'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-sys-spl-ctr-001      .
           inspect   w-sys-spl-cmd-tem
                                      tallying w-sys-spl-ctr-001
                                          for  characters
                                        before initial "@"            .
       cll-sys-spc-140.
      *                  *---------------------------------------------*
      *                  * Copiatura                                   *
      *                  *---------------------------------------------*
           move      w-sys-spl-cmd-tem
                    (w-sys-spl-inx-tem:
                     w-sys-spl-ctr-001)   to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
       cll-sys-spc-160.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su area o-shs          *
      *                  *---------------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
       cll-sys-spc-200.
      *              *-------------------------------------------------*
      *              * Composizione della porzione relativa al primo   *
      *              * carattere '@'                                   *
      *              *-------------------------------------------------*
       cll-sys-spc-220.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-tem      .
           add       001                  to   w-sys-spl-inx-tem      .
       cll-sys-spc-240.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di parame-  *
      *                  * tro                                         *
      *                  *---------------------------------------------*
           if        w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "p" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "P"
                     go to cll-sys-spc-260
           else if   w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "f" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "F"
                     go to cll-sys-spc-280
           else      go to cll-sys-spc-300.
       cll-sys-spc-260.
      *                  *---------------------------------------------*
      *                  * Se codice stampante                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il codice stampante         *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-cst
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      013                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione codice stampante           *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-cst
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spc-300.
       cll-sys-spc-280.
      *                  *---------------------------------------------*
      *                  * Se file pathname                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il file pathname            *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-pat
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      060                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione file pathname              *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-pat
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spc-300.
       cll-sys-spc-300.
      *              *-------------------------------------------------*
      *              * Copiatura porzione tra il primo carattere '@'   *
      *              * ed il secondo carattere '@'                     *
      *              *-------------------------------------------------*
       cll-sys-spc-320.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       001                  to   w-sys-spl-inx-tem      .
       cll-sys-spc-340.
      *                  *---------------------------------------------*
      *                  * Copiatura                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-sys-spl-ctr-001      .
           move      spaces               to   w-sys-spl-wkx-060      .
           move      w-sys-spl-inx-tem    to   w-sys-spl-pnt-tem      .
           unstring  w-sys-spl-cmd-tem
                                delimited by   "@"
                                          into w-sys-spl-wkx-060
                                    count in   w-sys-spl-ctr-001
                                  with pointer w-sys-spl-pnt-tem      .
           move      w-sys-spl-wkx-060
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
       cll-sys-spc-360.
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
       cll-sys-spc-400.
      *              *-------------------------------------------------*
      *              * Composizione della porzione relativa al secondo *
      *              * carattere '@'                                   *
      *              *-------------------------------------------------*
       cll-sys-spc-420.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-tem      .
           add       001                  to   w-sys-spl-inx-tem      .
       cll-sys-spc-440.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di parame-  *
      *                  * tro                                         *
      *                  *---------------------------------------------*
           if        w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "p" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "P"
                     go to cll-sys-spc-460
           else if   w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "f" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "F"
                     go to cll-sys-spc-480
           else      go to cll-sys-spc-500.
       cll-sys-spc-460.
      *                  *---------------------------------------------*
      *                  * Se codice stampante                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il codice stampante         *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-cst
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      013                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione codice stampante           *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-cst
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spc-500.
       cll-sys-spc-480.
      *                  *---------------------------------------------*
      *                  * Se file pathname                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il file pathname            *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-pat
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      060                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione file pathname              *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-pat
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cll-sys-spc-500.
       cll-sys-spc-500.
      *              *-------------------------------------------------*
      *              * Copiatura porzione tra il secondo carattere '@' *
      *              * in poi                                          *
      *              *-------------------------------------------------*
       cll-sys-spc-520.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       001                  to   w-sys-spl-inx-tem      .
       cll-sys-spc-540.
      *                  *---------------------------------------------*
      *                  * Copiatura                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-sys-spl-wkx-060      .
           unstring  w-sys-spl-cmd-tem    into w-sys-spl-wkx-060
                                  with pointer w-sys-spl-inx-tem      .
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-wkx-060
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      060                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
           if        w-sys-spl-ctr-001    =    zero
                     go to cll-sys-spc-560.
           move      w-sys-spl-wkx-060
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
       cll-sys-spc-560.
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
       cll-sys-spc-600.
      *              *-------------------------------------------------*
      *              * Copiatura literal fisso per redirezione output  *
      *              * ed errori                                       *
      *              *-------------------------------------------------*
           move      w-sys-spl-cmd-roe    to   o-shs
                                              (w-sys-spl-inx-hpr: )   .
       cll-sys-spc-700.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cll-sys-spc-999.
       cll-sys-spc-999.
           exit.

      *    *===========================================================*
      *    * Chiamata al sistema per il richiamo del tool di tratta-   *
      *    * mento dei file PDF                                        *
      *    *-----------------------------------------------------------*
       cll-pdf-tkp-000.
      *              *-------------------------------------------------*
      *              * Test se password attiva                         *
      *              *                                                 *
      *              * La password e' quella legata alla stampante     *
      *              *-------------------------------------------------*
           if        w-stp-pwd-mms        not  = spaces
                     go to cll-pdf-tkp-100
           else      go to cll-pdf-tkp-500.
       cll-pdf-tkp-100.
      *              *-------------------------------------------------*
      *              * Composizione del comando di protezione          *
      *              *                                                 *
      *              * Se esiste la password                           *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
           string    "pdftk "
                                delimited by   size
                     w-imm-exe-nls
                                delimited by   spaces
                     w-imm-exe-fil
                                delimited by   spaces
                     " output "
                                delimited by   size
                     w-imm-exe-nls
                                delimited by   spaces
                     p-sel-cod-azi
                                delimited by   spaces
                     "_"
                                delimited by   size
                     w-imm-exe-fil
                                delimited by   spaces
                     " owner_pw "
                                delimited by   size
                     w-stp-pwd-mms
                                delimited by   spaces
                     " allow printing"
                                delimited by   size
                     " ; rm -f "
                                delimited by   size
                     w-imm-exe-nls
                                delimited by   spaces
                     w-imm-exe-fil
                                delimited by   spaces
                     " &"
                                delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cll-pdf-tkp-200.
      *              *-------------------------------------------------*
      *              * Consegna del percorso archivio generato alla    *
      *              * segreteria                                      *
      *              *-------------------------------------------------*
           move      "Pa"                 to   s-ope                  .
           move      spaces               to   s-pat                  .
           string    w-imm-exe-nls
                                delimited by   spaces
                     p-sel-cod-azi
                                delimited by   spaces
                     "_"
                                delimited by   size
                     w-imm-exe-fil
                                delimited by   spaces
                                          into s-pat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       cll-pdf-tkp-290.
      *              *-------------------------------------------------*
      *              * Attesa di un secondo                            *
      *              *-------------------------------------------------*
           perform   wai-000              thru wai-999                .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     cll-pdf-tkp-900.
       cll-pdf-tkp-500.
      *              *-------------------------------------------------*
      *              * Consegna del percorso archivio generato alla    *
      *              * segreteria                                      *
      *              *                                                 *
      *              * Se non esiste la password                       *
      *              *                                                 *
      *              * In questo caso non si antepone la sigla azienda *
      *              *-------------------------------------------------*
           move      "Pa"                 to   s-ope                  .
           move      spaces               to   s-pat                  .
           string    w-imm-exe-nls
                                delimited by   spaces
                     w-imm-exe-fil
                                delimited by   spaces
                                          into s-pat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       cll-pdf-tkp-590.
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     cll-pdf-tkp-900.
       cll-pdf-tkp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cll-pdf-tkp-999.
       cll-pdf-tkp-999.
           exit.

      *    *===========================================================*
      *    * Write Page per stampa immediata                           *
      *    *-----------------------------------------------------------*
       imm-wrt-pag-000.
      *              *-------------------------------------------------*
      *              * Se numero pagina zero non si esegue nessuna o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           if        p-pag                =    zero
                     go to imm-wrt-pag-999.
      *              *-------------------------------------------------*
      *              * Se open output in sospeso                       *
      *              *-------------------------------------------------*
           if        w-imm-flg-opo        not  = spaces
                     go to imm-wrt-pag-200.
      *                  *---------------------------------------------*
      *                  * Operazioni preliminari alla prima scrittura *
      *                  * per stampa immediata                        *
      *                  *---------------------------------------------*
           perform   imm-pre-out-000      thru imm-pre-out-999        .
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione : uscita            *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to imm-wrt-pag-999.
      *                  *---------------------------------------------*
      *                  * Flag di open output non piu' in sospeso     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-imm-flg-opo          .
       imm-wrt-pag-200.
      *              *-------------------------------------------------*
      *              * Scrittura pagina                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione buffer di stampa            *
      *                  *---------------------------------------------*
           perform   nor-buf-stp-000      thru nor-buf-stp-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura pagina su stampante mediante ri-  *
      *                  * chiamo del driver di stampa                 *
      *                  *---------------------------------------------*
           move      "WP"                 to   p-sub                  .
           call      d-pth               using p                      .
      *                  *---------------------------------------------*
      *                  * Se errori in scrittura pagina su stampante  *
      *                  *---------------------------------------------*
           if        p-rsc                =    spaces
                     go to imm-wrt-pag-999.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore per stam- *
      *                      * pante                                   *
      *                      *-----------------------------------------*
           perform   imm-err-stp-000      thru imm-err-stp-999        .
       imm-wrt-pag-999.
           exit.

      *    *===========================================================*
      *    * Eject Page per stampa immediata                           *
      *    *-----------------------------------------------------------*
       imm-ejc-pag-000.
      *              *-------------------------------------------------*
      *              * Se numero pagina zero non si esegue nessuna o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           if        p-pag                =    zero
                     go to imm-ejc-pag-999.
      *              *-------------------------------------------------*
      *              * Se open output in sospeso                       *
      *              *-------------------------------------------------*
           if        w-imm-flg-opo        not  = spaces
                     go to imm-ejc-pag-200.
      *                  *---------------------------------------------*
      *                  * Operazioni preliminari alla prima scrittura *
      *                  * per stampa immediata                        *
      *                  *---------------------------------------------*
           perform   imm-pre-out-000      thru imm-pre-out-999        .
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione : uscita            *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to imm-ejc-pag-999.
      *                  *---------------------------------------------*
      *                  * Flag di open output non piu' in sospeso     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-imm-flg-opo          .
       imm-ejc-pag-200.
      *              *-------------------------------------------------*
      *              * Eject pagina                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eject pagina su stampante mediante richiamo *
      *                  * del driver di stampa                        *
      *                  *---------------------------------------------*
           move      "EJ"                 to   p-sub                  .
           call      d-pth               using p                      .
      *                  *---------------------------------------------*
      *                  * Se errori in eject pagina su stampante      *
      *                  *---------------------------------------------*
           if        p-rsc                =    spaces
                     go to imm-ejc-pag-400.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore per stam- *
      *                      * pante                                   *
      *                      *-----------------------------------------*
           perform   imm-err-stp-000      thru imm-err-stp-999        .
       imm-ejc-pag-400.
      *              *-------------------------------------------------*
      *              * Trattamento per stampa in spool                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se stampa non in spool : uscita             *
      *                  *---------------------------------------------*
           if        w-imm-flg-dos        =    spaces
                     go to imm-ejc-pag-999.
      *                  *---------------------------------------------*
      *                  * Incremento numero di sub-file di spool      *
      *                  *---------------------------------------------*
           if        w-imm-sub-fds        <    999
                     add   1              to   w-imm-sub-fds
           else      move  1              to   w-imm-sub-fds          .
      *                  *---------------------------------------------*
      *                  * Esecuzione chiamata al sistema per lo spo-  *
      *                  * oler di sistema                             *
      *                  *---------------------------------------------*
           perform   cll-sys-spl-000      thru cll-sys-spl-999        .
       imm-ejc-pag-999.
           exit.

      *    *===========================================================*
      *    * Operazioni preliminari alla prima scrittura su stampante  *
      *    *-----------------------------------------------------------*
       imm-pre-out-000.
      *              *-------------------------------------------------*
      *              * Se stampa in diretta : ad apertura stampante    *
      *              * mediante richiamo del driver di stampa          *
      *              *-------------------------------------------------*
           if        w-imm-flg-dos        =    spaces
                     go to imm-pre-out-800.
       imm-pre-out-100.
      *              *-------------------------------------------------*
      *              * Se stampa in spool                              *
      *              *-------------------------------------------------*
       imm-pre-out-200.
      *                  *---------------------------------------------*
      *                  * Apertura [pfc]                              *
      *                  *---------------------------------------------*
           perform   opn-fil-pfc-000      thru opn-fil-pfc-999        .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con flag di errore       *
      *                  *---------------------------------------------*
           if        w-opn-fil-pfc        not  = spaces
                     move  "#"            to   p-err
                     go to imm-pre-out-999.
      *                  *---------------------------------------------*
      *                  * Prelevamento di un numero progressivo da    *
      *                  * [pfc]                                       *
      *                  *---------------------------------------------*
           perform   pre-num-pfc-000      thru pre-num-pfc-999        .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con flag di errore       *
      *                  *---------------------------------------------*
           if        w-pre-num-pfc        not  = spaces
                     move  "#"            to   p-err
                     go to imm-pre-out-999.
      *                  *---------------------------------------------*
      *                  * Memorizzazione numero progressivo spool-fi- *
      *                  * le gia' incrementato                        *
      *                  *---------------------------------------------*
           move      w-num-num-pfc        to   w-imm-prg-fds          .
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero sub-file di spool   *
      *                  * per Eject                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-imm-sub-fds          .
      *                  *---------------------------------------------*
      *                  * Determinazione del pathname di base relati- *
      *                  * vo agli spool-files per l'azienda           *
      *                  *---------------------------------------------*
           move      ".S"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-imm-pdb-spl          .
       imm-pre-out-300.
      *                  *---------------------------------------------*
      *                  * Se pathname di base relativo agli spool-fi- *
      *                  * les per l'azienda a spaces : lo si norma-   *
      *                  * lizza                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-imm-pdb-spl        not  = spaces
                     go to imm-pre-out-400.
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per [spl]         *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "spl"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-imm-pdb-spl          .
       imm-pre-out-400.
      *                  *---------------------------------------------*
      *                  * Composizione pathname per lo spool-file, e- *
      *                  * sclusa l'eventuale estensione relativa al   *
      *                  * sub-file di spool                           *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      w-imm-pdb-spl        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    "sf"       delimited by   size
                     w-imm-prg-fds
                                delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-imm-pat-spl          .
      *                  *---------------------------------------------*
      *                  * Pathname per lo spool-file in area per dri- *
      *                  * ver di stampa                               *
      *                  *---------------------------------------------*
           move      w-imm-pat-spl        to   p-sel-can-stp          .
       imm-pre-out-800.
      *              *-------------------------------------------------*
      *              * Open output stampante mediante richiamo del     *
      *              * driver di stampa, passando in p-sgn il flag     *
      *              * di stampa in diretta o in spool ed il segnale   *
      *              * di inibizione del salto pagina finale           *
      *              *-------------------------------------------------*
           move      "OO"                 to   p-sub                  .
           move      w-imm-flg-drv        to   p-alf                  .
           call      d-pth               using p                      .
       imm-pre-out-850.
      *              *-------------------------------------------------*
      *              * Se errori in open output stampante              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        p-rsc                =    spaces
                     go to imm-pre-out-900.
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore per stampante *
      *                  *---------------------------------------------*
           perform   imm-err-stp-000      thru imm-err-stp-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     imm-pre-out-999.
       imm-pre-out-900.
      *              *-------------------------------------------------*
      *              * Comunicazione al modulo di segreteria dei para- *
      *              * metri di tipo stampa relativi all'ultima stampa *
      *              * eseguita                                        *
      *              *-------------------------------------------------*
           move      "T>"                 to   s-ope                  .
           move      p-sel-tds-sel        to   s-alf                  .
           move      zero                 to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       imm-pre-out-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di errore per stampa immediata        *
      *    *-----------------------------------------------------------*
       imm-err-stp-000.
      *              *-------------------------------------------------*
      *              * File name                                       *
      *              *-------------------------------------------------*
           move      "stp "               to   p-nam                  .
      *              *-------------------------------------------------*
      *              * File pathname                                   *
      *              *-------------------------------------------------*
           move      p-sel-can-stp        to   p-pat                  .
      *              *-------------------------------------------------*
      *              * File status                                     *
      *              *-------------------------------------------------*
           move      p-rsc                to   p-sts                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo per emissione errore            *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpioer"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo richiamato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mpioer"                         .
      *              *-------------------------------------------------*
      *              * Return status code                              *
      *              *-------------------------------------------------*
           move      p-sts                to   p-rsc                  .
      *              *-------------------------------------------------*
      *              * Segnale di errore in corso                      *
      *              *-------------------------------------------------*
           move      "#"                  to   p-err                  .
       imm-err-stp-999.
           exit.

      *    *===========================================================*
      *    * Open output su disco                                      *
      *    *-----------------------------------------------------------*
       dsk-opn-out-000.
      *              *-------------------------------------------------*
      *              * Flag di open output in sospeso                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-dsk-flg-opo          .
      *                  *---------------------------------------------*
      *                  * Data e ora inizio creazione                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-dat                to   w-dsk-dat-icr          .
           move      o-tim                to   w-dsk-ora-icr          .
       dsk-opn-out-999.
           exit.

      *    *===========================================================*
      *    * Close output su disco                                     *
      *    *-----------------------------------------------------------*
       dsk-cls-out-000.
      *              *-------------------------------------------------*
      *              * Se numero pagina zero non si esegue nessuna o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           if        p-pag                =    zero
                     go to dsk-cls-out-999.
      *              *-------------------------------------------------*
      *              * Se l'ultima operazione eseguita e' stata eject  *
      *              * non si scarica la pagina in sospeso             *
      *              *-------------------------------------------------*
           if        p-vpm                =    "2"
                     go to dsk-cls-out-200.
      *              *-------------------------------------------------*
      *              * Scaricamento pagina in sospeso su disco         *
      *              *-------------------------------------------------*
           perform   dsk-wrt-pag-000      thru dsk-wrt-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione : uscita            *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to dsk-cls-out-999.
       dsk-cls-out-200.
      *              *-------------------------------------------------*
      *              * Close file [prf]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                            *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to dsk-cls-out-500.
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore su [prf]      *
      *                  *---------------------------------------------*
           perform   dsk-err-prf-000      thru dsk-err-prf-999        .
       dsk-cls-out-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento finale [pfc]                      *
      *              *-------------------------------------------------*
           perform   agg-fin-pfc-000      thru agg-fin-pfc-999        .
       dsk-cls-out-800.
      *              *-------------------------------------------------*
      *              * Emissione sperimentale file CSV                 *
      *              *                                                 *
      *              * ___ DA PERFEZIONARE per agg. pgep6901 ___       *
      *              *                                                 *
      *              * ___ SOLO 'nob' e SOLO 'gep690' ___              *
      *              *                                                 *
      *              * ___ SOLO 'su disco' o 'a video' ___             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * ATTUALMENTE INIBITO                         *
      *                  *---------------------------------------------*
           go to     dsk-cls-out-850.
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   w-imm-fil-azi          .
      *                  *---------------------------------------------*
      *                  * Test su azienda                             *
      *                  *---------------------------------------------*
           if        w-imm-fil-azi        not  = "nob "
                     go to dsk-cls-out-900.
      *                  *---------------------------------------------*
      *                  * Test su fase                                *
      *                  *---------------------------------------------*
           if        pfc-ide-fas          not  = "gep690"
                     go to dsk-cls-out-900.
       dsk-cls-out-820.
      *              *-------------------------------------------------*
      *              * Composizione del comando                        *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
      *
           string    "t_prf2csv"
                                delimited by   spaces
                     " "        delimited by   size
                     "pf"       delimited by   size
                     w-dsk-num-prg
                                delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       dsk-cls-out-850.
      *              *-------------------------------------------------*
      *              * Emissione sperimentale documento con EPS        *
      *              *                                                 *
      *              * ___ DA PERFEZIONARE per agg. ele/pbol300s ___   *
      *              *                                                 *
      *              * ___ SOLO 'ele' e SOLO 'bfo300' ___              *
      *              *                                                 *
      *              * ___ SOLO 'su disco' o 'a video' ___             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * ATTUALMENTE INIBITO                         *
      *                  *---------------------------------------------*
           go to     dsk-cls-out-900.
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   w-imm-fil-azi          .
      *                  *---------------------------------------------*
      *                  * Test su azienda                             *
      *                  *---------------------------------------------*
           if        w-imm-fil-azi        not  = "ele "
                     go to dsk-cls-out-900.
      *                  *---------------------------------------------*
      *                  * Test su fase                                *
      *                  *---------------------------------------------*
           if        pfc-ide-fas          not  = "bfo300"
                     go to dsk-cls-out-900.
       dsk-cls-out-870.
      *              *-------------------------------------------------*
      *              * Composizione del comando                        *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
      *
           string    "t_ele_stp_bfo"
                                delimited by   spaces
                     " "        delimited by   size
                     "pf"       delimited by   size
                     w-dsk-num-prg
                                delimited by   size
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Chiamata al sistema effettiva                   *
      *              *-------------------------------------------------*
           move      "CU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       dsk-cls-out-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsk-cls-out-999.
       dsk-cls-out-999.
           exit.

      *    *===========================================================*
      *    * Write Page su disco                                       *
      *    *-----------------------------------------------------------*
       dsk-wrt-pag-000.
      *              *-------------------------------------------------*
      *              * Se numero pagina zero non si esegue nessuna o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           if        p-pag                =    zero
                     go to dsk-wrt-pag-999.
      *              *-------------------------------------------------*
      *              * Se open output in sospeso                       *
      *              *-------------------------------------------------*
           if        w-dsk-flg-opo        not  = spaces
                     go to dsk-wrt-pag-100.
      *                  *---------------------------------------------*
      *                  * Operazioni preliminari alla prima scrittura *
      *                  * su disco                                    *
      *                  *---------------------------------------------*
           perform   dsk-pre-out-000      thru dsk-pre-out-999        .
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione : uscita            *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     go to dsk-wrt-pag-999.
      *                  *---------------------------------------------*
      *                  * Flag di open output non piu' in sospeso     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-dsk-flg-opo          .
       dsk-wrt-pag-100.
      *              *-------------------------------------------------*
      *              * Linea per la pagina                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione buffer di stampa            *
      *                  *---------------------------------------------*
           perform   nor-buf-stp-000      thru nor-buf-stp-999        .
      *                  *---------------------------------------------*
      *                  * Composizione record [prf] per l'intestazio- *
      *                  * ne della pagina                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Intero record a spaces                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prf-rec              .
      *                      *-----------------------------------------*
      *                      * Numero pagina                           *
      *                      *-----------------------------------------*
           move      p-pag                to   w-prf-num-pag          .
      *                      *-----------------------------------------*
      *                      * Numero linea                            *
      *                      *-----------------------------------------*
           move      zero                 to   w-prf-num-lin          .
      *                      *-----------------------------------------*
      *                      * Tipo record                             *
      *                      *-----------------------------------------*
           move      "P"                  to   w-prf-tip-rec          .
      *                  *---------------------------------------------*
      *                  * Scrittura record [prf] per l'intestazione   *
      *                  * della pagina su disco                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione record in area file        *
      *                      *-----------------------------------------*
           move      w-prf-rec            to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Operazione di i-o                       *
      *                      *-----------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Se errori in scrittura                  *
      *                      *-----------------------------------------*
           if        g-sts                =    e-not-err
                     go to dsk-wrt-pag-200.
      *                          *-------------------------------------*
      *                          * Emissione messaggio errore [prf] su *
      *                          * disco                               *
      *                          *-------------------------------------*
           perform   dsk-err-prf-000      thru dsk-err-prf-999        .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [prf]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Aggiornamento finale di [pfc]       *
      *                          *-------------------------------------*
           perform   agg-fin-pfc-000      thru agg-fin-pfc-999        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     dsk-wrt-pag-999.
       dsk-wrt-pag-200.
      *              *-------------------------------------------------*
      *              * Linee buffer stampa                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore linee pagina     *
      *                  *---------------------------------------------*
           move      zero                 to   w-dsk-ctr-lin          .
       dsk-wrt-pag-300.
      *                  *---------------------------------------------*
      *                  * Incremento contatore linee pagina           *
      *                  *---------------------------------------------*
           add       1                    to   w-dsk-ctr-lin          .
      *                  *---------------------------------------------*
      *                  * Test se fine linee  pagina                  *
      *                  *---------------------------------------------*
           if        w-dsk-ctr-lin        >    p-sel-efc-sel
                     go to dsk-wrt-pag-999.
      *                  *---------------------------------------------*
      *                  * Scrittura linea su disco                    *
      *                  *---------------------------------------------*
           perform   dsk-wrt-lin-000      thru dsk-wrt-lin-999        .
      *                  *---------------------------------------------*
      *                  * Se avvenuti errori in scrittura linea si e- *
      *                  * sce, altrimenti si ricicla per scrivere la  *
      *                  * linea successiva                            *
      *                  *---------------------------------------------*
           if        p-rsc                =    spaces
                     go to dsk-wrt-pag-300.
       dsk-wrt-pag-999.
           exit.

      *    *===========================================================*
      *    * Eject Page su disco                                       *
      *    *-----------------------------------------------------------*
       dsk-ejc-pag-000.
      *              *-------------------------------------------------*
      *              * Write page su disco                             *
      *              *-------------------------------------------------*
           perform   dsk-wrt-pag-000      thru dsk-wrt-pag-999        .
       dsk-ejc-pag-999.
           exit.

      *    *===========================================================*
      *    * Write Line su disco                                       *
      *    *-----------------------------------------------------------*
       dsk-wrt-lin-000.
      *              *-------------------------------------------------*
      *              * Se linea a spaces : nessuna operazione          *
      *              *-------------------------------------------------*
           if        p-buf-lin
                    (w-dsk-ctr-lin)       =    spaces
                     go to dsk-wrt-lin-999.
      *              *-------------------------------------------------*
      *              * Se non ci sono attributi si passa alla scrittu- *
      *              * ra dell'immagine della linea                    *
      *              *-------------------------------------------------*
           if        p-bat-lin
                    (w-dsk-ctr-lin)       =    spaces
                     go to dsk-wrt-lin-500.
      *              *-------------------------------------------------*
      *              * Trattamento attributi                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record [prf] per gli attributi *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Intero record a spaces                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prf-rec              .
      *                      *-----------------------------------------*
      *                      * Numero pagina                           *
      *                      *-----------------------------------------*
           move      p-pag                to   w-prf-num-pag          .
      *                      *-----------------------------------------*
      *                      * Numero linea                            *
      *                      *-----------------------------------------*
           move      w-dsk-ctr-lin        to   w-prf-num-lin          .
      *                      *-----------------------------------------*
      *                      * Tipo record                             *
      *                      *-----------------------------------------*
           move      "A"                  to   w-prf-tip-rec          .
      *                      *-----------------------------------------*
      *                      * Attributi della linea della pagina      *
      *                      *-----------------------------------------*
           move      p-bat-lin
                    (w-dsk-ctr-lin)       to   w-prf-dat-240          .
      *                  *---------------------------------------------*
      *                  * Scrittura record [prf] su disco per gli at- *
      *                  * tributi                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione record in area file        *
      *                      *-----------------------------------------*
           move      w-prf-rec            to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Operazione di i-o                       *
      *                      *-----------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Se errori in scrittura                  *
      *                      *-----------------------------------------*
           if        g-sts                =    e-not-err
                     go to dsk-wrt-lin-500.
      *                          *-------------------------------------*
      *                          * Emissione messaggio errore [prf] su *
      *                          * disco                               *
      *                          *-------------------------------------*
           perform   dsk-err-prf-000      thru dsk-err-prf-999        .
      *                          *-------------------------------------*
      *                          * Chiusura forzata [prf]              *
      *                          *-------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                          *-------------------------------------*
      *                          * Aggiornamento finale di [pfc]       *
      *                          *-------------------------------------*
           perform   agg-fin-pfc-000      thru agg-fin-pfc-999        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     dsk-wrt-lin-999.
       dsk-wrt-lin-500.
      *              *-------------------------------------------------*
      *              * Trattamento immagine                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record [prf] per l'immagine    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Intero record a spaces                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prf-rec              .
      *                      *-----------------------------------------*
      *                      * Numero pagina                           *
      *                      *-----------------------------------------*
           move      p-pag                to   w-prf-num-pag          .
      *                      *-----------------------------------------*
      *                      * Numero linea                            *
      *                      *-----------------------------------------*
           move      w-dsk-ctr-lin        to   w-prf-num-lin          .
      *                      *-----------------------------------------*
      *                      * Tipo record                             *
      *                      *-----------------------------------------*
           move      "I"                  to   w-prf-tip-rec          .
      *                      *-----------------------------------------*
      *                      * Immagine della linea della pagina       *
      *                      *-----------------------------------------*
           move      p-buf-lin
                    (w-dsk-ctr-lin)       to   w-prf-dat-240          .
      *                  *---------------------------------------------*
      *                  * Scrittura record [prf] su disco per l'imma- *
      *                  * gine della riga di stampa                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione record in area file        *
      *                      *-----------------------------------------*
           move      w-prf-rec            to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Operazione di i-o                       *
      *                      *-----------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione di i-o             *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to dsk-wrt-lin-999.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio errore [prf] su     *
      *                      * disco                                   *
      *                      *-----------------------------------------*
           perform   dsk-err-prf-000      thru dsk-err-prf-999        .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [prf]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Aggiornamento finale di [pfc]           *
      *                      *-----------------------------------------*
           perform   agg-fin-pfc-000      thru agg-fin-pfc-999        .
       dsk-wrt-lin-999.
           exit.

      *    *===========================================================*
      *    * Operazioni preliminari alla prima scrittura su disco      *
      *    *-----------------------------------------------------------*
       dsk-pre-out-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento iniziale di [pfc]                 *
      *              *-------------------------------------------------*
           perform   agg-ini-pfc-000      thru agg-ini-pfc-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        w-agg-ini-pfc        not  = spaces
                     go to dsk-pre-out-999.
       dsk-pre-out-200.
      *              *-------------------------------------------------*
      *              * Open output file [prf]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione del name, mentre il pathname   *
      *                  * e' gia' stato preparato in precedenza       *
      *                  *---------------------------------------------*
           move      "prf "               to   g-nam                  .
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
                     go to dsk-pre-out-400.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   dsk-err-prf-000      thru dsk-err-prf-999        .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [prf]                  *
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
           go to     dsk-pre-out-999.
       dsk-pre-out-400.
      *              *-------------------------------------------------*
      *              * Scrittura record 'header' file [prf] su disco   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      spaces               to   w-prf-rec              .
      *                      *-----------------------------------------*
      *                      * Composizione chiave                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero pagina                       *
      *                          *-------------------------------------*
           move      zero                 to   w-prf-num-pag          .
      *                          *-------------------------------------*
      *                          * Numero linea                        *
      *                          *-------------------------------------*
           move      zero                 to   w-prf-num-lin          .
      *                          *-------------------------------------*
      *                          * Tipo record                         *
      *                          *-------------------------------------*
           move      "H"                  to   w-prf-tip-rec          .
      *                      *-----------------------------------------*
      *                      * Composizione dati                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Area p-sel di selezione stampa      *
      *                          *-------------------------------------*
           move      p-sel                to   w-prf-dat              .
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione record in area file        *
      *                      *-----------------------------------------*
           move      w-prf-rec            to   g-rec                  .
      *                      *-----------------------------------------*
      *                      * Operazione di i-o                       *
      *                      *-----------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to dsk-pre-out-900.
      *                  *---------------------------------------------*
      *                  * Se errori in scrittura record               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   dsk-err-prf-000      thru dsk-err-prf-999        .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [prf]                  *
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
           go to     dsk-pre-out-999.
       dsk-pre-out-900.
      *              *-------------------------------------------------*
      *              * Comunicazione al modulo di segreteria dei para- *
      *              * metri di tipo stampa relativi all'ultima stampa *
      *              * eseguita                                        *
      *              *-------------------------------------------------*
           move      "T>"                 to   s-ope                  .
           move      p-sel-tds-sel        to   s-alf                  .
           move      w-num-num-pfc        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dsk-pre-out-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di errore su file [prf]               *
      *    *-----------------------------------------------------------*
       dsk-err-prf-000.
      *              *-------------------------------------------------*
      *              * File name                                       *
      *              *-------------------------------------------------*
           move      g-nam                to   p-nam                  .
      *              *-------------------------------------------------*
      *              * File pathname                                   *
      *              *-------------------------------------------------*
           move      g-pat                to   p-pat                  .
      *              *-------------------------------------------------*
      *              * File status                                     *
      *              *-------------------------------------------------*
           move      g-sts                to   p-sts                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo per emissione errore            *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpioer"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo richiamato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mpioer"                         .
      *              *-------------------------------------------------*
      *              * Return status code                              *
      *              *-------------------------------------------------*
           move      p-sts                to   p-rsc                  .
      *              *-------------------------------------------------*
      *              * Segnale di errore in corso                      *
      *              *-------------------------------------------------*
           move      "#"                  to   p-err                  .
       dsk-err-prf-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento iniziale di [pfc]                           *
      *    *-----------------------------------------------------------*
       agg-ini-pfc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-agg-ini-pfc          .
      *              *-------------------------------------------------*
      *              * Apertura [pfc]                                  *
      *              *-------------------------------------------------*
           perform   opn-fil-pfc-000      thru opn-fil-pfc-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        w-opn-fil-pfc        not  = spaces
                     move  "#"            to   w-agg-ini-pfc
                     go to agg-ini-pfc-999.
      *              *-------------------------------------------------*
      *              * Prelevamento di un numero progressivo da [pfc]  *
      *              *-------------------------------------------------*
           perform   pre-num-pfc-000      thru pre-num-pfc-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        w-pre-num-pfc        not  = spaces
                     move  "#"            to   w-agg-ini-pfc
                     go to agg-ini-pfc-999.
      *              *-------------------------------------------------*
      *              * Memorizzazione numero progressivo print-file    *
      *              * gia' incrementato                               *
      *              *-------------------------------------------------*
           move      w-num-num-pfc        to   w-dsk-num-prg          .
      *              *-------------------------------------------------*
      *              * Determinazione del pathname di base relativo ai *
      *              * print-files per l'azienda                       *
      *              *-------------------------------------------------*
           move      ".P"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-dsk-pdb-prf          .
       agg-ini-pfc-300.
      *              *-------------------------------------------------*
      *              * Se pathname di base relativo ai print-files per *
      *              * l'azienda a spaces : lo si normalizza           *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-dsk-pdb-prf        not  = spaces
                     go to agg-ini-pfc-400.
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per [prf]         *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "prf"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-dsk-pdb-prf          .
       agg-ini-pfc-400.
      *              *-------------------------------------------------*
      *              * Composizione pathname per file [prf] su disco   *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      w-dsk-pdb-prf        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    "pf"       delimited by   size
                     w-dsk-num-prg
                                delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   g-pat                  .
       agg-ini-pfc-500.
      *              *-------------------------------------------------*
      *              * Scrittura record di catalogo in [pfc]           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      spaces               to   pfc-rec                .
      *                      *-----------------------------------------*
      *                      * Composizione chiave                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Chiave numero 1                     *
      *                          *-------------------------------------*
           move      w-dsk-num-prg        to   pfc-num-prg-001        .
      *                          *-------------------------------------*
      *                          * Chiave numero 2                     *
      *                          *-------------------------------------*
           move      p-sel-cod-azi        to   pfc-cod-azi-002        .
           move      w-dsk-num-prg        to   pfc-num-prg-002        .
      *                          *-------------------------------------*
      *                          * Chiave numero 3                     *
      *                          *-------------------------------------*
           move      p-sel-cod-azi        to   pfc-cod-azi-003        .
           move      p-sel-cod-ute        to   pfc-cod-ute-003        .
           move      w-dsk-num-prg        to   pfc-num-prg-003        .
      *                          *-------------------------------------*
      *                          * Chiave numero 4                     *
      *                          *-------------------------------------*
           move      p-sel-cod-ute        to   pfc-cod-ute-004        .
           move      w-dsk-num-prg        to   pfc-num-prg-004        .
      *                          *-------------------------------------*
      *                          * Chiave numero 5                     *
      *                          *-------------------------------------*
           move      p-sel-cod-ute        to   pfc-cod-ute-005        .
           move      p-sel-cod-azi        to   pfc-cod-azi-005        .
           move      w-dsk-num-prg        to   pfc-num-prg-005        .
      *                      *-----------------------------------------*
      *                      * Composizione dati                       *
      *                      *-----------------------------------------*
           move      01                   to   pfc-tip-fil            .
           move      w-dsk-num-prg        to   pfc-num-prg            .
           move      p-sel-cod-azi        to   pfc-cod-azi            .
           move      p-sel-cod-ter        to   pfc-cod-ter            .
           move      p-sel-cod-ute        to   pfc-cod-ute            .
           move      p-sel-sis-app        to   pfc-ide-sap            .
           move      p-sel-are-ges        to   pfc-ide-arg            .
           move      p-sel-set-ges        to   pfc-ide-set            .
           move      p-sel-fas-ges        to   pfc-ide-fas            .
           move      w-dsk-dat-icr        to   pfc-dat-icr            .
           move      w-dsk-ora-icr        to   pfc-ora-icr            .
           move      zero                 to   pfc-dat-fcr            .
           move      zero                 to   pfc-ora-fcr            .
           move      zero                 to   pfc-nmr-pgn            .
           move      g-pat                to   pfc-pat-stp            .
           move      p-sel-stp-sel        to   pfc-cod-stp            .
           move      p-sel-mod-sel        to   pfc-cod-mod            .
           move      spaces               to   pfc-not-001            .
           move      spaces               to   pfc-not-002            .
           move      zero                 to   pfc-num-sef            .
           move      zero                 to   pfc-dat-ius            .
           move      zero                 to   pfc-ora-ius            .
           move      zero                 to   pfc-dat-fus            .
           move      zero                 to   pfc-ora-fus            .
           move      zero                 to   pfc-dat-ise            .
           move      zero                 to   pfc-ora-ise            .
      *
           if        pfc-ide-fas          =    "e-mail"
                     move  "#"            to   pfc-snx-msg
           else      move  spaces         to   pfc-snx-msg            .
      *
           move      spaces               to   pfc-snx-vis            .
           move      spaces               to   pfc-alx-fut            .
       agg-ini-pfc-600.
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione status                  *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                      *-----------------------------------------*
      *                      * Esecuzione put record                   *
      *                      *-----------------------------------------*
           write     pfc-rec invalid key
                             go to agg-ini-pfc-800.
      *                      *-----------------------------------------*
      *                      * Non invalid key                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se record locked si esegue una      *
      *                          * pausa di un secondo e si ritenta    *
      *                          * l'operazione                        *
      *                          *-------------------------------------*
           if        e-sts                =    e-use-err
                     perform wai-000      thru wai-999
                     go to   agg-ini-pfc-600.
      *                          *-------------------------------------*
      *                          * Se nessun errore : oltre            *
      *                          *-------------------------------------*
           if        e-sts                =    e-not-err
                     go to agg-ini-pfc-900.
       agg-ini-pfc-800.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-agg-ini-pfc          .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-ini-pfc-999.
       agg-ini-pfc-900.
      *              *-------------------------------------------------*
      *              * Chiusura [pfc]                                  *
      *              *-------------------------------------------------*
           perform   cls-fil-pfc-000      thru cls-fil-pfc-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        w-cls-fil-pfc        not  = spaces
                     move  "#"            to   w-agg-ini-pfc
                     go to agg-ini-pfc-999.
       agg-ini-pfc-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento finale di [pfc]                             *
      *    *-----------------------------------------------------------*
       agg-fin-pfc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-agg-fin-pfc          .
      *              *-------------------------------------------------*
      *              * Apertura [pfc]                                  *
      *              *-------------------------------------------------*
           perform   opn-fil-pfc-000      thru opn-fil-pfc-999        .
      *              *-------------------------------------------------*
      *              * Se errori : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        w-opn-fil-pfc        not  = spaces
                     move  "#"            to   w-agg-fin-pfc
                     go to agg-fin-pfc-999.
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
           move      w-dsk-num-prg        to   pfc-num-prg-001        .
           move      e-not-err            to   e-sts                  .
           read      pfc    invalid key
                            go to   agg-fin-pfc-400.
           if        e-sts                =    e-not-err
                     go to agg-fin-pfc-600.
      *              *-------------------------------------------------*
      *              * Se errori                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura forzata [pfc]                      *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     pfc                                              .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-agg-fin-pfc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     agg-fin-pfc-999.
       agg-fin-pfc-400.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Unlock [pfc]                                *
      *                  *---------------------------------------------*
           unlock    pfc    records                                   .
      *                  *---------------------------------------------*
      *                  * Chiusura forzata [pfc]                      *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     pfc                                              .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-agg-fin-pfc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     agg-fin-pfc-999.
       agg-fin-pfc-600.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data e ora fine creazione                   *
      *                  *---------------------------------------------*
           move      "DT"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-dat                to   pfc-dat-fcr            .
           move      o-tim                to   pfc-ora-fcr            .
      *                  *---------------------------------------------*
      *                  * Numero pagine                               *
      *                  *---------------------------------------------*
           move      p-pag                to   pfc-nmr-pgn            .
      *                  *---------------------------------------------*
      *                  * Riscrittura record                          *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   pfc-rec                                          .
           if        e-sts                =    e-not-err
                     go to agg-fin-pfc-800.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [pfc]                  *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     pfc                                              .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-agg-fin-pfc          .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-fin-pfc-800.
       agg-fin-pfc-800.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record senza errori          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [pfc]                            *
      *                      *-----------------------------------------*
           unlock    pfc    records                                   .
      *                      *-----------------------------------------*
      *                      * Chiusura [pfc]                          *
      *                      *-----------------------------------------*
           perform   cls-fil-pfc-000      thru cls-fil-pfc-999        .
      *                      *-----------------------------------------*
      *                      * Se errori : uscita con flag di errore   *
      *                      *-----------------------------------------*
           if        w-cls-fil-pfc        not  = spaces
                     move  "#"            to   w-agg-fin-pfc
                     go to agg-fin-pfc-999.
       agg-fin-pfc-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [pfc]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-pfc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-opn-fil-pfc          .
      *              *-------------------------------------------------*
      *              * Apertura file [pfc]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione error-code                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [pfc]             *
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
           move      "pfc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pfc-pat              .
      *                  *---------------------------------------------*
      *                  * Operazione di Open                          *
      *                  *---------------------------------------------*
           open      i-o   pfc                                        .
      *              *-------------------------------------------------*
      *              * Se nessun errore : uscita                       *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to opn-fil-pfc-999.
      *              *-------------------------------------------------*
      *              * Se errori                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-opn-fil-pfc          .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-pfc-999.
       opn-fil-pfc-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [pfc]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-pfc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cls-fil-pfc          .
      *              *-------------------------------------------------*
      *              * Operazione di chiusura                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     pfc                                              .
      *              *-------------------------------------------------*
      *              * Se nessun errore : uscita                       *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to cls-fil-pfc-999.
      *              *-------------------------------------------------*
      *              * Se errori                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cls-fil-pfc          .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cls-fil-pfc-999.
       cls-fil-pfc-999.
           exit.

      *    *===========================================================*
      *    * Prelevamento numero progressivo da [pfc]                  *
      *    *-----------------------------------------------------------*
       pre-num-pfc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-pre-num-pfc          .
      *              *-------------------------------------------------*
      *              * Lettura record con progressivo zero, se record  *
      *              * non trovato : a trattamento per record non esi- *
      *              * stente                                          *
      *              *-------------------------------------------------*
           move      zero                 to   pfc-num-prg-001        .
           move      e-not-err            to   e-sts                  .
           read      pfc    invalid key
                            go to   pre-num-pfc-200.
      *              *-------------------------------------------------*
      *              * Se nessun errore : a trattamento per record     *
      *              * gia' esistente                                  *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to pre-num-pfc-400.
      *              *-------------------------------------------------*
      *              * Se errori                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-pre-num-pfc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-num-pfc-999.
       pre-num-pfc-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record normalizzato            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare totale      *
      *                      *-----------------------------------------*
           move      spaces               to   pfc-rec                .
      *                      *-----------------------------------------*
      *                      * Composizione chiave                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Chiave numero 1                     *
      *                          *-------------------------------------*
           move      zero                 to   pfc-num-prg-001        .
      *                          *-------------------------------------*
      *                          * Chiave numero 2                     *
      *                          *-------------------------------------*
           move      spaces               to   pfc-cod-azi-002        .
           move      zero                 to   pfc-num-prg-002        .
      *                          *-------------------------------------*
      *                          * Chiave numero 3                     *
      *                          *-------------------------------------*
           move      spaces               to   pfc-cod-azi-003        .
           move      spaces               to   pfc-cod-ute-003        .
           move      zero                 to   pfc-num-prg-003        .
      *                          *-------------------------------------*
      *                          * Chiave numero 4                     *
      *                          *-------------------------------------*
           move      spaces               to   pfc-cod-ute-004        .
           move      zero                 to   pfc-num-prg-004        .
      *                          *-------------------------------------*
      *                          * Chiave numero 5                     *
      *                          *-------------------------------------*
           move      spaces               to   pfc-cod-ute-005        .
           move      spaces               to   pfc-cod-azi-005        .
           move      zero                 to   pfc-num-prg-005        .
      *                      *-----------------------------------------*
      *                      * Composizione dati                       *
      *                      *-----------------------------------------*
           move      zero                 to   pfc-tip-fil            .
           move      zero                 to   pfc-num-prg            .
           move      spaces               to   pfc-cod-azi            .
           move      spaces               to   pfc-cod-ter            .
           move      spaces               to   pfc-cod-ute            .
           move      spaces               to   pfc-ide-sap            .
           move      spaces               to   pfc-ide-arg            .
           move      spaces               to   pfc-ide-set            .
           move      spaces               to   pfc-ide-fas            .
           move      zero                 to   pfc-dat-icr            .
           move      zero                 to   pfc-ora-icr            .
           move      zero                 to   pfc-dat-fcr            .
           move      zero                 to   pfc-ora-fcr            .
           move      zero                 to   pfc-nmr-pgn            .
           move      spaces               to   pfc-pat-stp            .
           move      spaces               to   pfc-cod-stp            .
           move      spaces               to   pfc-cod-mod            .
           move      spaces               to   pfc-not-001            .
           move      spaces               to   pfc-not-002            .
           move      zero                 to   pfc-num-sef            .
           move      zero                 to   pfc-dat-ius            .
           move      zero                 to   pfc-ora-ius            .
           move      zero                 to   pfc-dat-fus            .
           move      zero                 to   pfc-ora-fus            .
           move      zero                 to   pfc-dat-ise            .
           move      zero                 to   pfc-ora-ise            .
           move      spaces               to   pfc-snx-msg            .
           move      spaces               to   pfc-snx-vis            .
           move      spaces               to   pfc-alx-fut            .
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato, se record    *
      *                  * gia' esistente : riciclo a rilettura del    *
      *                  * record                                      *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     pfc-rec invalid key
                             go to   pre-num-pfc-000.
      *                  *---------------------------------------------*
      *                  * Se nessun errore : riciclo a rilettura del  *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to pre-num-pfc-000.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-pre-num-pfc          .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-num-pfc-999.
       pre-num-pfc-400.
      *              *-------------------------------------------------*
      *              * Se record letto esistente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero progressivo            *
      *                  *---------------------------------------------*
           move      pfc-num-prg          to   w-num-num-pfc          .
           if        w-num-num-pfc        =    000000999999
                     move  zero           to   w-num-num-pfc          .
           add       1                    to   w-num-num-pfc          .
           move      w-num-num-pfc        to   pfc-num-prg            .
      *                  *---------------------------------------------*
      *                  * Riscrittura record aggiornato, se errori :  *
      *                  * riciclo a lettura record                    *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   pfc-rec invalid key
                             go to   pre-num-pfc-000.
      *                  *---------------------------------------------*
      *                  * Se nessun errore : ad unlock record         *
      *                  *---------------------------------------------*
           if        e-sts                =    e-not-err
                     go to pre-num-pfc-500.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   out-err-pfc-000      thru out-err-pfc-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-pre-num-pfc          .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-num-pfc-999.
       pre-num-pfc-500.
      *                  *---------------------------------------------*
      *                  * Unlock del record                           *
      *                  *---------------------------------------------*
           unlock    pfc    records                                   .
       pre-num-pfc-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di errore su file [pfc]               *
      *    *-----------------------------------------------------------*
       out-err-pfc-000.
      *              *-------------------------------------------------*
      *              * File name                                       *
      *              *-------------------------------------------------*
           move      f-pfc-nam            to   p-nam                  .
      *              *-------------------------------------------------*
      *              * File pathname                                   *
      *              *-------------------------------------------------*
           move      f-pfc-pat            to   p-pat                  .
      *              *-------------------------------------------------*
      *              * File status                                     *
      *              *-------------------------------------------------*
           move      f-pfc-sts            to   p-sts                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo per emissione errore            *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpioer"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo richiamato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mpioer"                         .
      *              *-------------------------------------------------*
      *              * Return status code                              *
      *              *-------------------------------------------------*
           move      p-sts                to   p-rsc                  .
      *              *-------------------------------------------------*
      *              * Segnale di errore in corso                      *
      *              *-------------------------------------------------*
           move      "#"                  to   p-err                  .
       out-err-pfc-999.
           exit.

      *    *===========================================================*
      *    * Lettura flag di inibizione salto pagina a fine stampa in  *
      *    * caso di stampa in spool, associato al codice stampante    *
      *    *-----------------------------------------------------------*
       ibz-fff-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del risultato a : No            *
      *              *-------------------------------------------------*
           move      "N"                  to   w-imm-ibz-fff          .
      *              *-------------------------------------------------*
      *              * Se tipo di stampa non immediata : uscita        *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        not  = "I" and
                     p-sel-tds-sel        not  = "F"
                     go to ibz-fff-stp-999.
      *              *-------------------------------------------------*
      *              * Se stampa immediata ma non in spool : uscita    *
      *              *-------------------------------------------------*
           if        w-imm-flg-dos        =    spaces
                     go to ibz-fff-stp-999.
      *              *-------------------------------------------------*
      *              * Se stampa immediata ed in spool, ma su stampan- *
      *              * te locale : uscita                              *
      *              *-------------------------------------------------*
           if        w-imm-spl-dau        =    "#"
                     go to ibz-fff-stp-999.
       ibz-fff-stp-200.
      *              *-------------------------------------------------*
      *              * Lettura effettiva                               *
      *              *-------------------------------------------------*
       ibz-fff-stp-300.
      *                  *---------------------------------------------*
      *                  * Open file [pss]                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione error-code              *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per [pss]         *
      *                      *-----------------------------------------*
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
           move      "pss"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pss-pat              .
      *                      *-----------------------------------------*
      *                      * Operazione di Open                      *
      *                      *-----------------------------------------*
           open      i-o   pss                                        .
      *                      *-----------------------------------------*
      *                      * Se errori : a chiusura ed uscita        *
      *                      *-----------------------------------------*
           if        e-sts                not  = e-not-err
                     go to ibz-fff-stp-800.
       ibz-fff-stp-400.
      *                  *---------------------------------------------*
      *                  * Lettura del record del codice stampante     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione chiave                     *
      *                      *-----------------------------------------*
           move      "stp "               to   pss-tre                .
           move      p-sel-stp-sel        to   pss-kre                .
      *                      *-----------------------------------------*
      *                      * Lettura, e se record non esistente si   *
      *                      * va' a chiusura ed uscita                *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    with no lock
                            invalid key
                            go to   ibz-fff-stp-800.
      *                      *-----------------------------------------*
      *                      * Test se errori di i-o : se record loc-  *
      *                      * ked si ricicla alla lettura fino a che  *
      *                      * il record non si sia sbloccato; se er-  *
      *                      * rore grave di i-o si va' a chiusura ed  *
      *                      * uscita                                  *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to ibz-fff-stp-450
           else if   e-sts                =    e-use-err
                     go to ibz-fff-stp-400
           else      go to ibz-fff-stp-800.
       ibz-fff-stp-450.
      *                      *-----------------------------------------*
      *                      * Record letto in area di comodo          *
      *                      *-----------------------------------------*
           move      pss-dat              to   w-stp                  .
       ibz-fff-stp-500.
      *                  *---------------------------------------------*
      *                  * Flag di inibizione in area di memorizzazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           move      w-stp-ibz-fff        to   w-imm-ibz-fff          .
           if        w-imm-ibz-fff        not  = "S"
                     move  "N"            to   w-imm-ibz-fff          .
       ibz-fff-stp-800.
      *                  *---------------------------------------------*
      *                  * Close file [pss]                            *
      *                  *---------------------------------------------*
           close      pss                                             .
       ibz-fff-stp-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione buffer di stampa                          *
      *    *-----------------------------------------------------------*
       nor-buf-stp-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero di caratteri oltre l'am-  *
      *              * piezza massima della linea di stampa, ed entro  *
      *              * il 240mo carattere                              *
      *              *-------------------------------------------------*
           move      240                  to   w-nor-buf-nce          .
           subtract  p-sel-als-sel        from w-nor-buf-nce          .
      *              *-------------------------------------------------*
      *              * Se pari a zero : uscita                         *
      *              *-------------------------------------------------*
           if        w-nor-buf-nce        =    zero
                     go to nor-buf-stp-999.
      *              *-------------------------------------------------*
      *              * Determinazione indice del primo carattere ecce- *
      *              * dente                                           *
      *              *-------------------------------------------------*
           move      p-sel-als-sel        to   w-nor-buf-ipc          .
           add       1                    to   w-nor-buf-ipc          .
      *              *-------------------------------------------------*
      *              * Preparazione literal per abblencamento          *
      *              *-------------------------------------------------*
           move      spaces               to   w-nor-spc-240          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice 01..96                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-nor-buf-inx          .
       nor-buf-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento indice 01..96                        *
      *              *-------------------------------------------------*
           add       1                    to   w-nor-buf-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-nor-buf-inx        >    96
                     go to nor-buf-stp-999.
      *              *-------------------------------------------------*
      *              * Linea in buffer di comodo                       *
      *              *-------------------------------------------------*
           move      p-buf-lin
                    (w-nor-buf-inx)       to   w-nor-lin-240          .
      *              *-------------------------------------------------*
      *              * Abblencamento porzione eccedente                *
      *              *-------------------------------------------------*
           move      w-nor-spc-240        to   w-nor-lin-240
                                              (w-nor-buf-ipc :)       .
      *              *-------------------------------------------------*
      *              * Linea in buffer di stampa                       *
      *              *-------------------------------------------------*
           move      w-nor-lin-240        to   p-buf-lin
                                              (w-nor-buf-inx)         .
      *              *-------------------------------------------------*
      *              * Riciclo a linea successiva                      *
      *              *-------------------------------------------------*
           go to     nor-buf-stp-100.
       nor-buf-stp-999.
           exit.

      *    *===========================================================*
      *    * Rilascio impegno stampante                                *
      *    *-----------------------------------------------------------*
       ril-imp-stp-000.
      *              *-------------------------------------------------*
      *              * Se stampante locale : uscita immediata          *
      *              *-------------------------------------------------*
           if        p-sel-stp-sel
                    (01 : 01)             =    "#"
                     go to ril-imp-stp-999.
      *              *-------------------------------------------------*
      *              * Se stampante in spool : uscita immediata        *
      *              *-------------------------------------------------*
           if        p-sel-can-stp
                    (01 : 05)             =    "spool"
                     go to ril-imp-stp-999.
      *              *-------------------------------------------------*
      *              * Se stampante tipo 'rcp' o 'ucp' o 'ghs' :       *
      *              * uscita immediata                                *
      *              *-------------------------------------------------*
           if        p-sel-can-stp
                    (01 : 03)             =    "rcp" or
                     p-sel-can-stp
                    (01 : 03)             =    "ucp" or
                     p-sel-can-stp
                    (01 : 03)             =    "ghs" or
                     p-sel-can-stp
                    (01 : 03)             =    "fax" or
                     p-sel-can-stp
                    (01 : 03)             =    "eml"
                     go to ril-imp-stp-999.
      *              *-------------------------------------------------*
      *              * Se tipo di stampa non immediata non e' necessa- *
      *              * rio alcun rilascio in quanto l'impegno non era  *
      *              * mai stato eseguito                              *
      *              *-------------------------------------------------*
           if        p-sel-tds-sel        not  = "I" and
                     p-sel-tds-sel        not  = "F"
                     go to ril-imp-stp-999.
      *              *-------------------------------------------------*
      *              * Se rilascio stampante gia' eseguito : uscita    *
      *              *-------------------------------------------------*
           if        w-ril-flg-ril        not  = spaces
                     go to ril-imp-stp-999.
      *              *-------------------------------------------------*
      *              * Flag di rilascio stampante eseguito in On       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ril-flg-ril          .
       ril-imp-stp-200.
      *              *-------------------------------------------------*
      *              * Rilascio stampante effettivo                    *
      *              *-------------------------------------------------*
       ril-imp-stp-300.
      *                  *---------------------------------------------*
      *                  * Open file [pss]                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione error-code              *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per [pss]         *
      *                      *-----------------------------------------*
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
           move      "pss"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pss-pat              .
      *                      *-----------------------------------------*
      *                      * Operazione di Open                      *
      *                      *-----------------------------------------*
           open      i-o   pss                                        .
      *                      *-----------------------------------------*
      *                      * Se errori : a chiusura ed uscita        *
      *                      *-----------------------------------------*
           if        e-sts                not  = e-not-err
                     go to ril-imp-stp-800.
       ril-imp-stp-400.
      *                  *---------------------------------------------*
      *                  * Lettura del record di impegno della stam-   *
      *                  * pante con lock                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione chiave                     *
      *                      *-----------------------------------------*
           move      "imp "               to   pss-tre                .
           move      p-sel-can-stp        to   pss-kre                .
      *                      *-----------------------------------------*
      *                      * Lettura, e se record non esistente si   *
      *                      * va' a chiusura ed uscita                *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      pss    invalid key
                            go to   ril-imp-stp-800.
      *                      *-----------------------------------------*
      *                      * Test se errori di i-o : se record loc-  *
      *                      * ked si ricicla alla lettura fino a che  *
      *                      * il record non si sia sbloccato; se er-  *
      *                      * rore grave di i-o si va' a chiusura ed  *
      *                      * uscita                                  *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to ril-imp-stp-425
           else if   e-sts                =    e-use-err
                     go to ril-imp-stp-400
           else      go to ril-imp-stp-800.
       ril-imp-stp-425.
      *                      *-----------------------------------------*
      *                      * Record letto in area di comodo          *
      *                      *-----------------------------------------*
           move      pss-dat              to   w-imp                  .
       ril-imp-stp-500.
      *                  *---------------------------------------------*
      *                  * Aggiornamento del record con status di non  *
      *                  * impegnato                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-imp                  .
           move      p-sel-can-stp        to   w-imp-can-stp          .
           move      zero                 to   w-imp-sts-imp          .
           move      spaces               to   w-imp-azi-imp          .
           move      spaces               to   w-imp-ter-imp          .
           move      spaces               to   w-imp-ute-imp          .
           move      spaces               to   w-imp-sap-imp          .
           move      spaces               to   w-imp-arg-imp          .
           move      spaces               to   w-imp-set-imp          .
           move      spaces               to   w-imp-fas-imp          .
           move      zero                 to   w-imp-sdt-imp          .
       ril-imp-stp-600.
      *                  *---------------------------------------------*
      *                  * Riscrittura del record aggiornato           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione chiave                     *
      *                      *-----------------------------------------*
           move      "imp "               to   pss-tre                .
           move      p-sel-can-stp        to   pss-kre                .
      *                      *-----------------------------------------*
      *                      * Preparazione dati                       *
      *                      *-----------------------------------------*
           move      w-imp                to   pss-dat                .
      *                      *-----------------------------------------*
      *                      * Riscrittura, e se record non esistente  *
      *                      * va' ad unlock, chiusura, ed uscita      *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   pss-rec invalid key
                             go to   ril-imp-stp-800.
      *                      *-----------------------------------------*
      *                      * Test se errori di i-o : se record loc-  *
      *                      * ked si ricicla alla riscrittura fino a  *
      *                      * che il record non si sia sbloccato; se  *
      *                      * errore grave di i-o si va' a chiusura   *
      *                      * ed uscita                               *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to ril-imp-stp-700
           else if   e-sts                =    e-use-err
                     go to ril-imp-stp-600
           else      go to ril-imp-stp-800.
       ril-imp-stp-700.
      *                  *---------------------------------------------*
      *                  * Unlock di [pss]                             *
      *                  *---------------------------------------------*
           unlock    pss    records                                   .
       ril-imp-stp-800.
      *                  *---------------------------------------------*
      *                  * Close file [pss]                            *
      *                  *---------------------------------------------*
           close      pss                                             .
       ril-imp-stp-999.
           exit.

      *================================================================*
      *    Convert                                                     *
      *----------------------------------------------------------------*
       cvt-000.
      *              *-------------------------------------------------*
      *              * Conversione da stringa a numero                 *
      *              *-------------------------------------------------*
           move      p-alf                to   w-fld                  .
           move      zero                 to   w-ctr                  .
           move      zero                 to   w-int                  .
           move      zero                 to   w-dec                  .
           move      zero                 to   w-dim                  .
           move      spaces               to   w-sgn                  .
       cvt-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    p-car
                     go to cvt-300.
           if        w-chr (w-ctr)        =    ","
                     go to cvt-200.
           if        w-chr (w-ctr)        =    "-"
                     move  "#"            to   w-sgn
                     go to cvt-100.
           if        w-chr (w-ctr)        <    "0" or
                     w-chr (w-ctr)        >    "9"
                     go to cvt-100.
           multiply  10                   by   w-int                  .
           add       w-chn (w-ctr)        to   w-int                  .
           go to     cvt-100.
       cvt-200.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    p-car
                     go to cvt-300.
           if        w-chr (w-ctr)        <    "0" or
                     w-chr (w-ctr)        >    "9"
                     go to cvt-200.
           add       1                    to   w-dim                  .
           move      w-chr (w-ctr)        to   w-dcc (w-dim)          .
           if        w-dim                =    5
                     go to cvt-300
           else      go to cvt-200.
       cvt-300.
            move     w-dec                to   p-num                  .
            divide   100000               into p-num                  .
            add      w-int                to   p-num                  .
            if       w-sgn                not  = spaces
                     multiply -1          by   p-num                  .
       cvt-999.
           exit.

      *    *===========================================================*
      *    * Wait di un secondo                                        *
      *    *-----------------------------------------------------------*
       wai-000.
      *              *-------------------------------------------------*
      *              * Richiamo del modulo                             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
       wai-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

